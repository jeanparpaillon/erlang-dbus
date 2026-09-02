-module(prop_dbus_marshaller).
-moduledoc """
Encoding laws for `dbus_marshaller'.

The laws, in the order they are stated below:

1. decoding a signature inverts encoding it, decoding accepts nothing the
   encoder could not have produced, and decoding an arbitrary binary answers
   rather than raises;
2. decoding a value list inverts encoding it, modulo
   `dbus_marshaller_gen:canon/2';
3. the position an encoder returns accounts for every byte it emitted;
4. every fixed-width value starts on its alignment boundary;
5. encoder and decoder agree on the position they end at, at any starting
   offset -- offsets the public API cannot reach, since it always starts at 0;
6. the permissive input clauses of `marshal/3' produce the same bytes as the
   canonical input they accept in place of;
7. a value marshalled as a bare variant survives the round trip even though its
   type does not;
8. the big-endian decode path agrees with the little-endian one.

3 and 4 are the reason this is not just a round-trip suite: a padding rule that
is wrong symmetrically on both sides satisfies law 2 and fails those two.

`?SLACK' and the trailing `uint64' guard element both work around BUG-4 in
`docs/marshaller-property-triage.md'.
""".

-include_lib("proper/include/proper.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([
    prop_signature_roundtrip/0,
    prop_signature_never_crashes/0,
    prop_signature_accepts_only_valid/0,
    prop_signature_rejects_invalid/0,
    prop_array_nesting_depth/0,
    prop_value_roundtrip/0,
    prop_position_accounts_for_bytes/0,
    prop_alignment/0,
    prop_offset_agreement/0,
    prop_lax_input_equivalent/0,
    prop_atom_string_input/0,
    prop_legacy_dict_input/0,
    prop_bare_variant_value_roundtrip/0,
    prop_endian_agreement/0
]).

%% BUG-4: `unmarshal/4' reports `more' for a struct whenever fewer than 8 bytes
%% remain in the buffer, whether or not the struct is complete. Eight trailing
%% bytes keep every struct clear of the end of the buffer.
-define(SLACK, <<0:64>>).

%% The caps the specification's "Valid Signatures" section puts on the
%% signature language, and `dbus_marshaller' with it.
-define(MAX_LENGTH, 255).
-define(MAX_ARRAY_DEPTH, 32).
-define(MAX_STRUCT_DEPTH, 32).

%%%
%%% 1. Signatures
%%%

prop_signature_roundtrip() ->
    ?FORALL(
        Sig,
        dbus_marshaller_gen:signature(),
        begin
            Bin = iolist_to_binary(dbus_marshaller:marshal_signature(Sig)),
            {ok, Sig} =:= dbus_marshaller:unmarshal_signature(Bin)
        end
    ).

%% `unmarshal_signature/1' is public and is also reached from the wire through
%% `unmarshal_body/4', where the argument is a header field written by the
%% peer. Only `unmarshal_data/1' has an outer try, so the exported function has
%% to be total on its own.
prop_signature_never_crashes() ->
    ?FORALL(
        Bin,
        signature_fuzz(),
        case catch dbus_marshaller:unmarshal_signature(Bin) of
            more -> true;
            {ok, Sig} -> is_list(Sig);
            {error, _} -> true;
            _Other -> false
        end
    ).

%% The negative direction of law 1, and the reason it is stated over fuzz
%% rather than over `signature()': feeding back what `marshal_signature/1'
%% produced only ever states that the valid language is accepted. Re-encoding
%% what was accepted is what excludes the `r' and `e' type codes, an unbalanced
%% bracket and a dict entry outside an array, without listing them here.
prop_signature_accepts_only_valid() ->
    ?FORALL(
        Bin,
        signature_fuzz(),
        case dbus_marshaller:unmarshal_signature(Bin) of
            {ok, Sig} -> (catch iolist_to_binary(dbus_marshaller:marshal_signature(Sig))) =:= Bin;
            {error, _} -> true;
            more -> true
        end
    ).

%% The shapes fuzzing is unlikely to reach, one per rule of the specification's
%% "Valid Signatures" section. `more' is not an acceptable answer for any of
%% them: none is a valid signature cut short.
prop_signature_rejects_invalid() ->
    ?FORALL(
        Bin,
        invalid_signature(),
        case dbus_marshaller:unmarshal_signature(Bin) of
            {error, {bad_signature, _}} -> true;
            _Other -> false
        end
    ).

invalid_signature() ->
    union([
        %% A byte that is no type code at all, and the two type codes reserved
        %% for STRUCT and DICT_ENTRY outside a signature.
        <<255>>,
        <<0>>,
        <<"r">>,
        <<"e">>,
        <<"ir">>,
        <<"m">>,
        %% Brackets with nothing to close, and a struct with no fields.
        <<")">>,
        <<"}">>,
        <<"s)s">>,
        <<"(s))">>,
        <<"()">>,
        %% A dict entry outside an array, with a container key, or with other
        %% than two fields.
        <<"{sv}">>,
        <<"a{vs}">>,
        <<"a{av}">>,
        <<"a{s}">>,
        <<"a{sss}">>,
        %% Past the depth caps: 33 array codes, then 33 open parentheses.
        ?LET(
            N,
            integer(?MAX_ARRAY_DEPTH + 1, ?MAX_ARRAY_DEPTH + 8),
            iolist_to_binary(lists:duplicate(N, $a) ++ "v")
        ),
        ?LET(
            N,
            integer(?MAX_STRUCT_DEPTH + 1, ?MAX_STRUCT_DEPTH + 8),
            iolist_to_binary([lists:duplicate(N, $(), "v", lists:duplicate(N, $))])
        ),
        %% Past the 255-byte cap, which is what a single length byte can hold.
        ?LET(
            N,
            integer(?MAX_LENGTH + 1, ?MAX_LENGTH + 8),
            iolist_to_binary(lists:duplicate(N, $y))
        )
    ]).

%% An array element is one single complete type, at every depth the caps allow.
%% `aav' is the instance of this that BUG-6 in
%% `docs/marshaller-property-triage.md' was about: the array element type used
%% to be the whole of the rest of the signature, so `aav' decoded to
%% `{array, [{array, variant}]}' -- an element type that is a list. Anything
%% deeper than the cap is `invalid_signature/0' above.
prop_array_nesting_depth() ->
    ?FORALL(
        N,
        integer(1, ?MAX_ARRAY_DEPTH),
        begin
            Bin = iolist_to_binary(lists:duplicate(N, $a) ++ "v"),
            Type = nested_array(N, variant),
            dbus_marshaller:unmarshal_signature(Bin) =:= {ok, [Type]} andalso
                iolist_to_binary(dbus_marshaller:marshal_signature([Type])) =:= Bin
        end
    ).

nested_array(0, Type) -> Type;
nested_array(N, Type) -> {array, nested_array(N - 1, Type)}.

signature_fuzz() ->
    union([binary(), corrupted_signature()]).

corrupted_signature() ->
    ?LET(
        Sig,
        dbus_marshaller_gen:signature(),
        ?LET(
            Bin,
            exactly(iolist_to_binary(dbus_marshaller:marshal_signature(Sig))),
            dbus_marshaller_gen:corrupted(Bin)
        )
    ).

%%%
%%% 2. Values
%%%

prop_value_roundtrip() ->
    ?FORALL(
        {Sig, Vs},
        dbus_marshaller_gen:guarded_sig_and_values(),
        aggregate(
            with_title("type shapes"),
            [shape(T) || T <- Sig],
            begin
                {Io, _Pos} = dbus_marshaller:marshal_list(Sig, Vs),
                Bin = iolist_to_binary(Io),
                Expected = dbus_marshaller_gen:canon_list(Sig, Vs),
                case dbus_marshaller:unmarshal_tuple(Sig, Bin, $l) of
                    {ok, Decoded, <<>>, _} -> tuple_to_list(Decoded) =:= Expected;
                    _Other -> false
                end
            end
        )
    ).

shape({array, _}) -> array;
shape({struct, _}) -> struct;
shape({dict, _, _}) -> dict;
shape(variant) -> variant;
shape(Basic) -> Basic.

%%%
%%% 3. Positions account for bytes
%%%

prop_position_accounts_for_bytes() ->
    ?FORALL(
        {Type, Value, Pos},
        dbus_marshaller_gen:typed_value(),
        begin
            {Io, Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
            Pos1 - Pos =:= iolist_size(Io)
        end
    ).

%%%
%%% 4. Alignment
%%%

prop_alignment() ->
    ?FORALL(
        {Type, Value, Pos},
        dbus_marshaller_gen:fixed_width_typed_value(),
        begin
            {_Io, Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
            Pad = Pos1 - Pos - dbus_marshaller_gen:fixed_width(Type),
            Pad >= 0 andalso (Pos + Pad) rem dbus_marshaller:padding(Type) =:= 0
        end
    ).

%%%
%%% 5. Encoder and decoder agree on the end position
%%%

prop_offset_agreement() ->
    ?FORALL(
        {Type, Value, Pos},
        dbus_marshaller_gen:typed_value(),
        begin
            {Io, Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
            Bin = <<(iolist_to_binary(Io))/binary, ?SLACK/binary>>,
            Expected = dbus_marshaller_gen:canon(Type, Value),
            case dbus_marshaller:unmarshal(Type, Bin, Pos, $l) of
                {ok, Decoded, ?SLACK, Pos2} -> Pos2 =:= Pos1 andalso Decoded =:= Expected;
                _Other -> false
            end
        end
    ).

%%%
%%% 6. Permissive inputs encode like the canonical ones
%%%

prop_lax_input_equivalent() ->
    ?FORALL(
        {Type, Value, Pos},
        dbus_marshaller_gen:typed_value(),
        ?FORALL(
            Lax,
            dbus_marshaller_gen:lax(Type, Value),
            begin
                {Io, Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
                {LaxIo, LaxPos1} = dbus_marshaller:marshal(Type, Lax, Pos),
                {iolist_to_binary(LaxIo), LaxPos1} =:= {iolist_to_binary(Io), Pos1}
            end
        )
    ).

%% An atom is accepted where a string is expected. The pool is fixed rather than
%% generated: one fresh atom per test would grow the atom table for the lifetime
%% of the node.
prop_atom_string_input() ->
    ?FORALL(
        {Atom, Pos},
        {union(['', a, abc, 'a.b.C', 'org.freedesktop.DBus', 'Ping']), integer(0, 64)},
        begin
            {Io, Pos1} = dbus_marshaller:marshal(string, Atom, Pos),
            {BinIo, BinPos1} = dbus_marshaller:marshal(string, atom_to_binary(Atom, utf8), Pos),
            {iolist_to_binary(Io), Pos1} =:= {iolist_to_binary(BinIo), BinPos1}
        end
    ).

%% A legacy `dict:dict()' is accepted where a map is expected. Its entries come
%% out in `dict:to_list/1' order rather than `maps:to_list/1' order, so the two
%% are compared after decoding rather than as bytes.
prop_legacy_dict_input() ->
    ?FORALL(
        {Type, Map, Pos},
        dbus_marshaller_gen:dict_typed_value(),
        begin
            Legacy = dict:from_list(maps:to_list(Map)),
            {Io, Pos1} = dbus_marshaller:marshal(Type, Legacy, Pos),
            Bin = <<(iolist_to_binary(Io))/binary, ?SLACK/binary>>,
            Expected = dbus_marshaller_gen:canon(Type, Map),
            case dbus_marshaller:unmarshal(Type, Bin, Pos, $l) of
                {ok, Decoded, ?SLACK, Pos2} -> Pos2 =:= Pos1 andalso Decoded =:= Expected;
                _Other -> false
            end
        end
    ).

%%%
%%% 7. Bare variants
%%%

prop_bare_variant_value_roundtrip() ->
    ?FORALL(
        Value,
        dbus_marshaller_gen:bare_variant_value(),
        begin
            {Io, _Pos} = dbus_marshaller:marshal(variant, Value, 0),
            Bin = <<(iolist_to_binary(Io))/binary, ?SLACK/binary>>,
            %% `infer_type/1' picks the encoded type from the value, and a list
            %% is inferred to be a byte array rather than a string.
            Expected =
                case is_binary(Value) of
                    true -> binary_to_list(Value);
                    false -> Value
                end,
            case dbus_marshaller:unmarshal(variant, Bin, 0, $l) of
                {ok, Decoded, ?SLACK, _} -> Decoded =:= Expected;
                _Other -> false
            end
        end
    ).

%%%
%%% 8. Endianness
%%%

prop_endian_agreement() ->
    ?FORALL(
        {Type, Value, _Pos},
        dbus_marshaller_gen:fixed_width_typed_value(),
        begin
            %% At position 0 no type is padded, so the encoding is exactly the
            %% value and reversing it byte-wise is its big-endian form.
            {Io, _Pos1} = dbus_marshaller:marshal(Type, Value, 0),
            Little = iolist_to_binary(Io),
            Big = list_to_binary(lists:reverse(binary_to_list(Little))),
            dbus_marshaller:unmarshal(Type, Big, 0, $B) =:=
                dbus_marshaller:unmarshal(Type, Little, 0, $l)
        end
    ).
