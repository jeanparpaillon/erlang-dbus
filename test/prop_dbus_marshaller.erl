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
4. a value of any type starts on the alignment boundary the specification gives
   that type;
5. an array's length word counts its elements and none of the padding around
   them;
6. the specification's own worked examples encode to, and decode from, exactly
   the bytes it prints;
7. encoder and decoder agree on the position they end at, at any starting
   offset -- offsets the public API cannot reach, since it always starts at 0;
8. the permissive input clauses of `marshal/3' produce the same bytes as the
   canonical input they accept in place of;
9. a value marshalled as a bare variant survives the round trip even though its
   type does not;
10. the big-endian decode path agrees with the little-endian one.

3 to 6 are the reason this is not just a round-trip suite: a padding rule that
is wrong symmetrically on both sides satisfies law 2 and fails them. Laws 4 and
5 are stated against `dbus_marshaller_gen:alignment/1', a table transcribed from
the specification, rather than against `dbus_marshaller:padding/1', which is
under test; law 6 is stated against bytes the D-Bus authors wrote down and is
the only oracle here that owes nothing to the implementation.

What the two of them cannot see is an entry of `padding/1' swapped for another
value no greater than 4. `padding/1' is consulted for two things only: the
alignment of an array's own length word, which law 4 checks at every position,
and where that array's elements begin -- and that always follows the 4-aligned
length word, so 1, 2 and 4 cannot be told apart there. Every entry raised to 8,
and every 8 lowered, is caught.

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
    prop_array_length_excludes_padding/0,
    prop_spec_figures/0,
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

%% The ten types the specification gives a fixed width, in the order of its
%% "Summary of D-Bus marshalling" table.
-define(FIXED_WIDTH_TYPES, [
    byte, boolean, int16, uint16, int32, uint32, int64, uint64, double, unix_fd
]).

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
            Bin = dbus_marshaller:marshal_signature(Sig),
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
            {ok, Sig} -> (catch dbus_marshaller:marshal_signature(Sig)) =:= Bin;
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
                dbus_marshaller:marshal_signature([Type]) =:= Bin
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
            exactly(dbus_marshaller:marshal_signature(Sig)),
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

%% `alignment_probe/0' guarantees the first byte of the value proper is not
%% zero, which is what lets the padding be read straight off the output as its
%% leading zero bytes -- no position arithmetic against the encoder, and so no
%% type is out of reach the way the variable-width ones were when the law was
%% stated as a position delta.
prop_alignment() ->
    ?FORALL(
        {Type, Value, Pos},
        dbus_marshaller_gen:alignment_probe(),
        begin
            {Io, _Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
            Bin = iolist_to_binary(Io),
            Pad = dbus_marshaller_gen:align(Type, Pos) - Pos,
            leading_zeros(Bin) =:= Pad andalso content_size_ok(Type, byte_size(Bin) - Pad)
        end
    ).

leading_zeros(Bin) -> leading_zeros(Bin, 0).

leading_zeros(<<0, Rest/binary>>, N) -> leading_zeros(Rest, N + 1);
leading_zeros(_Bin, N) -> N.

%% For the nine fixed-width types the size of the value proper is known from the
%% specification too, so the law says how many bytes follow the padding as well
%% as where they start.
content_size_ok(Type, Size) ->
    case lists:member(Type, ?FIXED_WIDTH_TYPES) of
        true -> Size =:= dbus_marshaller_gen:fixed_width(Type);
        false -> true
    end.

%%%
%%% 5. An array's length word
%%%

%% "n does not include the padding after the length, or any padding after the
%% last element" -- Marshalling containers. Both ends of that are checked here:
%% where the elements begin is computed from the transcribed alignment table,
%% and n is read off the wire, so an element type padded to the wrong boundary
%% shows up as an n that does not reach the end of the encoding.
prop_array_length_excludes_padding() ->
    ?FORALL(
        {{array, SubType} = Type, Value, Pos},
        dbus_marshaller_gen:array_typed_value(),
        aggregate(
            with_title("array element shapes"),
            [shape(SubType)],
            begin
                {Io, Pos1} = dbus_marshaller:marshal(Type, Value, Pos),
                Bin = iolist_to_binary(Io),
                LenPos = dbus_marshaller_gen:align(uint32, Pos),
                First = dbus_marshaller_gen:align(SubType, LenPos + 4),
                Offset = LenPos - Pos,
                <<_:Offset/binary, N:32/little-unsigned, _/binary>> = Bin,
                N =:= Pos1 - First andalso byte_size(Bin) =:= First - Pos + N
            end
        )
    ).

%%%
%%% 6. The specification's worked examples
%%%

%% A fixed set of vectors, stated as a property rather than as test cases so
%% that they run in the same suite, under the same command, as everything else
%% here. `-n' well above the number of figures is what makes each one certain to
%% be drawn.
prop_spec_figures() ->
    ?FORALL(
        {Sig, Values, Endian, Bytes},
        dbus_marshaller_gen:spec_figure(),
        begin
            Expected = list_to_tuple(dbus_marshaller_gen:canon_list(Sig, Values)),
            decodes_as(Sig, Bytes, Endian, Expected) andalso encodes_as(Sig, Values, Endian, Bytes)
        end
    ).

decodes_as(Sig, Bytes, Endian, Expected) ->
    dbus_marshaller:unmarshal_tuple(Sig, Bytes, Endian) =:=
        {ok, Expected, <<>>, byte_size(Bytes)}.

%% Only the little-endian transcription of a figure can be checked in the
%% encoding direction: `marshal_list/2' has no byte order argument and always
%% writes `$l'.
encodes_as(_Sig, _Values, $B, _Bytes) ->
    true;
encodes_as(Sig, Values, $l, Bytes) ->
    {Io, Pos} = dbus_marshaller:marshal_list(Sig, Values),
    {iolist_to_binary(Io), Pos} =:= {Bytes, byte_size(Bytes)}.

%%%
%%% 7. Encoder and decoder agree on the end position
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
%%% 8. Permissive inputs encode like the canonical ones
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
%%% 9. Bare variants
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
%%% 10. Endianness
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
