-module(dbus_marshaller_gen).
-moduledoc """
PropEr generators for the D-Bus type language.

The generators are the substance of the property suite; the properties
themselves are one-liners on top of them. Four things live here:

- generators for `dbus_type()`, `dbus_signature()` and, dependently, for a value
  of a given type;
- `canon/2`, which maps a marshalling *input* to the value unmarshalling gives
  back. Encode and decode are not inverses on the nose -- decode is onto a
  canonical subset -- so a round-trip property is not statable without it;
- `lax/2`, which turns a canonical value into an equivalent non-canonical one,
  so the permissive input clauses of `marshal/3` get exercised too;
- `corrupted/1`, which damages valid bytes, since the robustness properties get
  nowhere feeding a decoder random ones.

Beside them are three oracles transcribed from the specification rather than
derived from `dbus_marshaller`: `fixed_width/1`, `alignment/1` and
`spec_figure/0`. The duplication is the point -- a law checked against the
implementation's own opinion of where a boundary is holds however wrong that
opinion is.

Several generators are deliberately narrower than the D-Bus specification. Every
such narrowing carries a `BUG-n' marker naming its entry in
`docs/marshaller-property-triage.md'; without them the properties fail on
existing defects that this suite is not chartered to fix.
""".

-include_lib("proper/include/proper.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([
    basic_type/0,
    type/0,
    signature/0,
    value/1,
    values/1,
    sig_and_values/0,
    guarded_sig_and_values/0,
    typed_value/0,
    dict_typed_value/0,
    fixed_width_typed_value/0,
    array_typed_value/0,
    alignment_probe/0,
    bare_variant_value/0,
    lax/2,
    canon/2,
    canon_list/2,
    empty_message/0,
    message/0,
    messages/0,
    body_size/2,
    fixed_width/1,
    alignment/1,
    align/2,
    spec_figure/0,
    corrupted/1
]).

%%%
%%% Types and signatures
%%%

-doc "The D-Bus basic types, i.e. everything that may be a dict key.".
basic_type() ->
    union([
        byte,
        boolean,
        int16,
        uint16,
        int32,
        uint32,
        int64,
        uint64,
        double,
        string,
        object_path,
        signature
    ]).

-doc """
A single complete type.

`empty' is in `dbus_type()' but has no `marshal_signature/1' clause, so it is
not part of the generated language.
""".
type() ->
    ?SIZED(Size, type(erlang:min(Size, 6))).

type(0) ->
    %% Flattened rather than `union([variant, basic_type()])': `union/1' picks
    %% uniformly over its list elements, so a nested union counts once. Written
    %% that way `variant' is half of every leaf type, and since a variant's
    %% value draws a fresh full-size `type()', the recursion is supercritical --
    %% a single draw grows without bound and exhausts memory before the property
    %% runs. Listing the basic types here makes `variant' one leaf in thirteen.
    union([
        variant,
        byte,
        boolean,
        int16,
        uint16,
        int32,
        uint32,
        int64,
        uint64,
        double,
        string,
        object_path,
        signature
    ]);
type(Size) ->
    ?LAZY(
        union([
            type(0),
            ?LET(T, ?SUCHTHAT(T0, type(Size div 2), is_array_element(T0)), {array, T}),
            ?LET(Ts, sub_types(Size), {struct, Ts}),
            ?LET({K, V}, {basic_type(), type(Size div 2)}, {dict, K, V})
        ])
    ).

sub_types(Size) ->
    ?LET(N, integer(1, 3), vector(N, type(Size div 2))).

%% One element type an array may not have here. It is a defect, not a rule of
%% the protocol, and it is confined to arrays -- a dict nested anywhere else is
%% generated as normal.
%%
%% BUG-5: `padding/1' has no `{dict,_,_}' clause and `pad/2' has no guard
%% matching one, and an array is the only construct that consults the padding of
%% its element type. An array of dicts is a function_clause in both directions.
is_array_element({dict, _, _}) -> false;
is_array_element(_) -> true.

-doc """
A complete signature.

Bounded by the specification's 255-byte limit on the encoded form, which is also
what makes a signature marshallable: its length is written as a single byte.
`marshal_signature/1' raises on a longer one rather than returning it, so the
filter has to ask the encoder rather than measure its result.
""".
signature() ->
    ?SUCHTHAT(
        Sig,
        ?LET(N, integer(1, 4), vector(N, type())),
        is_marshallable(Sig)
    ).

is_marshallable(Sig) ->
    case catch dbus_marshaller:marshal_signature(Sig) of
        Bin when is_binary(Bin) -> byte_size(Bin) =< 255;
        _Error -> false
    end.

sig_bin(Sig) ->
    dbus_marshaller:marshal_signature(Sig).

%%%
%%% Values
%%%

-doc """
A canonical value of the given type.

Canonical means: the shape `unmarshal/4' produces, modulo the shape changes
`canon/2' describes. Binaries for the string-like types, floats for `double',
tuples for structs, maps for dicts, `#dbus_variant{}' for variants.
""".
value(byte) ->
    integer(0, 255);
value(boolean) ->
    boolean();
value(int16) ->
    %% BUG-1: the guard is `Value > -32767', so the two smallest int16 values the
    %% specification allows are rejected. Spec range is -32768..32767.
    integer(-32766, 32767);
value(uint16) ->
    integer(0, 65535);
value(int32) ->
    %% BUG-2: the guard is `Value >= -2147483647'; the spec minimum is
    %% -2147483648.
    integer(-2147483647, 2147483647);
value(uint32) ->
    integer(0, 4294967295);
value(int64) ->
    integer(-9223372036854775808, 9223372036854775807);
value(uint64) ->
    integer(0, 18446744073709551615);
value(double) ->
    float();
value(string) ->
    text();
value(object_path) ->
    object_path();
value(signature) ->
    ?LET(Sig, signature(), sig_bin(Sig));
value({array, byte}) ->
    %% `marshal/3' has a dedicated binary clause for byte arrays; generating a
    %% binary is what reaches `marshal_byte_array/2'.
    binary();
value({array, SubType}) ->
    short_list(value(SubType));
value({struct, SubTypes}) ->
    ?LET(Vs, [value(T) || T <- SubTypes], list_to_tuple(Vs));
value({dict, KeyType, ValueType}) ->
    ?LET(
        KVs,
        short_list({value(KeyType), value(ValueType)}),
        maps:from_list(KVs)
    );
value(variant) ->
    ?LET(T, type(), ?LET(V, value(T), #dbus_variant{type = T, value = V})).

%% Collections are kept short deliberately. `list/1' at PropEr's default size
%% draws around forty elements, and a value of a three-deep nested type is then
%% built from tens of thousands of leaves -- enough to exhaust memory before the
%% property is ever checked.
short_list(Gen) ->
    ?LET(N, integer(0, 4), vector(N, Gen)).

-doc "One value per type of a signature.".
values(Sig) ->
    [value(T) || T <- Sig].

-doc "A signature and a matching value list.".
sig_and_values() ->
    ?LET(Sig, signature(), ?LET(Vs, values(Sig), {Sig, Vs})).

-doc """
A signature and value list ending in a `uint64' guard element.

BUG-4: `unmarshal/4' answers `more' for a struct whenever fewer than 8 bytes
remain in the buffer, complete or not. Eight bytes of trailing scalar keep every
struct clear of the end. A message body cannot be padded from the outside -- its
length is in the header -- so the guard has to be part of the signature.
""".
guarded_sig_and_values() ->
    ?LET(
        {Sig, Vs},
        sig_and_values(),
        ?LET(Guard, value(uint64), {Sig ++ [uint64], Vs ++ [Guard]})
    ).

-doc "A type, a value of it, and a starting position to encode it at.".
typed_value() ->
    ?LET(T, type(), ?LET(V, value(T), {T, V, integer(0, 64)})).

-doc "A dict type, a map value of it, and a starting position.".
dict_typed_value() ->
    ?LET(
        {KeyType, ValueType},
        {basic_type(), type()},
        ?LET(
            V,
            value({dict, KeyType, ValueType}),
            {{dict, KeyType, ValueType}, V, integer(0, 64)}
        )
    ).

-doc """
A type of fixed width, a value of it, and a starting position.

The alignment law can only be stated for these: for anything variable-width the
emitted padding cannot be recovered from the position delta alone.
""".
fixed_width_typed_value() ->
    ?LET(
        T,
        union([byte, boolean, int16, uint16, int32, uint32, int64, uint64, double]),
        ?LET(V, value(T), {T, V, integer(0, 64)})
    ).

-doc """
An array type, a value of it, and a starting position.

The three element types `prop_array_length_excludes_padding' is stated over are
drawn explicitly beside a general one: `byte' and `int64' are the extremes of
element padding after the length word -- none and four bytes -- and a struct is
the case where the element's alignment is stricter than anything it contains.
""".
array_typed_value() ->
    ?LET(
        SubType,
        array_element_type(),
        ?LET(V, value({array, SubType}), {{array, SubType}, V, integer(0, 64)})
    ).

array_element_type() ->
    union([
        byte,
        int64,
        ?LET(Ts, sub_types(4), {struct, Ts}),
        ?SUCHTHAT(T, type(), is_array_element(T))
    ]).

-doc """
A type, a value of it whose first encoded byte is not zero, and a starting
position.

That guarantee is what makes the alignment law statable for every type rather
than only the fixed-width ones: the padding a marshalling emitted is then
exactly the count of leading zero bytes in its output, and nothing has to be
recovered from a position delta.

It is met differently per type. A string-like type, an array and a dict all
start with their length word, so a short non-empty one starts non-zero; a
variant starts with the length byte of its signature, which is never zero; an
integer is filtered on its least significant byte, which little-endian puts
first; a struct is given a non-zero `byte' as its first field.
""".
alignment_probe() ->
    ?LET(
        Type,
        probe_type(),
        ?LET(V, probe_value(Type), {Type, V, integer(0, 64)})
    ).

probe_type() ->
    union([
        byte,
        boolean,
        int16,
        uint16,
        int32,
        uint32,
        int64,
        uint64,
        double,
        string,
        object_path,
        signature,
        variant,
        {array, byte},
        {array, uint64},
        ?LET(Ts, sub_types(4), {struct, [byte | Ts]}),
        {dict, byte, byte}
    ]).

probe_value(byte) ->
    integer(1, 255);
probe_value(boolean) ->
    %% `false' encodes as four zero bytes, which no count of leading zeros can
    %% tell from padding.
    exactly(true);
probe_value(double) ->
    ?SUCHTHAT(V, value(double), first_byte(<<V:64/little-float>>) =/= 0);
probe_value(Type) when
    Type =:= int16;
    Type =:= uint16;
    Type =:= int32;
    Type =:= uint32;
    Type =:= int64;
    Type =:= uint64
->
    ?SUCHTHAT(V, value(Type), V band 255 =/= 0);
probe_value(Type) when Type =:= string; Type =:= object_path ->
    %% The length is a `uint32'; one that is a multiple of 256 would put a zero
    %% in its first byte.
    ?SUCHTHAT(V, value(Type), byte_size(V) > 0 andalso byte_size(V) rem 256 =/= 0);
probe_value(signature) ->
    %% A signature is at least one type code long and its length is one byte.
    value(signature);
probe_value(variant) ->
    value(variant);
probe_value({array, byte}) ->
    ?LET(N, integer(1, 32), binary(N));
probe_value({array, uint64}) ->
    %% n is 8 per element, so one to four elements keep it clear of 256.
    ?LET(N, integer(1, 4), vector(N, value(uint64)));
probe_value({struct, [byte | SubTypes]}) ->
    ?LET(
        {B, Vs},
        {integer(1, 255), [value(T) || T <- SubTypes]},
        list_to_tuple([B | Vs])
    );
probe_value({dict, byte, byte}) ->
    %% One entry, so n is 2 whatever the key and the value are.
    ?LET({K, V}, {value(byte), value(byte)}, #{K => V}).

first_byte(<<B:8, _/binary>>) -> B.

-doc "Byte width of a fixed-width type, padding excluded.".
fixed_width(byte) -> 1;
fixed_width(boolean) -> 4;
fixed_width(int16) -> 2;
fixed_width(uint16) -> 2;
fixed_width(int32) -> 4;
fixed_width(uint32) -> 4;
fixed_width(int64) -> 8;
fixed_width(uint64) -> 8;
fixed_width(double) -> 8.

-doc """
Alignment boundary of a type, in bytes.

Read off the specification's "Summary of D-Bus marshalling" table, and duplicated
here for the reason `fixed_width/1' is: `dbus_marshaller:padding/1' is a function
under test, so a law stated against it is checked against the implementation's
own opinion of where the boundaries are.

`{dict, K, V}' is this library's spelling of `a{kv}' -- an array of dict
entries, not the entry itself -- so it aligns as an array, to 4. The 8-byte
alignment the specification gives a dict entry belongs to the
`{struct, [K, V]}' the marshaller builds from it.
""".
alignment(byte) -> 1;
alignment(boolean) -> 4;
alignment(int16) -> 2;
alignment(uint16) -> 2;
alignment(int32) -> 4;
alignment(uint32) -> 4;
alignment(int64) -> 8;
alignment(uint64) -> 8;
alignment(double) -> 8;
alignment(string) -> 4;
alignment(object_path) -> 4;
alignment(signature) -> 1;
alignment(variant) -> 1;
alignment({array, _SubType}) -> 4;
alignment({struct, _SubTypes}) -> 8;
alignment({dict, _KeyType, _ValueType}) -> 4.

-doc "The first position at or after `Pos' on a `Type' boundary.".
align(Type, Pos) ->
    A = alignment(Type),
    Pos + (A - Pos rem A) rem A.

-doc """
A value marshalled as a variant *without* a `#dbus_variant{}' wrapper.

This is the `infer_type/1' path: the encoder picks a type from the value alone.
The type it picks is not recoverable, so only the value survives the round trip.
""".
bare_variant_value() ->
    union([
        boolean(),
        float(),
        text(),
        %% BUG-3: `marshal_int_variant/2' sends -32768 and -32767 to `int16' and
        %% everything down to -4294967296 to `int32', both of which `marshal/3'
        %% then rejects. Only the values outside those two windows encode.
        integer(-32766, 4294967295),
        integer(-9223372036854775808, -4294967297),
        integer(4294967296, 18446744073709551615)
    ]).

text() ->
    ?LET(
        Cs,
        list(
            union([
                integer($a, $z),
                integer($A, $Z),
                integer($0, $9),
                $_,
                $-,
                $.,
                $\s,
                %% Two multi-byte code points, so the byte length of a string is
                %% not its character count.
                16#00E9,
                16#4E2D
            ])
        ),
        unicode:characters_to_binary(Cs)
    ).

object_path() ->
    ?LET(
        Elems,
        list(non_empty(list(union([integer($a, $z), integer($0, $9), $_])))),
        case Elems of
            [] -> <<"/">>;
            _ -> list_to_binary([[$/, E] || E <- Elems])
        end
    ).

%%%
%%% Lax inputs
%%%

-doc """
A non-canonical but equivalent input for `marshal/3'.

`marshal/3' accepts atoms and iolists where it produces binaries, lists where it
produces tuples, and proplists where it produces maps. Those clauses are only
reached by feeding them something a round-trip generator would never produce.

Two accepted inputs are deliberately absent, because they are not byte-for-byte
equivalent and so belong to properties of their own: an atom in place of a
string (generating one per test would grow the atom table without bound -- see
`prop_dbus_marshaller:prop_atom_string_input/0'), and a legacy `dict:dict()' in
place of a map, whose `to_list/1' order is unrelated to `maps:to_list/1''s and
so yields the same entries in a different order -- see
`prop_dbus_marshaller:prop_legacy_dict_input/0'.
""".
lax(string, V) when is_binary(V) ->
    union([V, binary_to_list(V)]);
lax(object_path, V) when is_binary(V) ->
    union([V, binary_to_list(V)]);
lax({array, byte}, V) when is_binary(V) ->
    union([V, binary_to_list(V)]);
lax({struct, _}, V) when is_tuple(V) ->
    union([V, tuple_to_list(V)]);
lax({dict, _, _}, V) when is_map(V) ->
    union([V, maps:to_list(V)]);
lax(_Type, V) ->
    exactly(V).

%%%
%%% Canonicalisation
%%%

-doc """
The value `unmarshal/4' returns for a value `marshal/3' was given.

Every clause here is an asymmetry between the two directions, read off
`dbus_marshaller':

- the string-like types accept atoms and iolists, and return binaries;
- a byte array accepts a binary, and returns a list of bytes;
- a struct accepts a list or a tuple, and returns a tuple;
- a dict accepts a map, a proplist or a legacy `dict:dict()', and returns a map,
  so duplicate keys collapse;
- a variant returns the bare value: the wrapper and its declared type are lost;
- `double' accepts an integer and returns a float.
""".
canon(string, V) ->
    to_binary(V);
canon(object_path, V) ->
    to_binary(V);
canon(signature, V) ->
    to_binary(V);
canon(double, V) when is_integer(V) ->
    float(V);
canon({array, byte}, V) when is_binary(V) ->
    binary_to_list(V);
canon({array, SubType}, V) when is_list(V) ->
    [canon(SubType, E) || E <- V];
canon({struct, SubTypes}, V) when is_tuple(V) ->
    canon({struct, SubTypes}, tuple_to_list(V));
canon({struct, SubTypes}, V) when is_list(V) ->
    list_to_tuple(lists:zipwith(fun canon/2, SubTypes, V));
canon({dict, KeyType, ValueType}, V) when is_map(V) ->
    canon({dict, KeyType, ValueType}, maps:to_list(V));
canon({dict, KeyType, ValueType}, V) when is_list(V) ->
    maps:from_list([{canon(KeyType, K), canon(ValueType, Val)} || {K, Val} <- V]);
canon({dict, KeyType, ValueType}, V) when element(1, V) =:= dict ->
    canon({dict, KeyType, ValueType}, dict:to_list(V));
canon(variant, #dbus_variant{type = Type, value = V}) ->
    canon(Type, V);
canon(_Type, V) ->
    V.

-doc "`canon/2' over a signature and its value list.".
canon_list(Sig, Vs) ->
    lists:zipwith(fun canon/2, Sig, Vs).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> list_to_binary(V).

%%%
%%% Messages
%%%

-doc """
A marshallable message, paired with the message decoding it must give back.

The pair is generated rather than derived because the expected body can only be
built from the values that went in; recomputing it by decoding would make the
property compare the decoder against itself.

Three constraints come from the encoder rather than from the protocol:
`marshal_message/1' errors on serial 0; it does not synthesise the `SIGNATURE'
header field, while `unmarshal_message/1' errors on a non-empty body that has
none, so the generator has to set it; and the decoded message differs from the
encoded one in three fixed ways -- `size' is filled in, header field values lose
their `#dbus_variant{}' wrapper, and the body is the decoded values alone rather
than the `{Signature, Values}' pair that was encoded. That last one means
`marshal_message/1' cannot consume the output of `unmarshal_data/1', even though
`#dbus_message.body' is typed as if it could.
""".
message() ->
    ?LET(
        {Sig, Vs},
        guarded_sig_and_values(),
        ?LET(
            {Type, Flags, Serial, Extra},
            {message_type(), integer(0, 255), integer(1, 4294967295), extra_fields()},
            begin
                Fields =
                    [
                        {?FIELD_SIGNATURE, #dbus_variant{type = signature, value = sig_bin(Sig)}}
                        | Extra
                    ],
                Header = #dbus_header{
                    type = Type,
                    flags = Flags,
                    serial = Serial,
                    fields = Fields
                },
                Decoded = #dbus_message{
                    header = Header#dbus_header{
                        size = body_size(Sig, Vs),
                        fields = [{Code, canon(variant, Var)} || {Code, Var} <- Fields]
                    },
                    body = decoded_body(Sig, Vs)
                },
                {#dbus_message{header = Header, body = {Sig, Vs}}, Decoded}
            end
        )
    ).

-doc """
No `SIGNATURE` field
Empty body
""".
empty_message() ->
    ?LET(
        {Type, Flags, Serial},
        {message_type(), integer(0, 255), integer(1, 4294967295)},
        #dbus_message{
            header = #dbus_header{
                type = Type,
                flags = Flags,
                serial = Serial,
                fields = []
            },
            body = undefined
        }
    ).

-doc """
The number of bytes `marshal_message/1' writes for a body, i.e. the `size' the
decoded header carries.

`marshal_list/2' encodes from position 0, so the position it ends at is the
length of what it emitted.
""".
body_size(Sig, Vs) ->
    {_Io, Pos} = dbus_marshaller:marshal_list(Sig, Vs),
    Pos.

%% `unmarshal_body/4' returns the values without the signature that was decoded
%% with them, and unwraps a one-value body out of its tuple.
decoded_body(Sig, Vs) ->
    case canon_list(Sig, Vs) of
        [] -> undefined;
        [One] -> One;
        Many -> list_to_tuple(Many)
    end.

-doc "A short stream of messages, paired with what they must decode to.".
messages() ->
    ?LET(N, integer(1, 4), vector(N, message())).

message_type() ->
    %% ?TYPE_INVALID is excluded: `unmarshal_body/4' discards the body for it,
    %% so no round-trip law holds.
    union([?TYPE_METHOD_CALL, ?TYPE_METHOD_RETURN, ?TYPE_ERROR, ?TYPE_SIGNAL]).

extra_fields() ->
    ?LET(
        {Path, Iface, Member, Serial},
        {object_path(), text(), text(), integer(0, 4294967295)},
        ?LET(
            Which,
            list(union([path, interface, member, reply_serial])),
            [
                extra_field(W, Path, Iface, Member, Serial)
             || W <- lists:usort(Which)
            ]
        )
    ).

extra_field(path, Path, _, _, _) ->
    {?FIELD_PATH, #dbus_variant{type = object_path, value = Path}};
extra_field(interface, _, Iface, _, _) ->
    {?FIELD_INTERFACE, #dbus_variant{type = string, value = Iface}};
extra_field(member, _, _, Member, _) ->
    {?FIELD_MEMBER, #dbus_variant{type = string, value = Member}};
extra_field(reply_serial, _, _, _, Serial) ->
    {?FIELD_REPLY_SERIAL, #dbus_variant{type = uint32, value = Serial}}.

%%%
%%% Spec figures
%%%

-doc """
A worked example from the specification: `{Signature, Values, Endian, Bytes}'.

`Bytes' is the encoding the D-Bus authors wrote down, so it is an oracle by
definition -- the only one in this suite that no part of `dbus_marshaller'
contributed to. `Values' is what `marshal_list/2' is given; what unmarshalling
gives back is `canon_list/2' of it.

The two big-endian figures are the specification's own; the little-endian
transcription beside each is the same figure with the byte order of every
numeric field reversed, since `dbus_marshaller' only ever writes `$l' and the
encoding direction could not be checked otherwise.
""".
spec_figure() ->
    union([
        %% Marshalling basic types: the strings `foo', `+' and `bar' in
        %% sequence from a multiple of 8, little-endian, with two bytes of
        %% padding before the third length word.
        {[string, string, string], [<<"foo">>, <<"+">>, <<"bar">>], $l, <<
            16#03,
            16#00,
            16#00,
            16#00,
            $f,
            $o,
            $o,
            16#00,
            16#01,
            16#00,
            16#00,
            16#00,
            $+,
            16#00,
            16#00,
            16#00,
            16#03,
            16#00,
            16#00,
            16#00,
            $b,
            $a,
            $r,
            16#00
        >>},
        %% Marshalling containers: an array holding only the 64-bit integer 5,
        %% big-endian, from a multiple of 8. Four bytes of padding between the
        %% length word and the element, which n does not count.
        {[{array, int64}], [[5]], $B, <<
            16#00,
            16#00,
            16#00,
            16#08,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#05
        >>},
        {[{array, int64}], [[5]], $l, <<
            16#08,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#05,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00
        >>},
        %% Marshalling containers: a variant holding the 64-bit integer 5,
        %% big-endian, from a multiple of 8. The variant itself needs no
        %% padding; the value inside it needs five bytes.
        {[variant], [#dbus_variant{type = uint64, value = 5}], $B, <<
            16#01,
            $t,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#05
        >>},
        {[variant], [#dbus_variant{type = uint64, value = 5}], $l, <<
            16#01,
            $t,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#05,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00,
            16#00
        >>}
    ]).

%%%
%%% Corruption
%%%

-doc """
A binary with bytes overwritten and its tail cut off at an arbitrary point.

The shapes a real peer produces when it is buggy or hostile, and the ones a
`binary()' almost never reaches: random bytes are rejected by the first length
or type code they meet, so nothing past that is ever exercised without starting
from something valid.

`Bin' must be non-empty.
""".
corrupted(Bin) ->
    ?LET(
        {Overwrites, Cut},
        {
            list({integer(0, byte_size(Bin) - 1), integer(0, 255)}),
            integer(0, byte_size(Bin))
        },
        binary:part(lists:foldl(fun overwrite/2, Bin, Overwrites), 0, Cut)
    ).

overwrite({Index, Byte}, Bin) ->
    <<Head:Index/binary, _:8, Tail/binary>> = Bin,
    <<Head/binary, Byte:8, Tail/binary>>.
