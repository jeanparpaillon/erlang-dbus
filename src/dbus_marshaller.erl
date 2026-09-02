-module(dbus_marshaller).
-moduledoc """
D-Bus binary format (un)marshaling.

See [D-Bus
Specification](https://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-marshaling).
""".

-include("dbus.hrl").
-include("dbus_constants.hrl").

-elvis([
    {elvis_style, export_used_types, #{ignore => [dbus_marshaller]}},
    {elvis_style, dont_repeat_yourself, #{
        ignore => [
            {dbus_marshaller, marshal_byte_array, 2},
            {dbus_marshaller, marshal_array, 3},
            {dbus_marshaller, unmarshal_dict_test, 0},
            {dbus_marshaller, marshall_string_test_, 0}
        ]
    }}
]).

%% `marshal_dict/4' also accepts a legacy `dict:dict()', recognised by its tag
%% because that is all a guard can test. That narrows the argument to `tuple()',
%% never to the opaque `dict:dict()', so the `dict:to_list/1' call is reported as an
%% opacity violation that no rewriting of the clause can avoid.
-dialyzer({no_opaque, [marshal_dict/4]}).

-ifdef(TEST).
%% The public API always encodes a whole message from position 0 and offers no
%% way to encode or decode a bare value list, so the signature, alignment and
%% offset laws cannot be stated through it. These exports let
%% `test/prop_dbus_marshaller.erl' and `test/dbus_marshaller_gen.erl' reach the
%% functions those laws are about.
-export([
    marshal/3,
    marshal_list/2,
    marshal_signature/1,
    unmarshal/4,
    unmarshal_tuple/3,
    unmarshal_signature/1
]).
-endif.

%% api
-export([
    marshal_message/1,
    unmarshal_data/1
]).

-define(HEADER_SIGNATURE, [
    byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}
]).

%% The limits the specification's "Valid Signatures" section puts on the
%% signature language. The length is not a decorative one: a signature is
%% marshalled with its length in a single byte, so a 256-byte signature could
%% not be read back.
-define(SIGNATURE_MAX_LENGTH, 255).
-define(SIGNATURE_MAX_ARRAY_DEPTH, 32).
-define(SIGNATURE_MAX_STRUCT_DEPTH, 32).
-define(SIGNATURE_MAX_DEPTH, 64).

-type signature_error() ::
    too_long
    | {too_deep, array | struct | total}
    | {bad_type_code, byte()}
    | {bad_dict_key, byte()}
    | {unbalanced, byte()}
    | {trailing, binary()}
    | empty_struct
    | dict_entry_outside_array
    | {dict_entry_arity, byte()}.

-type error() ::
    invalid_serial
    | {marshaling, dbus_type(), binary()}
    | {unmarshaling, dbus_type(), binary()}
    | {dbus_parse_error, term()}
    | {bad_type_code, integer()}
    | {bad_signature, signature_error()}
    | dbus_parse_error
    | body_parse_error
    | bad_header
    | term().

-export_type([error/0]).

%%%
%%% API
%%%

-doc """
Encode a message.

Encodes a `dbus_message:t()` into an iolist, including any padding that may be
required. Such a marshalled message is ready to send through a socket onto D-Bus.
""".
-spec marshal_message(dbus_message()) -> binary().
marshal_message(#dbus_message{header = #dbus_header{serial = 0}}) ->
    error(invalid_serial);
marshal_message(#dbus_message{header = Header, body = undefined}) ->
    marshal_header(Header#dbus_header{size = 0});
marshal_message(#dbus_message{header = Header, body = {Types, Content}}) ->
    try marshal_list(Types, Content) of
        {Data, Pos} ->
            HeaderBin = marshal_header(Header#dbus_header{size = Pos}),
            BodyBin = iolist_to_binary(Data),
            <<HeaderBin/binary, BodyBin/binary>>
    catch
        error:_Err ->
            error({?DBUS_ERROR_INVALID_PARAMETERS, Types})
    end.

-doc """
Decode messages.

Returns:

- `{ok, [dbus_message()], binary()}`: if the binary describes a complete list of
  messages, eventually with a remaining binary.
- `more`: if no complete message could be decoded.
""".
-spec unmarshal_data(binary()) ->
    {ok, Msgs :: [dbus_message()], Rest :: binary()}
    | {error, error()}
    | more.
unmarshal_data(Data) ->
    try
        unmarshal_data(Data, [])
    catch
        error:Err ->
            {error, Err}
    end.

%%%
%%% Priv
%%%
-doc """
Encode a signature.

Raises `{bad_signature, Reason}` for the two things a type may be that the
specification forbids on the wire: longer than 255 bytes once encoded, or
nested deeper than 32 arrays, 32 open parentheses or 64 in total.
`unmarshal_signature/1` rejects the same language from the other side, so a
signature this function produces always parses back.
""".
-spec marshal_signature(dbus_type() | dbus_signature()) -> binary().
marshal_signature(Type) ->
    case signature_depth(Type, 0, 0) of
        {error, Err} ->
            error(Err);
        ok ->
            Sig = iolist_to_binary(signature_bytes(Type)),
            case byte_size(Sig) > ?SIGNATURE_MAX_LENGTH of
                true -> error({bad_signature, too_long});
                false -> Sig
            end
    end.

-doc "Encode objects, given a signature.".
-spec marshal_list(dbus_signature(), term()) -> {iolist(), integer()}.
marshal_list(Types, Value) ->
    marshal_list(Types, Value, 0, []).

-doc """
Decode a signature.

The three answers are the ones `unmarshal_data/1` gives, and for the same
reason: the argument comes off a socket, so it is arbitrary bytes.

- `{ok, dbus_signature()}`: the whole binary is a list of single complete
  types.
- `more`: a well-formed signature, cut short.
- `{error, {bad_signature, Reason}}`: a shape the specification's
  "Valid Signatures" section forbids -- a byte that is not a type code, the
  `r` and `e` type codes that stand for STRUCT and DICT_ENTRY outside a
  signature, an unbalanced bracket, a dict entry outside an array or with the
  wrong number of fields, an empty struct, or a signature over 255 bytes or
  nested past 32 arrays, 32 parentheses or 64 in total.
""".
-spec unmarshal_signature(binary()) -> {ok, dbus_signature()} | {error, error()} | more.
unmarshal_signature(<<>>) ->
    {ok, []};
unmarshal_signature(Bin) when is_binary(Bin), byte_size(Bin) > ?SIGNATURE_MAX_LENGTH ->
    {error, {bad_signature, too_long}};
unmarshal_signature(Bin) when is_binary(Bin) ->
    case parse_types(Bin, 0, 0, []) of
        {ok, Signature, <<>>} -> {ok, Signature};
        {ok, _Signature, Rest} -> {error, {bad_signature, {trailing, Rest}}};
        more -> more;
        {error, _} = Err -> Err
    end.

-spec marshal_header(dbus_header()) -> binary().
marshal_header(#dbus_header{} = Header) ->
    HeaderFields = [
        Header#dbus_header.endian,
        Header#dbus_header.type,
        Header#dbus_header.flags,
        Header#dbus_header.version,
        Header#dbus_header.size,
        Header#dbus_header.serial,
        Header#dbus_header.fields
    ],
    {Value, Pos} = marshal_list(?HEADER_SIGNATURE, HeaderFields),
    case pad(8, Pos) of
        0 -> iolist_to_binary(Value);
        Pad -> iolist_to_binary([Value, <<0:Pad>>])
    end.

marshal_list([], [], Pos, Res) ->
    {Res, Pos};
marshal_list([Type | T], [Value | V], Pos, Res) ->
    {Res1, Pos1} = marshal(Type, Value, Pos),
    marshal_list(T, V, Pos1, [Res, Res1]).

marshal(byte, Value, Pos) when is_integer(Value) andalso 255 >= Value ->
    marshal_uint(1, Value, Pos);
marshal(boolean, Value, Pos) when is_boolean(Value) ->
    Int =
        case Value of
            true -> 1;
            false -> 0
        end,
    marshal(uint32, Int, Pos);
marshal(int16, Value, Pos) when Value > -32767 andalso Value =< 32767 ->
    marshal_int(2, Value, Pos);
marshal(uint16, Value, Pos) when Value >= 0 andalso Value =< 65535 ->
    marshal_uint(2, Value, Pos);
marshal(int32, Value, Pos) when Value >= -2147483647 andalso Value =< 2147483647 ->
    marshal_int(4, Value, Pos);
marshal(uint32, Value, Pos) when Value >= 0 andalso Value =< 4294967295 ->
    marshal_uint(4, Value, Pos);
marshal(int64, Value, Pos) ->
    marshal_int(8, Value, Pos);
marshal(uint64, Value, Pos) when Value >= 0 ->
    marshal_uint(8, Value, Pos);
marshal(double, Value, Pos) when is_integer(Value) ->
    Pad = pad(8, Pos),
    {<<0:Pad, (float(Value)):64/little-float>>, Pos + Pad div 8 + 8};
marshal(double, Value, Pos) when is_float(Value) ->
    Pad = pad(8, Pos),
    {<<0:Pad, Value:64/little-float>>, Pos + Pad div 8 + 8};
marshal(string, Value, Pos) when is_atom(Value) ->
    marshal(string, atom_to_binary(Value, utf8), Pos);
marshal(string, Value, Pos) when is_binary(Value) ->
    marshal_string(uint32, Value, Pos);
marshal(string, Value, Pos) when is_list(Value) ->
    marshal(string, list_to_binary(Value), Pos);
marshal(object_path, Value, Pos) ->
    marshal(string, Value, Pos);
marshal(signature, Value, Pos) ->
    marshal_string(byte, Value, Pos);
marshal({array, {struct, [_KeyType, _ValueType]} = SubType}, Value, Pos) when is_map(Value) ->
    marshal_array(SubType, maps:to_list(Value), Pos);
marshal({array, byte} = _Type, Value, Pos) when is_binary(Value) ->
    marshal_byte_array(Value, Pos);
marshal({array, SubType}, Value, Pos) when is_list(Value) ->
    marshal_array(SubType, Value, Pos);
marshal({struct, _SubTypes} = Type, Value, Pos) when is_tuple(Value) ->
    marshal(Type, tuple_to_list(Value), Pos);
marshal({struct, SubTypes}, Value, Pos) when is_list(Value) ->
    marshal_struct(SubTypes, Value, Pos);
marshal({dict, KeyType, ValueType}, Value, Pos) ->
    marshal_dict(KeyType, ValueType, Value, Pos);
marshal(variant, Value, Pos) when is_binary(Value) ->
    marshal_variant({array, byte}, Value, Pos);
marshal(variant, #dbus_variant{type = Type, value = Value}, Pos) ->
    marshal_variant(Type, Value, Pos);
marshal(variant, true = Value, Pos) ->
    marshal_variant(boolean, Value, Pos);
marshal(variant, false = Value, Pos) ->
    marshal_variant(boolean, Value, Pos);
marshal(variant, Value, Pos) when is_float(Value) ->
    marshal_variant(double, Value, Pos);
marshal(variant, Value, Pos) when is_integer(Value) andalso Value < 0 ->
    marshal_int_variant(Value, Pos);
marshal(variant, Value, Pos) when is_integer(Value) andalso Value >= 0 ->
    marshal_uint_variant(Value, Pos);
marshal(variant, Value, Pos) when is_list(Value) ->
    marshal(variant, list_to_binary(Value), Pos);
marshal(variant, Value, Pos) when is_atom(Value) ->
    marshal_variant(string, atom_to_binary(Value, utf8), Pos);
marshal(variant, Value, Pos) ->
    Type = infer_type(Value),
    marshal_variant(Type, Value, Pos);
marshal(Type, #dbus_variant{type = Type, value = Value}, Pos) ->
    marshal(Type, Value, Pos);
marshal(Type, Value, _) ->
    error({marshaling, Type, Value}).

infer_type(Value) when is_binary(Value) ->
    {array, byte};
infer_type(true) ->
    boolean;
infer_type(false) ->
    boolean;
infer_type(Value) when is_integer(Value), Value < 0 ->
    infer_int(Value);
infer_type(Value) when is_integer(Value), Value >= 0 ->
    infer_uint(Value);
infer_type(Value) when is_tuple(Value) ->
    infer_struct(tuple_to_list(Value));
infer_type(Value) when is_atom(Value) ->
    string;
infer_type(Value) when is_list(Value) ->
    string;
infer_type(Value) when is_map(Value) ->
    infer_dict(Value).

infer_struct(Values) ->
    {struct, infer_struct(Values, [])}.

infer_struct([], Res) ->
    lists:reverse(Res);
infer_struct([Value | R], Res) ->
    infer_struct(R, [infer_type(Value) | Res]).

infer_int(Value) when Value >= -32767 ->
    int16;
infer_int(Value) when Value >= -2147483647 ->
    int32;
infer_int(_Value) ->
    int64.

infer_uint(Value) when Value < 32768 ->
    uint16;
infer_uint(Value) when Value < 4294967296 ->
    uint32;
infer_uint(_Value) ->
    uint64.

infer_dict(_Value) ->
    %% Can do better without going through all keys ?...
    {dict, variant, variant}.

marshal_int_variant(Value, Pos) when Value >= -32768 ->
    marshal_variant(int16, Value, Pos);
marshal_int_variant(Value, Pos) when Value >= -4294967296 ->
    marshal_variant(int32, Value, Pos);
marshal_int_variant(Value, Pos) ->
    marshal_variant(int64, Value, Pos).

marshal_uint_variant(Value, Pos) when Value < 32768 ->
    marshal_variant(uint16, Value, Pos);
marshal_uint_variant(Value, Pos) when Value < 4294967296 ->
    marshal_variant(uint32, Value, Pos);
marshal_uint_variant(Value, Pos) ->
    marshal_variant(uint64, Value, Pos).

marshal_variant(Type, Value, Pos) ->
    {Value1, Pos1} = marshal(signature, marshal_signature(Type), Pos),
    {Value2, Pos2} = marshal(Type, Value, Pos1),
    {[Value1, Value2], Pos2}.

marshal_uint(Len, Value, Pos) when is_integer(Value) ->
    Pad = pad(Len, Pos),
    {<<0:Pad, Value:(Len * 8)/little-unsigned>>, Pos + Pad div 8 + Len}.

marshal_int(Len, Value, Pos) when is_integer(Value) ->
    Pad = pad(Len, Pos),
    {<<0:Pad, Value:(Len * 8)/little-signed>>, Pos + Pad div 8 + Len}.

marshal_string(LenType, Value, Pos) when is_list(Value) ->
    marshal_string(LenType, list_to_binary(Value), Pos);
marshal_string(LenType, Value, Pos) when is_binary(Value) ->
    Length = byte_size(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    {[Value1, Value, 0], Pos1 + Length + 1}.

marshal_byte_array(Value, Pos) ->
    Pad = pad(uint32, Pos),
    Pos0 = Pos + Pad div 8,
    Pos1 = Pos0 + 4,
    Pad1 = pad(byte, Pos1),
    Pos1b = Pos1 + Pad1 div 8,
    Length = byte_size(Value),
    Pos2 = Pos1b + Length,
    {Value1, Pos1} = marshal(uint32, Length, Pos0),
    {[<<0:Pad>>, Value1, <<0:Pad1>>, Value], Pos2}.

marshal_array(SubType, Value, Pos) ->
    Pad = pad(uint32, Pos),
    Pos0 = Pos + Pad div 8,
    Pos1 = Pos0 + 4,
    Pad1 = pad(SubType, Pos1),
    Pos1b = Pos1 + Pad1 div 8,
    {Value2, Pos2} = marshal_array_item(SubType, Value, Pos1b),
    Length = Pos2 - Pos1b,
    {Value1, Pos1} = marshal(uint32, Length, Pos0),
    {[<<0:Pad>>, Value1, <<0:Pad1>>, Value2], Pos2}.

marshal_array_item(SubType, Array, Pos) ->
    marshal_array_item(SubType, Array, Pos, []).

marshal_array_item(_SubType, [], Pos, Res) ->
    {Res, Pos};
marshal_array_item(SubType, [Value | R], Pos, Res) ->
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_array_item(SubType, R, Pos1, [Res, Value1]).

marshal_dict(KeyType, ValueType, Value, Pos) when is_map(Value) ->
    marshal_array({struct, [KeyType, ValueType]}, maps:to_list(Value), Pos);
marshal_dict(KeyType, ValueType, Value, Pos) when is_list(Value) ->
    marshal_array({struct, [KeyType, ValueType]}, Value, Pos);
marshal_dict(KeyType, ValueType, Value, Pos) when element(1, Value) == dict ->
    marshal_array({struct, [KeyType, ValueType]}, dict:to_list(Value), Pos).

marshal_struct(SubTypes, Values, Pos) ->
    Pad = pad(8, Pos),
    {Values1, Pos1} = marshal_struct(SubTypes, Values, Pos + Pad div 8, []),
    case Pad of
        0 ->
            {Values1, Pos1};
        _ when Pad > 0 ->
            {[<<0:Pad>>, Values1], Pos1}
    end.

marshal_struct([], [], Pos, Res) ->
    {Res, Pos};
marshal_struct([SubType | R], [Value | V], Pos, Res) ->
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_struct(R, V, Pos1, [Res, Value1]).

marshal_struct_signature([], Res) ->
    Res;
marshal_struct_signature([SubType | R], Res) ->
    marshal_struct_signature(R, [Res, signature_bytes(SubType)]).

%% The encoding proper. `marshal_signature/1' is the checked entry point;
%% everything below it, and every recursive call, goes through here so that the
%% length and depth of a signature are checked once, at the top.
signature_bytes(byte) ->
    "y";
signature_bytes(boolean) ->
    "b";
signature_bytes(int16) ->
    "n";
signature_bytes(uint16) ->
    "q";
signature_bytes(int32) ->
    "i";
signature_bytes(uint32) ->
    "u";
signature_bytes(int64) ->
    "x";
signature_bytes(uint64) ->
    "t";
signature_bytes(double) ->
    "d";
signature_bytes(string) ->
    "s";
signature_bytes(object_path) ->
    "o";
signature_bytes(signature) ->
    "g";
signature_bytes({array, Type}) ->
    [$a, signature_bytes(Type)];
signature_bytes({struct, SubTypes}) ->
    ["(", marshal_struct_signature(SubTypes, []), ")"];
signature_bytes(variant) ->
    "v";
signature_bytes({dict, KeyType, ValueType}) ->
    KeySig = signature_bytes(KeyType),
    ValueSig = signature_bytes(ValueType),
    ["a{", KeySig, ValueSig, "}"];
signature_bytes([]) ->
    "";
signature_bytes([Type | R]) ->
    [signature_bytes(Type), signature_bytes(R)].

%% The nesting limits, walked over a type term. The parser counts the same two
%% depths as it descends and calls the same `check_depth/2', which is what
%% makes the two directions agree on one language.
signature_depth([], _ADepth, _PDepth) ->
    ok;
signature_depth([Type | R], ADepth, PDepth) ->
    case signature_depth(Type, ADepth, PDepth) of
        ok -> signature_depth(R, ADepth, PDepth);
        {error, _} = Err -> Err
    end;
signature_depth({array, Type}, ADepth, PDepth) ->
    signature_descend(ADepth + 1, PDepth, Type);
signature_depth({struct, SubTypes}, ADepth, PDepth) ->
    signature_descend(ADepth, PDepth + 1, SubTypes);
signature_depth({dict, KeyType, ValueType}, ADepth, PDepth) ->
    %% `a{kv}': one array code and one bracket pair.
    signature_descend(ADepth + 1, PDepth + 1, [KeyType, ValueType]);
signature_depth(_Basic, _ADepth, _PDepth) ->
    ok.

signature_descend(ADepth, PDepth, Sub) ->
    case check_depth(ADepth, PDepth) of
        ok -> signature_depth(Sub, ADepth, PDepth);
        {error, _} = Err -> Err
    end.

check_depth(ADepth, _PDepth) when ADepth > ?SIGNATURE_MAX_ARRAY_DEPTH ->
    {error, {bad_signature, {too_deep, array}}};
check_depth(_ADepth, PDepth) when PDepth > ?SIGNATURE_MAX_STRUCT_DEPTH ->
    {error, {bad_signature, {too_deep, struct}}};
check_depth(ADepth, PDepth) when ADepth + PDepth > ?SIGNATURE_MAX_DEPTH ->
    %% Implied by the two caps above rather than reachable past them. It is
    %% stated because the specification states it separately: the total is the
    %% limit that matters if the two ever move apart.
    {error, {bad_signature, {too_deep, total}}};
check_depth(_ADepth, _PDepth) ->
    ok.

%%%
%%% Private unmarshaling
%%%
unmarshal_data(<<>>, []) ->
    more;
unmarshal_data(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
unmarshal_data(Data, Acc) ->
    try unmarshal_message(Data) of
        {ok, #dbus_message{} = Msg, Rest} ->
            unmarshal_data(Rest, [Msg | Acc]);
        more when [] =:= Acc ->
            more;
        more ->
            {ok, lists:reverse(Acc), Data}
    catch
        {'EXIT', Err} ->
            error({dbus_parse_error, Err})
    end.

unmarshal_message(<<>>) ->
    more;
unmarshal_message(Data) when is_binary(Data) ->
    case unmarshal_header(Data) of
        more ->
            more;
        {ok, #dbus_header{endian = Endian, type = MsgType} = Header, BodyBin, Rest} ->
            case dbus_message:find_field(?FIELD_SIGNATURE, Header) of
                undefined ->
                    case BodyBin of
                        <<>> -> {ok, #dbus_message{header = Header, body = undefined}, Rest};
                        _ -> error(body_parse_error)
                    end;
                Signature ->
                    case unmarshal_body(MsgType, Signature, BodyBin, Endian) of
                        {ok, Body} -> {ok, #dbus_message{header = Header, body = Body}, Rest};
                        more -> more;
                        {error, Err} -> error(Err)
                    end
            end
    end.

unmarshal_body(?TYPE_INVALID, _, _, _) ->
    {ok, undefined};
unmarshal_body(_, SigBin, BodyBin, Endian) ->
    case unmarshal_signature(SigBin) of
        {error, _} = Err ->
            Err;
        {ok, Sig} ->
            case unmarshal_tuple(Sig, BodyBin, Endian) of
                more -> more;
                {ok, {}, <<>>, _Pos} -> {ok, undefined};
                {ok, {Body}, <<>>, _Pos} -> {ok, Body};
                {ok, Body, <<>>, _Pos} -> {ok, Body};
                {ok, _Body, _, _} -> {error, body_parse_error}
            end;
        more ->
            more
    end.

unmarshal_header(Bin) when byte_size(Bin) < 16 ->
    more;
unmarshal_header(<<Endian/integer, Type/integer, Flags/integer, ?DBUS_VERSION_MAJOR, Rest/bits>>) ->
    unmarshal_header2(Rest, #dbus_header{endian = Endian, type = Type, flags = Flags});
unmarshal_header(_Data) ->
    error(bad_header).

unmarshal_header2(
    <<Length:4/unsigned-little-integer-unit:8, Serial:4/unsigned-little-integer-unit:8, Bin/bits>>,
    #dbus_header{endian = $l} = Header
) ->
    unmarshal_header_fields(Bin, Header#dbus_header{size = Length, serial = Serial});
unmarshal_header2(
    <<Length:4/unsigned-big-integer-unit:8, Serial:4/unsigned-big-integer-unit:8, Bin/bits>>,
    #dbus_header{endian = $B} = Header
) ->
    unmarshal_header_fields(Bin, Header#dbus_header{size = Length, serial = Serial}).

unmarshal_header_fields(Bin, #dbus_header{endian = Endian, size = Size} = Header) ->
    case unmarshal({array, {struct, [byte, variant]}}, Bin, 12, Endian) of
        more ->
            more;
        {ok, [_, _, _, ?DBUS_VERSION_MAJOR, Size, _, _], Rest, _} when byte_size(Rest) < Size ->
            more;
        {ok, Fields, Rest, Pos} ->
            Pad = pad(8, Pos),
            case byte_size(Rest) < Pad / 8 + Size of
                true ->
                    more;
                false ->
                    <<0:Pad, Body:Size/binary, Rest2/binary>> = Rest,
                    {ok, Header#dbus_header{fields = Fields}, Body, Rest2}
            end
    end.

%% The signature of a variant, which the specification restricts to a single
%% complete type. Only `unmarshal/4' calls this, off a signature already read
%% from the wire, so a bad one raises and `unmarshal_data/1' turns it into
%% `{error, _}' like any other malformed body.
unmarshal_single_type(<<>>) ->
    empty;
unmarshal_single_type(Bin) when is_binary(Bin) ->
    case parse_type(Bin, 0, 0) of
        {ok, Type, <<>>} -> {ok, Type};
        {ok, _Type, _Rest} -> error({unmarshaling, signature, Bin});
        more -> more;
        {error, Err} -> error(Err)
    end.

unmarshal(_, <<>>, _, _) ->
    more;
unmarshal(byte, Data, Pos, _) ->
    <<Value:8, Data1/binary>> = Data,
    {ok, Value, Data1, Pos + 1};
unmarshal(boolean, Data, Pos, Endian) ->
    case unmarshal(uint32, Data, Pos, Endian) of
        more -> more;
        {ok, 1, Data1, Pos1} -> {ok, true, Data1, Pos1};
        {ok, 0, Data1, Pos1} -> {ok, false, Data1, Pos1};
        {ok, _, _, _} -> error({unmarshaling, boolean, Data})
    end;
unmarshal(uint16, Data, Pos, Endian) ->
    unmarshal_uint(2, Data, Pos, Endian);
unmarshal(uint32, Data, Pos, Endian) ->
    unmarshal_uint(4, Data, Pos, Endian);
unmarshal(uint64, Data, Pos, Endian) ->
    unmarshal_uint(8, Data, Pos, Endian);
unmarshal(int16, Data, Pos, Endian) ->
    unmarshal_int(2, Data, Pos, Endian);
unmarshal(int32, Data, Pos, Endian) ->
    unmarshal_int(4, Data, Pos, Endian);
unmarshal(int64, Data, Pos, Endian) ->
    unmarshal_int(8, Data, Pos, Endian);
unmarshal(double, Data, _, _) when byte_size(Data) < 8 ->
    more;
unmarshal(double, Data, Pos, Endian) ->
    Pad = pad(8, Pos),
    {Value, Data1} =
        case Endian of
            $l ->
                <<0:Pad, V:64/little-float, D/binary>> = Data,
                {V, D};
            $B ->
                <<0:Pad, V:64/big-float, D/binary>> = Data,
                {V, D}
        end,
    Pos1 = Pos + Pad div 8 + 8,
    {ok, Value, Data1, Pos1};
unmarshal(signature, Data, Pos, Endian) ->
    unmarshal_string(byte, Data, Pos, Endian);
unmarshal(string, Data, Pos, Endian) ->
    unmarshal_string(uint32, Data, Pos, Endian);
unmarshal(object_path, Data, Pos, Endian) ->
    unmarshal_string(uint32, Data, Pos, Endian);
unmarshal({array, SubType}, Data, Pos, Endian) ->
    case unmarshal(uint32, Data, Pos, Endian) of
        more ->
            more;
        {ok, Length, Rest, NewPos} ->
            unmarshal_array(SubType, Length, Rest, NewPos, Endian)
    end;
unmarshal({struct, _}, Data, _, _) when byte_size(Data) < 8 ->
    more;
unmarshal({struct, SubTypes}, Data, Pos, Endian) ->
    Pad = pad(8, Pos),
    <<0:Pad, Data1/binary>> = Data,
    Pos1 = Pos + Pad div 8,
    case unmarshal_struct(SubTypes, Data1, Pos1, Endian) of
        more ->
            more;
        {ok, Res, Data2, Pos2} ->
            {ok, list_to_tuple(Res), Data2, Pos2}
    end;
unmarshal({dict, KeyType, ValueType}, Data, Pos, Endian) ->
    case unmarshal(uint32, Data, Pos, Endian) of
        more ->
            more;
        {ok, Length, Data1, Pos1} ->
            case unmarshal_dict(KeyType, ValueType, Length, Data1, Pos1, Endian) of
                more ->
                    more;
                {ok, Res, Data2, Pos2} ->
                    {ok, Res, Data2, Pos2}
            end
    end;
unmarshal(variant, Data, Pos, Endian) ->
    case unmarshal(signature, Data, Pos, Endian) of
        more ->
            more;
        {ok, _, <<>>, _} ->
            more;
        {ok, Signature, Data1, Pos1} ->
            case unmarshal_single_type(Signature) of
                more ->
                    more;
                {ok, Type} ->
                    case unmarshal(Type, Data1, Pos1, Endian) of
                        more ->
                            more;
                        {ok, Value, Data2, Pos2} ->
                            {ok, Value, Data2, Pos2}
                    end
            end
    end.

unmarshal_uint(Len, Data, _, _) when is_integer(Len) andalso byte_size(Data) < Len ->
    more;
unmarshal_uint(Len, Data, Pos, Endian) when is_integer(Len) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    {Value, Data1} =
        case Endian of
            $l ->
                <<0:Pad, V:Bitlen/little-unsigned, D/binary>> = Data,
                {V, D};
            $B ->
                <<0:Pad, V:Bitlen/big-unsigned, D/binary>> = Data,
                {V, D}
        end,
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.

unmarshal_int(Len, Data, _, _) when is_integer(Len) andalso byte_size(Data) < Len ->
    more;
unmarshal_int(Len, Data, Pos, Endian) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    {Value, Data1} =
        case Endian of
            $l ->
                <<0:Pad, V:Bitlen/little-signed, D/binary>> = Data,
                {V, D};
            $B ->
                <<0:Pad, V:Bitlen/big-signed, D/binary>> = Data,
                {V, D}
        end,
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.

%%%
%%% Signature parsing
%%%
%%% The signature language of the specification's "Valid Signatures" section,
%%% parsed by descent. Three properties are the point of it:
%%%
%%% - it is total. The bytes come from a peer, so anything the language
%%%   forbids is `{error, _}' and a well-formed signature cut short is `more';
%%%   neither raises.
%%% - it accepts nothing the encoder cannot produce. That is what excludes the
%%%   `r' and `e' type codes -- reserved for STRUCT and DICT_ENTRY inside an
%%%   implementation, and forbidden in a signature, where the brackets are used
%%%   instead -- along with unbalanced brackets, empty structs, dict entries
%%%   outside an array and dict entries with a container key.
%%% - depth is counted down the descent and checked with `check_depth/2', the
%%%   function the encoder walks a type term with, so both directions cap
%%%   nesting at the same place.
%%%
%%% `ADepth' counts array type codes, `PDepth' open parentheses and open curly
%%% brackets.

%% A list of single complete types, to the end of the input. A closing bracket
%% reaching here has no opening one -- a container consumes its own -- and
%% `parse_type/3' rejects it as unbalanced.
parse_types(<<>>, _ADepth, _PDepth, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
parse_types(Bin, ADepth, PDepth, Acc) ->
    case parse_type(Bin, ADepth, PDepth) of
        {ok, Type, Rest} -> parse_types(Rest, ADepth, PDepth, [Type | Acc]);
        more -> more;
        {error, _} = Err -> Err
    end.

%% One single complete type. The depth a container adds is charged before its
%% contents are parsed, so a signature that is too deep is rejected as such
%% rather than by whatever it nests.
parse_type(<<>>, _ADepth, _PDepth) ->
    more;
parse_type(<<$a, ${, Rest/bits>>, ADepth, PDepth) ->
    %% A dict entry is only ever an array element, so this is the one place it
    %% can appear; `${' anywhere else is rejected below.
    case check_depth(ADepth + 1, PDepth + 1) of
        ok -> parse_dict(Rest, ADepth, PDepth);
        {error, _} = Err -> Err
    end;
parse_type(<<$a, Rest/bits>>, ADepth, PDepth) ->
    case check_depth(ADepth + 1, PDepth) of
        ok -> parse_array(Rest, ADepth, PDepth);
        {error, _} = Err -> Err
    end;
parse_type(<<$(, Rest/bits>>, ADepth, PDepth) ->
    case check_depth(ADepth, PDepth + 1) of
        ok -> parse_struct(Rest, ADepth, PDepth, []);
        {error, _} = Err -> Err
    end;
parse_type(<<$), _/bits>>, _ADepth, _PDepth) ->
    {error, {bad_signature, {unbalanced, $)}}};
parse_type(<<$}, _/bits>>, _ADepth, _PDepth) ->
    {error, {bad_signature, {unbalanced, $}}}};
parse_type(<<${, _/bits>>, _ADepth, _PDepth) ->
    {error, {bad_signature, dict_entry_outside_array}};
parse_type(<<C, Rest/bits>>, _ADepth, _PDepth) ->
    case type_code(C) of
        {ok, Type} -> {ok, Type, Rest};
        error -> {error, {bad_signature, {bad_type_code, C}}}
    end.

%% The element type of an array: exactly one single complete type. Parsing the
%% rest of the signature here instead is what made `aav' decode to
%% `{array, [{array, variant}]}', an element type that is a list.
parse_array(Bin, ADepth, PDepth) ->
    case parse_type(Bin, ADepth + 1, PDepth) of
        {ok, Type, Rest} -> {ok, {array, Type}, Rest};
        more -> more;
        {error, _} = Err -> Err
    end.

%% The body of a struct: one or more single complete types, then `)'.
parse_struct(<<>>, _ADepth, _PDepth, _Acc) ->
    more;
parse_struct(<<$), _Rest/bits>>, _ADepth, _PDepth, []) ->
    {error, {bad_signature, empty_struct}};
parse_struct(<<$), Rest/bits>>, _ADepth, _PDepth, Acc) ->
    {ok, {struct, lists:reverse(Acc)}, Rest};
parse_struct(<<$}, _/bits>>, _ADepth, _PDepth, _Acc) ->
    {error, {bad_signature, {unbalanced, $}}}};
parse_struct(Bin, ADepth, PDepth, Acc) ->
    case parse_type(Bin, ADepth, PDepth + 1) of
        {ok, Type, Rest} -> parse_struct(Rest, ADepth, PDepth, [Type | Acc]);
        more -> more;
        {error, _} = Err -> Err
    end.

%% The body of a dict entry, `a{' already consumed: a basic key, one value
%% type, then `}'. The key may not be a container, and there may not be a
%% third field.
parse_dict(<<>>, _ADepth, _PDepth) ->
    more;
parse_dict(<<C, Rest/bits>>, ADepth, PDepth) ->
    case basic_type_code(C) of
        {ok, KeyType} -> parse_dict_value(Rest, KeyType, ADepth, PDepth);
        error -> {error, {bad_signature, {bad_dict_key, C}}}
    end.

parse_dict_value(Bin, KeyType, ADepth, PDepth) ->
    case parse_type(Bin, ADepth + 1, PDepth + 1) of
        {ok, ValueType, Rest} -> parse_dict_end(Rest, KeyType, ValueType);
        more -> more;
        {error, _} = Err -> Err
    end.

parse_dict_end(<<>>, _KeyType, _ValueType) ->
    more;
parse_dict_end(<<$}, Rest/bits>>, KeyType, ValueType) ->
    {ok, {dict, KeyType, ValueType}, Rest};
parse_dict_end(<<C, _/bits>>, _KeyType, _ValueType) ->
    {error, {bad_signature, {dict_entry_arity, C}}}.

%% The type codes that stand for a complete type on their own. `a', `(' and
%% `{' are containers and handled above; `r' and `e' are not signature type
%% codes at all.
type_code($y) -> {ok, byte};
type_code($b) -> {ok, boolean};
type_code($n) -> {ok, int16};
type_code($q) -> {ok, uint16};
type_code($i) -> {ok, int32};
type_code($u) -> {ok, uint32};
type_code($x) -> {ok, int64};
type_code($t) -> {ok, uint64};
type_code($d) -> {ok, double};
type_code($s) -> {ok, string};
type_code($o) -> {ok, object_path};
type_code($g) -> {ok, signature};
type_code($v) -> {ok, variant};
type_code(_C) -> error.

%% A dict key must be basic, which among the codes above means anything but a
%% variant.
basic_type_code($v) -> error;
basic_type_code(C) -> type_code(C).

unmarshal_struct(SubTypes, Data, Pos, Endian) ->
    unmarshal_struct(SubTypes, Data, [], Pos, Endian).

unmarshal_struct([], Data, Acc, Pos, _) ->
    {ok, lists:reverse(Acc), Data, Pos};
unmarshal_struct([SubType | S], Data, Acc, Pos, Endian) ->
    case unmarshal(SubType, Data, Pos, Endian) of
        more -> more;
        {ok, Value, Data1, Pos1} -> unmarshal_struct(S, Data1, [Value | Acc], Pos1, Endian)
    end.

unmarshal_dict(KeyType, ValueType, Length, Data, Pos, Endian) ->
    SubType = {struct, [KeyType, ValueType]},
    Pad = pad(padding(SubType), Pos),
    case byte_size(Data) < Pad / 8 of
        true ->
            more;
        false ->
            <<0:Pad, Rest/binary>> = Data,
            NewPos = Pos + Pad div 8,
            unmarshal_dict(KeyType, ValueType, Length, Rest, #{}, NewPos, Endian)
    end.

unmarshal_dict(_KeyType, _ValueType, 0, Data, Acc, Pos, _) ->
    {ok, Acc, Data, Pos};
unmarshal_dict(KeyType, ValueType, Length, Data, Acc, Pos, Endian) when
    is_integer(Length), Length > 0
->
    SubType = {struct, [KeyType, ValueType]},
    case unmarshal(SubType, Data, Pos, Endian) of
        more ->
            more;
        {ok, {Key, Value}, Data1, Pos1} ->
            Size = Pos1 - Pos,
            unmarshal_dict(
                KeyType, ValueType, Length - Size, Data1, Acc#{Key => Value}, Pos1, Endian
            )
    end.

unmarshal_array(SubType, Length, Data, Pos, Endian) ->
    Pad = pad(padding(SubType), Pos),
    case byte_size(Data) < Pad / 8 of
        true ->
            more;
        false ->
            <<0:Pad, Rest/binary>> = Data,
            NewPos = Pos + Pad div 8,
            unmarshal_array(SubType, Length, Rest, [], NewPos, Endian)
    end.

unmarshal_array(_SubType, 0, Data, Acc, Pos, _) ->
    {ok, lists:reverse(Acc), Data, Pos};
unmarshal_array(SubType, Length, Data, Acc, Pos, Endian) when is_integer(Length), Length > 0 ->
    case unmarshal(SubType, Data, Pos, Endian) of
        more ->
            more;
        {ok, Value, Data1, Pos1} ->
            Size = Pos1 - Pos,
            unmarshal_array(SubType, Length - Size, Data1, [Value | Acc], Pos1, Endian)
    end.

unmarshal_tuple(Types, Data, Endian) when is_list(Types), is_binary(Data) ->
    unmarshal_tuple(Types, Data, [], 0, Endian).

unmarshal_tuple([], Rest, Acc, Pos, _) ->
    {ok, list_to_tuple(lists:reverse(Acc)), Rest, Pos};
unmarshal_tuple([Type | T], Data, Acc, Pos, Endian) when byte_size(Data) > 0 ->
    case unmarshal(Type, Data, Pos, Endian) of
        more ->
            more;
        {ok, Value, Rest, Pos1} ->
            unmarshal_tuple(T, Rest, [Value | Acc], Pos1, Endian)
    end.

unmarshal_string(LenType, Data, Pos, Endian) ->
    case unmarshal(LenType, Data, Pos, Endian) of
        more ->
            more;
        %% `Length' counts the characters; the NUL terminator is on the wire
        %% too, so `Length' bytes present is still a truncated string.
        {ok, Length, Data1, _} when byte_size(Data1) =< Length ->
            more;
        {ok, Length, Data1, Pos1} ->
            <<String:Length/binary, 0, Data2/binary>> = Data1,
            Pos2 = Pos1 + Length + 1,
            {ok, String, Data2, Pos2}
    end.

%%%
%%% Priv common
%%%
padding(byte) -> 1;
padding(boolean) -> 4;
padding(int16) -> 2;
padding(uint16) -> 2;
padding(int32) -> 4;
padding(uint32) -> 4;
padding(int64) -> 8;
padding(uint64) -> 8;
padding(double) -> 8;
padding(string) -> 4;
padding(object_path) -> 4;
padding(signature) -> 1;
padding({array, _Type}) -> 4;
padding({struct, _Types}) -> 8;
padding(variant) -> 1;
padding(dict) -> 4.

-spec pad(Size :: dbus_type() | integer(), MessagePos :: integer()) ->
    PaddingBits :: integer().
% Size: the size of the binary alignment in bytes
% Pos: the length of the formatted message in bytes
%
% Pos rem Size gives how many bytes beyond padding boundary
% the current data sits.
% (Size - (Pos rem Size)) gives the number of bytes of
% padding except in the case where Pos rem Size is 0,
% which will yeild Size instead of 0.
% There are several ways of dealing with this case, the
% method chosen here is to do another rem.
% Finally, the padding should be represented in bits (not
% bytes) so multiply by 8.
pad(Size, Pos) when is_integer(Size) ->
    ((Size - (Pos rem Size)) rem Size) * 8;
pad(Type, Pos) when
    is_atom(Type);
    array =:= element(1, Type);
    struct =:= element(1, Type)
->
    pad(padding(Type), Pos).
