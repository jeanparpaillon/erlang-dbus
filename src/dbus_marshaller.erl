%%
%% @copyright 2006-2007 Mikael Magnusson, 2014-2106 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc D-Bus binary format (un)marshaling.
%%
%% See <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-marshaling" >D-Bus Specification</a>.
%%
%% @end
-module(dbus_marshaller).

-include("dbus.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([
         marshal_message/1,
         marshal_signature/1,
         marshal_list/2,
         unmarshal_data/1,
         unmarshal_signature/1
        ]).

-define(HEADER_SIGNATURE, [byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}]).

-type errors() :: invalid_serial
		| {marshaling, dbus_type(), binary()}
		| {unmarshaling, dbus_type(), binary()}
		| {dbus_parse_error, term()}
		| {bad_type_code, integer()}
		| dbus_parse_error
		| body_parse_error
		| bad_header
		| term().

-export_type([errors/0]).

%%%
%%% API
%%%

%% @doc Encode a message
%% @end
-spec marshal_message(dbus_message()) -> iolist().
marshal_message(#dbus_message{header=#dbus_header{serial=0}}=_Msg) ->
    throw(invalid_serial);

marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields}, 
                              body= <<>>}=_Msg) ->
    marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, 0, S, Fields]);

marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields, size=Size}, 
                              body=Body}=_Msg) ->
    [ marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, Size, S, Fields]), Body ].


%% @doc Encode a signature
%% @end
-spec marshal_signature(dbus_signature()) -> iolist().
marshal_signature(byte)        ->   "y";
marshal_signature(boolean)     ->   "b";
marshal_signature(int16)       ->   "n";
marshal_signature(uint16)      ->   "q";
marshal_signature(int32)       ->   "i";
marshal_signature(uint32)      ->   "u";
marshal_signature(int64)       ->   "x";
marshal_signature(uint64)      ->   "t";
marshal_signature(double)      ->   "d";
marshal_signature(string)      ->   "s";
marshal_signature(object_path) ->   "o";
marshal_signature(signature)   ->   "g";
marshal_signature({array, Type}) ->
    [$a, marshal_signature(Type)];

marshal_signature({struct, SubTypes}) ->
    ["(", marshal_struct_signature(SubTypes, []), ")"];

marshal_signature(variant) ->
    "v";

marshal_signature({dict, KeyType, ValueType}) ->
    KeySig = marshal_signature(KeyType),
    ValueSig = marshal_signature(ValueType),
    ["a{", KeySig, ValueSig, "}"];

marshal_signature([]) ->
    "";

marshal_signature([Type|R]) ->
    [marshal_signature(Type), marshal_signature(R)].


%% @doc Encode objects, given a signature
%% @end
-spec marshal_list(dbus_signature(), term()) -> {iolist(), integer()}.
marshal_list(Types, Value) ->
    marshal_list(Types, Value, 0, []).


%% @doc Decode messages
%% 
%% Returns:
%% * `{ok, [dbus_message()], binary()}': if binary describe a complete list of messages, eventually with remaining binary.
%% * `more': if no complete message could be decoded.
%% @end
-spec unmarshal_data(binary()) -> {ok, Msgs :: [dbus_message()], Rest :: binary()} 
				      | {error, errors()}
				      | more.
unmarshal_data(Data) ->
    try unmarshal_data(Data, []) 
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Decode a signature
%%
%% Returns `more' if no complete signature could be decoded.
%% @end
-spec unmarshal_signature(binary()) -> {ok, dbus_signature()} | more.
unmarshal_signature(<<>>) -> 
    {ok, []};

unmarshal_signature(Bin) when is_binary(Bin) ->
    case unmarshal_signature(Bin, []) of
        {ok, Signature, <<>>} -> {ok, Signature};
        more -> more
    end.

%%%
%%% Priv marshalling
%%%
marshal_header(Header) when is_list(Header) ->
    {Value, Pos} = marshal_list(?HEADER_SIGNATURE, Header),
    case pad(8, Pos) of
        0 -> Value;
        Pad -> [Value, <<0:Pad>>]
    end.


marshal_list([], [], Pos, Res) ->
    {Res, Pos};
marshal_list([Type | T], [Value | V], Pos, Res) ->
    {Res1, Pos1} = marshal(Type, Value, Pos),
    marshal_list(T, V, Pos1, [Res, Res1]).

marshal(byte, Value, Pos) when is_integer(Value) andalso 255 >= Value ->
    marshal_uint(1, Value, Pos);

marshal(boolean, Value, Pos) when true =:= Value orelse false =:= Value ->
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

marshal(int32, Value, Pos) when Value >= -2147483647 andalso Value =< 2147483647->
    marshal_int(4, Value, Pos);

marshal(uint32, Value, Pos) when Value >= 0 andalso Value =< 4294967295 ->
    marshal_uint(4, Value, Pos);

marshal(int64, Value, Pos) ->
    marshal_int(8, Value, Pos);

marshal(uint64, Value, Pos) when Value >= 0 ->
    marshal_uint(8, Value, Pos);


marshal(double, Value, Pos) when is_integer(Value) ->
    Pad = pad(8, Pos),
    {<< 0:Pad, (float(Value)):64/little-float >>, Pos + Pad div 8+ 8};

marshal(double, Value, Pos) when is_float(Value) ->
    Pad = pad(8, Pos),
    {<< 0:Pad, Value:64/little-float >>, Pos + Pad div 8+ 8};

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

marshal({array, {struct, [_KeyType, _ValueType]}=SubType}, Value, Pos) when is_map(Value) ->
    marshal_array(SubType, maps:to_list(Value), Pos);

marshal({array, byte}=Type, Value, Pos) when is_binary(Value) ->
    marshal(Type, binary_to_list(Value), Pos);

marshal({array, SubType}, Value, Pos) when is_list(Value) ->
    marshal_array(SubType, Value, Pos);

marshal({struct, _SubTypes}=Type, Value, Pos) when is_tuple(Value) ->
    marshal(Type, tuple_to_list(Value), Pos);

marshal({struct, SubTypes}, Value, Pos) when is_list(Value) ->
    marshal_struct(SubTypes, Value, Pos);

marshal({dict, KeyType, ValueType}, Value, Pos) ->
    marshal_dict(KeyType, ValueType, Value, Pos);

marshal(variant, Value, Pos) when is_binary(Value) ->
    marshal_variant({array, byte}, Value, Pos);

marshal(variant, #dbus_variant{type=Type, value=Value}, Pos) ->
    marshal_variant(Type, Value, Pos);

marshal(variant, true=Value, Pos) ->
    marshal_variant(boolean, Value, Pos);

marshal(variant, false=Value, Pos) ->
    marshal_variant(boolean, Value, Pos);

marshal(variant, Value, Pos) when is_float(Value) ->
    marshal_variant(double, Value, Pos);

marshal(variant, Value, Pos) when is_integer(Value), Value < 0 ->
    marshal_int_variant(Value, Pos);

marshal(variant, Value, Pos) when is_integer(Value), Value >= 0 ->
    marshal_uint_variant(Value, Pos);

marshal(variant, Value, Pos) when is_list(Value) ->
    marshal(variant, list_to_binary(Value), Pos);

marshal(variant, Value, Pos) when is_atom(Value) ->
    marshal_variant(string, atom_to_binary(Value, utf8), Pos);

marshal(variant, Value, Pos) ->
    Type = infer_type(Value),
    marshal_variant(Type, Value, Pos);

marshal(Type, Value, _) ->
    throw({marshaling, Type, Value}).


infer_type(Value) when is_binary(Value)->
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
infer_type(Value) when is_atom(Value)->
    string;
infer_type(Value) when is_list(Value) ->
    string;
infer_type(Value) when is_map(Value) ->
    infer_dict(Value).


infer_struct(Values) ->
    {struct, infer_struct(Values, [])}.

infer_struct([], Res) ->
    lists:reverse(Res);
infer_struct([ Value | R ], Res) ->
    infer_struct(R, [ infer_type(Value) | Res ]).

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
    {<< 0:Pad, Value:(Len*8)/little-unsigned >>, Pos + Pad div 8 + Len}.

marshal_int(Len, Value, Pos) when is_integer(Value) ->
    Pad = pad(Len, Pos),
    {<< 0:Pad, Value:(Len*8)/little-signed >>, Pos + Pad div 8 + Len}.


marshal_string(LenType, Value, Pos) when is_list(Value) ->
    marshal_string(LenType, list_to_binary(Value), Pos);

marshal_string(LenType, Value, Pos) when is_binary(Value) ->
    Length = byte_size(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    {[Value1, Value, 0], Pos1 + Length + 1}.


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
marshal_array_item(SubType, [ Value | R ], Pos, Res) ->
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_array_item(SubType, R, Pos1, [Res, Value1]).


marshal_dict(KeyType, ValueType, Value, Pos) when is_map(Value) ->
    marshal_array({struct, [KeyType, ValueType]}, maps:to_list(Value), Pos);

marshal_dict(KeyType, ValueType, Value, Pos) when is_list(Value) ->
    marshal_array({struct, [KeyType, ValueType]}, Value, Pos).


marshal_struct(SubTypes, Values, Pos) ->
    Pad = pad(8, Pos),
    {Values1, Pos1} = marshal_struct(SubTypes, Values, Pos + Pad div 8, []),
    if
        Pad == 0 ->
            {Values1, Pos1};
        Pad > 0 ->
            {[<< 0:Pad >>, Values1], Pos1}
    end.

marshal_struct([], [], Pos, Res) ->
    {Res, Pos};
marshal_struct([SubType|R], [Value|V], Pos, Res) ->
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_struct(R, V, Pos1, [Res, Value1]).



marshal_struct_signature([], Res) ->
    Res;
marshal_struct_signature([SubType|R], Res) ->
    marshal_struct_signature(R, [Res, marshal_signature(SubType)]).

%%%
%%% Private unmarshaling
%%%
unmarshal_data(<<>>, []) ->
    more;

unmarshal_data(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};

unmarshal_data(Data, Acc) ->
    try unmarshal_message(Data) of
        {ok, #dbus_message{}=Msg, Rest} -> 
            unmarshal_data(Rest, [Msg | Acc]);
        more when [] =:= Acc ->
            more;
        more ->
            {ok, lists:reverse(Acc), Data};
        _ ->
            ?error("Error parsing data~n", []),
            throw(dbus_parse_error)
    catch
        {'EXIT', Err} -> 
            throw({dbus_parse_error, Err})
    end.


unmarshal_message(<<>>) ->
    more;

unmarshal_message(Data) when is_binary(Data) ->
    case unmarshal_header(Data) of
        more -> 
            more;
        {ok, #dbus_header{endian=Endian, type=MsgType}=Header, BodyBin, Rest} ->
            case dbus_message:find_field(?FIELD_SIGNATURE, Header) of
                undefined ->
                    case BodyBin of
                        <<>> -> {ok, #dbus_message{header=Header, body=undefined}, Rest};
                        _ -> throw(body_parse_error)
                    end;
                Signature ->
                    case unmarshal_body(MsgType, Signature, BodyBin, Endian) of
                        {ok, Body} -> {ok, #dbus_message{header=Header, body=Body}, Rest};
                        more -> more;
                        {error, Err} -> throw(Err)
                    end
            end
    end.


unmarshal_body(?TYPE_INVALID, _, _, _) ->
    {ok, undefined};

unmarshal_body(_, SigBin, BodyBin, Endian) ->
    case unmarshal_signature(SigBin) of
        {ok, Sig} ->
            case unmarshal_tuple(Sig, BodyBin, Endian) of
                more -> more;
                {ok, {}, <<>>, _Pos} -> 
                    {ok, undefined};
                {ok, {Body}, <<>>, _Pos} -> 
                    {ok, Body};
                {ok, Body, <<>>, _Pos} -> 
                    {ok, Body};
                {ok, _Body, _, _} -> {error, body_parse_error}
            end;
        more -> more
    end.


unmarshal_header(Bin) when byte_size(Bin) < 16 ->
    more;
unmarshal_header(<<Endian/integer, Type/integer, Flags/integer, ?DBUS_VERSION_MAJOR, Rest/bits>>) ->
    unmarshal_header2(Rest, #dbus_header{endian=Endian, type=Type, flags=Flags});
unmarshal_header(_Data) ->
    ?debug("Bad message header: ~p~n", [_Data]),
    throw(bad_header).

unmarshal_header2(<<Length:4/unsigned-little-integer-unit:8, Serial:4/unsigned-little-integer-unit:8, Bin/bits>>,
                  #dbus_header{endian=$l}=Header) ->
    unmarshal_header_fields(Bin, Header#dbus_header{size=Length, serial=Serial});
unmarshal_header2(<<Length:4/unsigned-big-integer-unit:8, Serial:4/unsigned-big-integer-unit:8, Bin/bits>>,
                  #dbus_header{endian=$B}=Header) ->
    unmarshal_header_fields(Bin, Header#dbus_header{size=Length, serial=Serial}).

unmarshal_header_fields(Bin, #dbus_header{endian=Endian, size=Size}=Header) ->
    case unmarshal({array, {struct, [byte, variant]}}, Bin, 12, Endian) of
        more -> 
            more;
        {ok, [_, _, _, ?DBUS_VERSION_MAJOR, Size, _, _], Rest, _} when byte_size(Rest) < Size  ->
            more;
        {ok, Fields, Rest, Pos} ->
            Pad = pad(8, Pos),
            if
                byte_size(Rest) < Pad/8 + Size ->
                    more;
                true ->
                    <<0:Pad, Body:Size/binary, Rest2/binary>> = Rest,
                    {ok, Header#dbus_header{fields=Fields}, Body, Rest2}
            end
    end.


unmarshal_single_type(<<>>) ->
    empty;

unmarshal_single_type(Bin) when is_binary(Bin) ->
    case unmarshal_signature(Bin, []) of
        {ok, [Type], <<>>} -> {ok, Type};
	{ok, _, _} -> throw({unmarshaling, signature, Bin});
        more -> more
    end.


unmarshal(_, <<>>, _, _) ->
    more;

unmarshal(byte, Data, Pos, _) ->
    << Value:8, Data1/binary >> = Data,
    {ok, Value, Data1, Pos + 1};

unmarshal(boolean, Data, Pos, Endian) ->
    case unmarshal(uint32, Data, Pos, Endian) of
        more -> more;
        {ok, 1, Data1, Pos1} ->
            {ok, true, Data1, Pos1};
        {ok, 0, Data1, Pos1} ->
            {ok, false, Data1, Pos1};
        {ok, _, _, _} ->
            throw({unmarshaling, boolean, Data})
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
    {Value, Data1} = case Endian of
                         $l ->
                             << 0:Pad, V:64/little-float, D/binary >> = Data,
                             {V, D};
                         $B ->
                             << 0:Pad, V:64/big-float, D/binary >> = Data,
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
    << 0:Pad, Data1/binary >> = Data,
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
                more -> more;
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
    {Value, Data1} = case Endian of
                         $l ->
                             << 0:Pad, V:Bitlen/little-unsigned, D/binary >> = Data,
                             {V, D};
                         $B ->
                             << 0:Pad, V:Bitlen/big-unsigned, D/binary >> = Data,
                             {V, D}
                     end,                            
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.


unmarshal_int(Len, Data, _, _) when is_integer(Len) andalso byte_size(Data) < Len ->
    more;

unmarshal_int(Len, Data, Pos, Endian) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    {Value, Data1} = case Endian of
                         $l ->
                             << 0:Pad, V:Bitlen/little-signed, D/binary >> = Data,
                             {V, D};
                         $B ->
                             << 0:Pad, V:Bitlen/big-signed, D/binary >> = Data,
                             {V, D}
                     end,
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.


unmarshal_signature(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};

unmarshal_signature(<<$a, ${, KeySig, Rest/bits>>, Acc) ->
    KeyType = unmarshal_type_code(KeySig),
    case unmarshal_signature(Rest, []) of
        {ok, [], _} -> 
            more;
        {ok, [ValueType], Rest2} ->
            unmarshal_signature(Rest2, [ {dict, KeyType, ValueType} | Acc ]);
        {ok, _, _} ->
	    throw({unmarshaling, dict, KeySig, Rest});
        more -> 
            more
    end;

unmarshal_signature(<<$a, Rest/bits>>, Acc) ->
    case unmarshal_array_signature(Rest) of
        {ok, Type, Rest2} ->
	    unmarshal_signature(Rest2, [ {array, Type} | Acc ]);
        more -> more
    end;

unmarshal_signature(<<$(, Rest/bits>>, Acc) ->
    case unmarshal_signature(Rest, []) of
        {ok, [], _} -> more;
        {ok, Types, Rest2} ->
	    unmarshal_signature(Rest2, [ {struct, Types} | Acc ]);
        more -> more
    end;

unmarshal_signature(<<$), Rest/bits>>, Acc) ->
    {ok, lists:reverse(Acc), Rest};

unmarshal_signature(<<$}, Rest/bits>>, Acc) ->
    {ok, Acc, Rest};

unmarshal_signature(<<C, Rest/bits>>, Acc) ->
    Code = unmarshal_type_code(C),
    unmarshal_signature(Rest, [Code | Acc]).


unmarshal_array_signature(<<>>) ->
    more;

unmarshal_array_signature(<< $a, Rest/bits >>) ->
    unmarshal_signature(<< $a, Rest/bits >>, []);

unmarshal_array_signature(<< $(, Rest/bits >>) ->
    case unmarshal_signature(Rest, []) of
	{ok, [], _} -> 
	    more;
	{ok, Types, Rest2} ->
	    {ok, {struct, Types}, Rest2};
	more ->
	    more
    end;

unmarshal_array_signature(<< C, Rest/bits >>) ->
    Code = unmarshal_type_code(C),
    {ok, Code, Rest}.


unmarshal_type_code($y) -> byte;
unmarshal_type_code($b) -> boolean;
unmarshal_type_code($n) -> int16;
unmarshal_type_code($q) -> uint16;
unmarshal_type_code($i) -> int32;
unmarshal_type_code($u) -> uint32;
unmarshal_type_code($x) -> int64;
unmarshal_type_code($t) -> uint64;
unmarshal_type_code($d) -> double;
unmarshal_type_code($s) -> string;
unmarshal_type_code($o) -> object_path;
unmarshal_type_code($g) -> signature;
unmarshal_type_code($r) -> struct;
unmarshal_type_code($v) -> variant;
unmarshal_type_code($e) -> dict_entry;
unmarshal_type_code($a) -> array;
unmarshal_type_code(_C) -> throw({bad_type_code, _C}).


unmarshal_struct(SubTypes, Data, Pos, Endian) ->
    unmarshal_struct(SubTypes, Data, [], Pos, Endian).


unmarshal_struct([], Data, Acc, Pos, _) ->
    {ok, lists:reverse(Acc), Data, Pos};

unmarshal_struct([SubType | S], Data, Acc, Pos, Endian) ->
    case unmarshal(SubType, Data, Pos, Endian) of
        more -> more;
        {ok, Value, Data1, Pos1} ->
            unmarshal_struct(S, Data1, [Value | Acc], Pos1, Endian)
    end.


unmarshal_dict(KeyType, ValueType, Length, Data, Pos, Endian) ->
    SubType = {struct, [KeyType, ValueType]},
    Pad = pad(padding(SubType), Pos),
    if 
        byte_size(Data) < Pad / 8 -> 
            more;
        true ->
            << 0:Pad, Rest/binary >> = Data,
            NewPos = Pos + Pad div 8,
            unmarshal_dict(KeyType, ValueType, Length, Rest, #{}, NewPos, Endian)
    end.


unmarshal_dict(_KeyType, _ValueType, 0, Data, Acc, Pos, _) ->
    {ok, Acc, Data, Pos};

unmarshal_dict(KeyType, ValueType, Length, Data, Acc, Pos, Endian) when is_integer(Length), Length > 0 ->
    SubType = {struct, [KeyType, ValueType]},
    case unmarshal(SubType, Data, Pos, Endian) of
        more -> 
            more;
        {ok, {Key, Value}, Data1, Pos1} ->
            Size = Pos1 - Pos,
            unmarshal_dict(KeyType, ValueType, Length - Size, Data1, Acc#{ Key => Value }, Pos1, Endian)
    end.


unmarshal_array(SubType, Length, Data, Pos, Endian) ->
    Pad = pad(padding(SubType), Pos),
    if 
        byte_size(Data) < Pad / 8 -> 
            more;
        true ->
            << 0:Pad, Rest/binary >> = Data,
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


unmarshal_tuple(Type, Data, Endian) when is_atom(Type), is_binary(Data) ->
    unmarshal(Type, Data, 0, Endian);
unmarshal_tuple(Types, Data, Endian) when is_list(Types), is_binary(Data) ->
    unmarshal_tuple(Types, Data, [], 0, Endian).


unmarshal_tuple([], Rest, Acc, Pos, _) ->
    {ok, list_to_tuple(lists:reverse(Acc)), Rest, Pos};
unmarshal_tuple([Type|T], Data, Acc, Pos, Endian) ->
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
        {ok, Length, Data1, _} when byte_size(Data1) < Length ->
            more;
        {ok, Length, Data1, Pos1} ->
            << String:Length/binary, 0, Data2/binary >> = Data1,
            Pos2 = Pos1 + Length + 1,
            {ok, String, Data2, Pos2}
    end.

%%%
%%% Priv common
%%%
padding(byte)             -> 1;
padding(boolean)          -> 4;
padding(int16)            -> 2;
padding(uint16)           -> 2;
padding(int32)            -> 4;
padding(uint32)           -> 4;
padding(int64)            -> 8;
padding(uint64)           -> 8;
padding(double)           -> 8;
padding(string)           -> 4;
padding(object_path)      -> 4;
padding(signature)        -> 1;
padding({array, _Type})   -> 4;
padding({struct, _Types}) -> 8;
padding(variant)          -> 1;
padding(dict)             -> 4.

pad(Size, Pos) when is_integer(Size) ->
    ((Size - (Pos rem Size)) rem Size) * 8;
pad(Type, Pos) when is_atom(Type); 
                    array =:= element(1, Type);
                    struct =:= element(1, Type)->
    pad(padding(Type), Pos).

%%%
%%% eunit
%%%
-ifdef(TEST).
marshal_list_test() ->
    %%{Bin, Pos} = marshal_list([string,{array,{struct,[byte,string,variant]}},uint32,int32],
    %%			      [<<"http://schemas.ogf.org/occi/infrastructure#compute">>, [], 0, -1]),
    {Bin, Pos} = marshal_list([string, {array, {struct, [byte, string, variant]}}, uint32, int32],
			      [<<"http://schemas.ogf.org/occi/infrastructure#compute">>, [], 0, -1]),
    ?assertMatch({<<
		    50:8/little-unsigned-unit:4, "http://schemas.ogf.org/occi/infrastructure#compute", 0, 
		    0,                                                          %% string + padding
		    0:8/little-unsigned-unit:4, 0:8/little-unsigned-unit:4,     %% array length + padding (struct)
		    0:8/little-unsigned-unit:4,                                 %% 0 (unit32)
		    -1:8/little-signed-unit:4                                   %% -1 (int32
		  >>,
		  72},
		 {iolist_to_binary(Bin), Pos}).

marshall_byte_test_() ->
    [
     ?_assertMatch({<< 16#ff >>, 1}, marshal(byte, 16#ff, 0)),
     ?_assertThrow({marshaling, byte, 256}, marshal(byte, 256, 3))
    ].

marshall_boolean_test_() ->
    [
     ?_assertMatch({<< 1:8/integer-little-unit:4 >>, 4}, marshal(boolean, true, 0)),
     ?_assertMatch({<< 0:8/integer-little-unit:4 >>, 4}, marshal(boolean, false, 0)),
     ?_assertThrow({marshaling, boolean, else}, marshal(boolean, else, 0))
    ].

marshall_int_test_() ->
    [
     ?_assertMatch({<< 67:8/integer-little-signed-unit:2 >>, 2}, marshal(int16, 67, 0)),
     ?_assertMatch({<< -67:8/integer-little-signed-unit:2 >>, 2}, marshal(int16, -67, 0)),
     ?_assertThrow({marshaling, int16, 300000}, marshal(int16, 300000, 0)),

     ?_assertMatch({<< 67:8/integer-little-unsigned-unit:2 >>, 2}, marshal(uint16, 67, 0)),
     ?_assertThrow({marshaling, uint16, -67}, marshal(uint16, -67, 0)),

     ?_assertMatch({<< 2000000000:8/integer-little-signed-unit:4 >>, 4}, marshal(int32, 2000000000, 0)),
     ?_assertMatch({<< -2000000000:8/integer-little-signed-unit:4 >>, 4}, marshal(int32, -2000000000, 0)),
     ?_assertThrow({marshaling, int32, 3000000000}, marshal(int32, 3000000000, 0)),

     ?_assertMatch({<< 4000000:8/integer-little-unsigned-unit:4 >>, 4}, marshal(uint32, 4000000, 0)),
     ?_assertThrow({marshaling, uint32, -67}, marshal(uint32, -67, 0)),

     ?_assertMatch({<< 4000000000:8/integer-little-signed-unit:8 >>, 8}, marshal(int64, 4000000000, 0)),

     ?_assertMatch({<< 4000000000:8/integer-little-unsigned-unit:8 >>, 8}, marshal(uint64, 4000000000, 0)),
     ?_assertThrow({marshaling, uint64, -400000}, marshal(uint64, -400000, 0))
    ].

marshall_float_test_() ->
    [
     ?_assertMatch({<< 67:64/float-little-signed-unit:1 >>, 8}, marshal(double, 67, 0)),
     %% Tests alignement
     ?_assertMatch({<< 0:8/unit:6, 67:64/float-little-signed-unit:1 >>, 16}, marshal(double, 67, 2))
    ].

marshall_string_test_() ->
    [
     ?_assertMatch({[<< 9:8/integer-little-unsigned-unit:4 >>, <<"my string">>, 0 ], 14},
		   marshal(string, "my string", 0)),
     ?_assertMatch({[<< 9:8/integer-little-unsigned-unit:4 >>, <<"my string">>, 0 ], 14},
		   marshal(string, <<"my string">>, 0)),
     ?_assertMatch({[<< 7:8/integer-little-unsigned-unit:4 >>, <<"an_atom">>, 0 ], 12},
		   marshal(string, 'an_atom', 0)),

     ?_assertMatch({[<< 0:8/unit:2, 9:8/integer-little-unsigned-unit:4 >>, <<"my string">>, 0 ], 18},
		   marshal(string, "my string", 2))
    ].

marshall_object_path_test_() ->
    [
     ?_assertMatch({[<< 10:8/integer-little-unsigned-unit:4 >>, <<"/my/string">>, 0 ], 15},
		   marshal(object_path, <<"/my/string">>, 0))
    ].

marshall_signature_test_() ->
    [
     ?_assertMatch({[<< 6:8/integer-little-unsigned-unit:1 >>, <<"yasgoy">>, 0 ], 8},
		   marshal(signature, <<"yasgoy">>, 0))
    ].

marshall_array_test() ->
    {Io, Pad} = marshal({array, string}, ["un", "deux", "trois"], 0),
    ?assertMatch({<< 
		     30:8/integer-little-unsigned-unit:4,
		     2:8/integer-little-unsigned-unit:4, "un", 0, 0:8/unit:1, 
		     4:8/integer-little-unsigned-unit:4, "deux", 0, 0:8/unit:3,
		     5:8/integer-little-unsigned-unit:4, "trois", 0
		  >>, 34},
		 {iolist_to_binary(Io), Pad}),
    
    {Io2, Pad2} = marshal({array, string}, ["un", "deux", "trois"], 1),
    ?assertMatch({<< 
		     0:8/unit:3, 30:8/integer-little-unsigned-unit:4,
		     2:8/integer-little-unsigned-unit:4, "un", 0, 0:8/unit:1, 
		     4:8/integer-little-unsigned-unit:4, "deux", 0, 0:8/unit:3,
		     5:8/integer-little-unsigned-unit:4, "trois", 0
		  >>, 38},
		 {iolist_to_binary(Io2), Pad2}),

    {Io3, Pad3} = marshal({array, uint64}, [500, 245], 0),
    ?assertMatch({<< 
		     16:8/integer-little-unsigned-unit:4, 0:8/unit:4,
		     500:64/integer-little-unsigned-unit:1, 
		     245:64/integer-little-unsigned-unit:1
		  >>, 24},
		 {iolist_to_binary(Io3), Pad3}).

unmarshal_byte_test_() ->
    [
     ?_assertEqual({ok, 4, <<>>, 1}, unmarshal(byte, <<4>>, 0, $l))
    ,?_assertEqual({ok, 4, <<"xyz">>, 1}, unmarshal(byte, <<4, "xyz">>, 0, $l))
    ].

unmarshal_boolean_test_() ->
    [
     ?_assertEqual({ok, true, <<>>, 4}, unmarshal(boolean, <<1,0,0,0>>, 0, $l))
    ,?_assertEqual({ok, true, <<"xyz">>, 4}, unmarshal(boolean, <<1,0,0,0,"xyz">>, 0, $l))
    ,?_assertEqual({ok, false, <<>>, 4}, unmarshal(boolean, <<0,0,0,0>>, 0, $l))
    ,?_assertEqual(more, unmarshal(boolean, <<"x">>, 0, $l))
    ,?_assertThrow({unmarshaling, boolean, <<2, 0, 0, 0>>}, unmarshal(boolean, <<2,0,0,0>>, 0, $l))
    ].

unmarshal_endian_test_() ->
    [
     ?_assertEqual({ok, 1, <<>>, 4}, unmarshal(uint32, <<1,0,0,0>>, 0, $l))
    ,?_assertEqual({ok, 1, <<>>, 4}, unmarshal(uint32, <<0,0,0,1>>, 0, $B))
    ,?_assertEqual({ok, 1, <<"xyz">>, 4}, unmarshal(uint32, <<1,0,0,0, "xyz">>, 0, $l))
    ,?_assertEqual({ok, 1, <<"xyz">>, 4}, unmarshal(uint32, <<0,0,0,1, "xyz">>, 0, $B))
    ].

unmarshal_dict_test() ->
    Bin = << 
	     29:8/integer-little-unsigned-unit:4, 0:8/unit:4,
	     $a, 0:8/unit:3,
	     4:8/integer-little-unsigned-unit:4, "plop", 0, 0:8/unit:3,
	     $b, 0:8/unit:3,
	     4:8/integer-little-unsigned-unit:4, "truc", 0
	  >>,
    ?assertMatch({ok, #{ $a := <<"plop">>, $b := <<"truc">> }, <<>>, 37}, 
		 unmarshal({dict, byte, string}, Bin, 0, $l)),
    
    ?assertMatch({ok, [ {$a, <<"plop">>}, {$b, <<"truc">>} ], <<>>, 37}, 
		 unmarshal({array, {struct, [byte, string]}}, Bin, 0, $l)),
    
    DictVariant = <<
		    5:8/integer-little-unsigned-unit:1, "a{ys}", 0, 0:8/unit:1,
		    Bin/binary
		  >>,
    ?assertMatch({ok, #{ $a := <<"plop">>, $b := <<"truc">> }, <<>>, 45}, 
		 unmarshal(variant, DictVariant, 0, $l)),
    
    ArrayVariant = <<
		     5:8/integer-little-unsigned-unit:1, "a(ys)", 0, 0:8/unit:1,
		     Bin/binary
		   >>,
    ?assertMatch({ok, [ {$a, <<"plop">>}, {$b, <<"truc">>} ], <<>>, 45}, 
		 unmarshal(variant, ArrayVariant, 0, $l)).
    

unmarshal_string_test_() ->
    Bin = <<
	    8:8/integer-little-unsigned-unit:4,
	    "a string", 0
	  >>,
    Variant = <<
		1, $s, 0, 0:8/unit:1, 
		Bin/binary
	      >>,
    [
     ?_assertMatch({ok, <<"a string">>, <<>>, 13},
		   unmarshal(string, Bin, 0, $l)),
     ?_assertMatch({ok, <<"a string">>, <<>>, 17},
		   unmarshal(variant, Variant, 0, $l))
    ].

unmarshal_signature_test() ->
    [
     ?_assertMatch([
		    {array, {array, {array, string}}}, byte
		   ], unmarshal_signature(<<"aaasy">>)),
     ?_assertMatch([
		    byte,
		    {dict, boolean, variant},
		    string,
		    string
		   ], unmarshal_signature(<<"ya{bv}ss">>)),
     ?_assertMatch([
		    {array, {struct, [string, string, {array, string}, {dict, string, variant}, string]}}, 
		    string
		   ], unmarshal_signature(<<"a(ssasa{sv}s)s">>))
    ].
-endif.
