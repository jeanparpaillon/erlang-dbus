%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc (un)marshalling
%%
-module(dbus_marshaller).
-compile([{parse_transform, lager_transform}]).

-include("dbus.hrl").

%% api
-export([
	 marshal_message/1,
	 marshal_signature/1,
	 marshal_list/2,
	 unmarshal_data/1,
	 unmarshal_signature/1
	]).

%%%
%%% API
%%%
-spec marshal_message(dbus_message()) -> iolist().
marshal_message(#dbus_message{header=#dbus_header{serial=0}}=_Msg) ->
    lager:debug("Invalid message: serial=0~n", []),
    throw({error, invalid_serial});
marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields}, body= <<>>}=_Msg) ->
    lager:debug("<1>Serializing message: ~p~n", [lager:pr(_Msg, ?MODULE)]),
    marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, 0, S, Fields]);
marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields}, body=Body}=_Msg) ->
    lager:debug("<2>Serializing message: ~p~n", [lager:pr(_Msg, ?MODULE)]),
    [ marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, size(Body), S, Fields]), Body ].


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


marshal_list(Types, Value) ->
    marshal_list(Types, Value, 0, []).


unmarshal_data(Data) ->
    unmarshal_data(Data, []).

unmarshal_data(<<>>, Res) ->
    {Res, <<>>};
unmarshal_data(Data, Res) ->
    try unmarshal_message(Data) of
	{Header, Data1} ->
	    unmarshal_data(Data1, Res ++ [Header])
    catch
	{'EXIT', _Reason} ->
	    {Res, Data}
    end.


unmarshal_signature(Signature) when is_binary(Signature) ->
    unmarshal_signature(binary_to_list(Signature));

unmarshal_signature(Signature) when is_list(Signature) ->
    {Sig, []} = unmarshal_signature(Signature, []),
    Sig;

unmarshal_signature($y) -> byte;
unmarshal_signature($b) -> boolean;
unmarshal_signature($n) -> int16;
unmarshal_signature($q) -> uint16;
unmarshal_signature($i) -> int32;
unmarshal_signature($u) -> uint32;
unmarshal_signature($x) -> int64;
unmarshal_signature($t) -> uint64;
unmarshal_signature($d) -> double;
unmarshal_signature($s) -> string;
unmarshal_signature($o) -> object_path;
unmarshal_signature($g) -> signature;
unmarshal_signature($r) -> struct;
unmarshal_signature($v) -> variant;
unmarshal_signature($e) -> dict_entry;
unmarshal_signature(Signature) ->
    throw({bad_signature, Signature}).

%%%
%%% Priv
%%%
marshal_header(Header) when is_list(Header) ->
    {Value, Pos} = marshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], 
				Header),
    Pad = pad(8, Pos),
    if
	Pad == 0 ->
	    Value;
	Pad > 0 ->
	    [Value, <<0:Pad>>]
    end.

unmarshal_message(Data) when is_binary(Data) ->
    {Header, BinBody, Data1} = unmarshal_header(Data),
    Signature =
	case dbus_message:find_field(?HEADER_SIGNATURE, Header) of
	    #dbus_variant{type=signature, value=Signature1} ->
		Signature1;
	    undefined ->
		<<"">>
	end,
    Types = unmarshal_signature(Signature),
    {<<>>, Body, _Pos} = unmarshal_list(Types, BinBody),
    {#dbus_message{header=Header, body=Body}, Data1}.

unmarshal_header(Bin) ->
    {Data1, HeaderData, Pos} = unmarshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], Bin),
    [$l, Type, Flags, ?DBUS_VERSION_MAJOR, Size, Serial, Fields] = HeaderData,
    Header = #dbus_header{type=Type,
			  flags=Flags,
			  serial=Serial,
			  fields=Fields},
    Pad = pad(8, Pos),
    <<0:Pad, Body:Size/binary, Data2/binary>> = Data1,
    {Header, Body, Data2}.


marshal_list([], [], Pos, Res) ->
    {Res, Pos};
marshal_list([Type | T], [Value | V], Pos, Res) ->
    {Res1, Pos1} = marshal(Type, Value, Pos),
    marshal_list(T, V, Pos1, [Res, Res1]).

marshal(byte, Value, Pos) ->
    marshal_uint(1, Value, Pos);

marshal(boolean, Value, Pos) ->
    Int =
	case Value of
	    true -> 1;
	    false -> 0
	end,
    marshal(uint32, Int, Pos);

marshal(int16, Value, Pos) ->
    marshal_int(2, Value, Pos);

marshal(uint16, Value, Pos) ->
    marshal_uint(2, Value, Pos);

marshal(int32, Value, Pos) ->
    marshal_int(4, Value, Pos);

marshal(uint32, Value, Pos) ->
    marshal_uint(4, Value, Pos);

marshal(int64, Value, Pos) ->
    marshal_int(8, Value, Pos);

marshal(uint64, Value, Pos) ->
    marshal_uint(8, Value, Pos);

marshal(double, Value, Pos) when is_float(Value) ->
    Pad = pad(8, Pos),
    {<< 0:Pad, Value:64/native-float >>, Pos + Pad div 8+ 8};

marshal(string, Value, Pos) when is_atom(Value) ->
    marshal(string, atom_to_binary(Value, utf8), Pos);

marshal(string, Value, Pos) when is_binary(Value) ->
    marshal_string(uint32, Value, Pos);

marshal(object_path, Value, Pos) ->
    marshal(string, Value, Pos);

marshal(signature, Value, Pos) ->
    marshal_string(byte, Value, Pos);

marshal({array, byte}=Type, Value, Pos) when is_binary(Value) ->
    marshal(Type, binary_to_list(Value), Pos);

marshal({array, SubType}, Value, Pos) when is_list(Value) ->
    Pad = pad(uint32, Pos),
    Pos0 = Pos + Pad div 8,
    Pos1 = Pos0 + 4,
    Pad1 = pad(SubType, Pos1),
    Pos1b = Pos1 + Pad1 div 8,
    {Value2, Pos2} = marshal_array(SubType, Value, Pos1b),
    Length = Pos2 - Pos1b,
    {Value1, Pos1} = marshal(uint32, Length, Pos0),
    {[<<0:Pad>>, Value1, <<0:Pad1>>, Value2], Pos2};

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

marshal(variant, Value, Pos) when is_integer(Value), Value < 0 ->
    marshal_int_variant(Value, Pos);

marshal(variant, Value, Pos) when is_integer(Value), Value >= 0 ->
    marshal_uint_variant(Value, Pos);

marshal(variant, Value, Pos) when is_tuple(Value) ->
    Type = infer_type(Value),
    marshal_variant(Type, Value, Pos).

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
    string.


infer_struct(Values) ->
    {struct, infer_struct(Values, [])}.

infer_struct([], Res) ->
    Res;
infer_struct([Value|R], Res) ->
    infer_struct(R, [Res, infer_type(Value)]).

infer_int(Value) when Value >= -32768 ->
    int16;
infer_int(Value) when Value >= -4294967296 ->
    int32;
infer_int(_Value) ->
    int64.

infer_uint(Value) when Value < 32768 ->
    uint16;
infer_uint(Value) when Value < 4294967296 ->
    uint32;
infer_uint(_Value) ->
    uint64.


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
    {<< 0:Pad, Value:(Len*8)/native-unsigned >>, Pos + Pad div 8 + Len}.

marshal_int(Len, Value, Pos) when is_integer(Value) ->
    Pad = pad(Len, Pos),
    {<< 0:Pad, Value:(Len*8)/native-signed >>, Pos + Pad div 8 + Len}.


marshal_string(LenType, Value, Pos) when is_list(Value) ->
    Length = length(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    {[Value1, Value, 0], Pos1 + Length + 1};

marshal_string(LenType, Value, Pos) when is_binary(Value) ->
    Length = size(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    {[Value1, Value, 0], Pos1 + Length + 1}.


marshal_array(SubType, Array, Pos) ->
    marshal_array(SubType, Array, Pos, []).

marshal_array(_SubType, [], Pos, Res) ->
    {Res, Pos};
marshal_array(SubType, [Value|R], Pos, Res) ->
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_array(SubType, R, Pos1, [Res, Value1]).


marshal_dict(KeyType, ValueType, Value, Pos) when is_tuple(Value) ->
    Array = dict:to_list(Value),
    marshal_dict(KeyType, ValueType, Array, Pos);

marshal_dict(KeyType, ValueType, Value, Pos) when is_list(Value) ->
    marshal({array, {struct, [KeyType, ValueType]}}, Value, Pos).


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

%% unmarshal(Type, Binary) ->

unmarshal(Type, <<>>, _Pos) ->
    throw({error, Type});
unmarshal(byte, Data, Pos) ->
    << Value:8, Data1/binary >> = Data,
    {Value, Data1, Pos + 1};

unmarshal(boolean, Data, Pos) ->
    {Int, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    Bool =
	case Int of
	    1 -> true;
	    0 -> false
	end,
    {Bool, Data1, Pos1};

unmarshal(uint16, Data, Pos) ->
    unmarshal_uint(2, Data, Pos);

unmarshal(uint32, Data, Pos) ->
    unmarshal_uint(4, Data, Pos);

unmarshal(uint64, Data, Pos) ->
    unmarshal_uint(8, Data, Pos);

unmarshal(int16, Data, Pos) ->
    unmarshal_int(2, Data, Pos);

unmarshal(int32, Data, Pos) ->
    unmarshal_int(4, Data, Pos);

unmarshal(int64, Data, Pos) ->
    unmarshal_int(8, Data, Pos);

unmarshal(double, Data, Pos) ->
    Pad = pad(8, Pos),
    << 0:Pad, Value:64/native-float, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + 8,
    {Value, Data1, Pos1};

unmarshal(signature, Data, Pos) ->
    unmarshal_string(byte, Data, Pos);

unmarshal(string, Data, Pos) ->
    unmarshal_string(uint32, Data, Pos);

unmarshal(object_path, Data, Pos) ->
    unmarshal_string(uint32, Data, Pos);

unmarshal({array, SubType}, Data, Pos) when true ->
    {Length, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    unmarshal_array(SubType, Length, Data1, Pos1);
unmarshal({struct, SubTypes}, Data, Pos) ->
    Pad = pad(8, Pos),
    << 0:Pad, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8,
    {Res, Data2, Pos2} = unmarshal_struct(SubTypes, Data1, Pos1),
    {list_to_tuple(Res), Data2, Pos2};

unmarshal({dict, KeyType, ValueType}, Data, Pos) ->
    {Length, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    {Res, Data2, Pos2} = unmarshal_array({struct, [KeyType, ValueType]}, Length, Data1, Pos1),
    {Res, Data2, Pos2};

unmarshal(variant, Data, Pos) ->
    {Signature, Data1, Pos1} = unmarshal(signature, Data, Pos),
    [Type] = unmarshal_signature(Signature),
    {Value, Data2, Pos2} = unmarshal(Type, Data1, Pos1),
    {#dbus_variant{type=Type, value=Value}, Data2, Pos2}.


unmarshal_uint(Len, Data, Pos) when is_integer(Len) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    << 0:Pad, Value:Bitlen/native-unsigned, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {Value, Data1, Pos1}.

unmarshal_int(Len, Data, Pos) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    << 0:Pad, Value:Bitlen/native-signed, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {Value, Data1, Pos1}.

unmarshal_signature([], Res) ->
    {Res, []};

unmarshal_signature([$a, ${, KeySig|R], Res) ->
    KeyType = unmarshal_signature(KeySig),
    {[ValueType], Sig} = unmarshal_signature(R, []),
    unmarshal_signature(Sig, [Res, {dict, KeyType, ValueType}]);

unmarshal_signature([$a|R], Res) ->
    {[Type | Types], []} = unmarshal_signature(R, []),
    {[Res, [{array, Type}], Types], []};

unmarshal_signature([$(|R], Res) ->
    {Types, Rest} = unmarshal_signature(R, []),
    unmarshal_signature(Rest, [Res, {struct, Types}]);

unmarshal_signature([$)|R], Res) ->
    {Res, R};

unmarshal_signature([$}|R], Res) ->
    {Res, R};

unmarshal_signature([Signature|R], Res) ->
    Type = unmarshal_signature(Signature),
    unmarshal_signature(R, [Res, Type]).


%unmarshal_struct_signature([$)|R], Res) ->
%    {Res, R};
%unmarshal_struct_signature([Signature|R], Res) ->
%    Type = unmarshal_signature(Signature),
%    unmarshal_struct_signature(R, Res ++ [Type]).
    


unmarshal_struct(SubTypes, Data, Pos) ->
    unmarshal_struct(SubTypes, Data, [], Pos).


unmarshal_struct([], Data, Res, Pos) ->
    {Res, Data, Pos};

unmarshal_struct([SubType|S], Data, Res, Pos) ->
    {Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    unmarshal_struct(S, Data1, [Res, Value], Pos1).

unmarshal_array(SubType, Length, Data, Pos) ->
    Pad = pad(padding(SubType), Pos),
    << 0:Pad, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8,
    unmarshal_array(SubType, Length, Data1, [], Pos1).

unmarshal_array(_SubType, 0, Data, Res, Pos) ->
    {Res, Data, Pos};
unmarshal_array(SubType, Length, Data, Res, Pos) when is_integer(Length), Length > 0 ->
    {Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    Size = Pos1 - Pos,
    unmarshal_array(SubType, Length - Size, Data1, Res ++ [Value], Pos1).

unmarshal_list(Types, Data) when is_list(Types), is_binary(Data) ->
    unmarshal_list(Types, Data, 0).

unmarshal_list(Types, Data, Pos) when is_list(Types) ->
    unmarshal_list(Types, Data, [], Pos).

unmarshal_list([], Data, Res, Pos) ->
    {Data, Res, Pos};
unmarshal_list([Type|T], Data, Res, Pos) ->
    {Value, Data1, Pos1} = unmarshal(Type, Data, Pos),
    unmarshal_list(T, Data1, [Res, Value], Pos1).


unmarshal_string(LenType, Data, Pos) ->
    {Length, Data1, Pos1} = unmarshal(LenType, Data, Pos),
    << String:Length/binary, 0, Data2/binary >> = Data1,
    Pos2 = Pos1 + Length + 1,
    {String, Data2, Pos2}.

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
