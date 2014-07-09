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

-define(HEADER_SIGNATURE, [byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}]).

%%%
%%% API
%%%
-spec marshal_message(dbus_message()) -> iolist().
marshal_message(#dbus_message{header=#dbus_header{serial=0}}=_Msg) ->
    throw({error, invalid_serial});
marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields}, body= <<>>}=_Msg) ->
<<<<<<< HEAD
    lager:debug("<1>Serializing message: ~p~n", [lager:pr(_Msg, ?MODULE)]),
    R = marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, 0, S, Fields]),
    lager:info("marshal_message :header R=~p",[R]);
=======
    marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, 0, S, Fields]);
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486
marshal_message(#dbus_message{header=#dbus_header{type=Type, flags=Flags, serial=S, fields=Fields}, body=Body}=_Msg) ->
    BinBody = iolist_to_binary(Body),
    [ marshal_header([$l, Type, Flags, ?DBUS_VERSION_MAJOR, byte_size(BinBody), S, Fields]), BinBody ].

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


-spec marshal_list(dbus_signature(), term()) -> iolist().
marshal_list(Types, Value) ->
    lager:info("marshal_list 2"),
    marshal_list(Types, Value, 0, []).


-spec unmarshal_data(binary()) -> {term(), binary()}.
unmarshal_data(Data) ->
    unmarshal_data(Data, []).


-spec unmarshal_signature(binary()) -> dbus_signature().
unmarshal_signature(<<>>) -> 
    lager:info("unmarshal_signature/1  1"),
    [];
unmarshal_signature(<<C>>) ->
    lager:info("unmarshal_signature/1  2"),
    Type = unmarshal_type(<<C>>),
    lager:info("TTTTTTYPE= ~p",[Type]),
    Type;
unmarshal_signature(Bin) when is_binary(Bin) ->
    lager:info("unmarshal_signature/1  3"),
    {Signature, <<>>} = unmarshal_signature(Bin, []),
    lager:info("unmarshal_signature/1  3 Signature=~p",[Signature]),
    Signature.


%%%
%%% Priv marshalling
%%%
marshal_header(Header) when is_list(Header) ->
<<<<<<< HEAD
    lager:info("marshal_header ~p", [Header]),
    {Value, Pos} = marshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], 
				Header),
=======
    {Value, Pos} = marshal_list(?HEADER_SIGNATURE, Header),
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486
    Pad = pad(8, Pos),
    lager:info("marshal_header Pad=~p",[Pad]),
    if
	    Pad == 0 ->
	        Value;
	    Pad > 0 ->
	        [Value, <<0:Pad>>]
    end.


marshal_list([], [], Pos, Res) ->
    lager:info("marshal_list 111   Pos=~p,Res=~p",[Pos, Res]),
    {Res, Pos};
marshal_list([Type | T], [Value | V], Pos, Res) ->
    {Res1, Pos1} = marshal(Type, Value, Pos),
    lager:info("marshal_list 222 Pos1=~p,Res1=~p",[Pos1, Res1]),
    marshal_list(T, V, Pos1, [Res, Res1]).

marshal(byte, Value, Pos) ->
    lager:info("marshal 1"),
    marshal_uint(1, Value, Pos);

marshal(boolean, Value, Pos) ->
    lager:info("marshal 2"),
    Int =
	    case Value of
	        true -> 1;
	        false -> 0
	    end,
    marshal(uint32, Int, Pos);

marshal(int16, Value, Pos) ->
    lager:info("marshal 3"),
    marshal_int(2, Value, Pos);

marshal(uint16, Value, Pos) ->
    lager:info("marshal 4"),
    marshal_uint(2, Value, Pos);

marshal(int32, Value, Pos) ->
    lager:info("marshal 5"),
    marshal_int(4, Value, Pos);

marshal(uint32, Value, Pos) ->
    lager:info("marshal 6"),
    marshal_uint(4, Value, Pos);

marshal(int64, Value, Pos) ->
    lager:info("marshal 7"),
    marshal_int(8, Value, Pos);

marshal(uint64, Value, Pos) ->
    lager:info("marshal 8"),
    marshal_uint(8, Value, Pos);

marshal(double, Value, Pos) when is_float(Value) ->
    Pad = pad(8, Pos),
    lager:info("marshal 8 Pad=~p",[Pad]),
    {<< 0:Pad, Value:64/native-float >>, Pos + Pad div 8+ 8};

marshal(string, Value, Pos) when is_atom(Value) ->
    lager:info("marshal 9"),
    marshal(string, atom_to_binary(Value, utf8), Pos);

marshal(string, Value, Pos) when is_binary(Value) ->
    lager:info("marshal 10"),
    marshal_string(uint32, Value, Pos);

marshal(object_path, Value, Pos) ->
    lager:info("marshal 11 Value=~p,Pos=~p",[Value,Pos]),
    marshal(string, Value, Pos);

marshal(signature, Value, Pos) ->
    lager:info("marshal 12"),
    marshal_string(byte, Value, Pos);

marshal({array, byte}=Type, Value, Pos) when is_binary(Value) ->
    lager:info("marshal 13"),
    marshal(Type, binary_to_list(Value), Pos);

marshal({array, SubType}, Value, Pos) when is_list(Value) ->
    lager:info("marshal 14"),
    Pad = pad(uint32, Pos),
    Pos0 = Pos + Pad div 8,
    Pos1 = Pos0 + 4,
    Pad1 = pad(SubType, Pos1),
    Pos1b = Pos1 + Pad1 div 8,
    {Value2, Pos2} = marshal_array(SubType, Value, Pos1b),
    lager:info("marshal  14 Valu2=~p, Pos2= ~p",[Value,Pos]),
    Length = Pos2 - Pos1b,
    {Value1, Pos1} = marshal(uint32, Length, Pos0),
    lager:info("marshal 14 Value1=~p, Pos1=~p",[Value,Pos]),
    {[<<0:Pad>>, Value1, <<0:Pad1>>, Value2], Pos2};

marshal({struct, _SubTypes}=Type, Value, Pos) when is_tuple(Value) ->
    lager:info("marshal 15"),
    marshal(Type, tuple_to_list(Value), Pos);

marshal({struct, SubTypes}, Value, Pos) when is_list(Value) ->
    lager:info("marshal 16"),
    marshal_struct(SubTypes, Value, Pos);

marshal({dict, KeyType, ValueType}, Value, Pos) ->
    lager:info("marshal 17"),
    marshal_dict(KeyType, ValueType, Value, Pos);

marshal(variant, Value, Pos) when is_binary(Value) ->
    lager:info("marshal 18"),
    marshal_variant({array, byte}, Value, Pos);

marshal(variant, #dbus_variant{type=Type, value=Value}, Pos) ->
    lager:info("marshal 19"),
    marshal_variant(Type, Value, Pos);

marshal(variant, true=Value, Pos) ->
    lager:info("marshal 20"),
    marshal_variant(boolean, Value, Pos);

marshal(variant, false=Value, Pos) ->
    lager:info("marshal 21"),
    marshal_variant(boolean, Value, Pos);

marshal(variant, Value, Pos) when is_integer(Value), Value < 0 ->
    lager:info("marshal 22"),
    marshal_int_variant(Value, Pos);

marshal(variant, Value, Pos) when is_integer(Value), Value >= 0 ->
    lager:info("marshal 23"),
    marshal_uint_variant(Value, Pos);

marshal(variant, Value, Pos) when is_tuple(Value) ->
    lager:info("marshal 24"),
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
    lager:info("marshal_variant "),
    {Value1, Pos1} = marshal(signature, marshal_signature(Type), Pos),
    lager:info("marshal_variant 1 Value1=~p,Pos1=~p", [Value1,Pos1]),
    {Value2, Pos2} = marshal(Type, Value, Pos1),
    lager:info("marshal_string 2 Value2=~p,Pos2=~p", [Value2,Pos2]),
    {[Value1, Value2], Pos2}.


marshal_uint(Len, Value, Pos) when is_integer(Value) ->
    
    Pad = pad(Len, Pos),
    lager:info("marshal_uint Pad=~p",[Pad]),
    {<< 0:Pad, Value:(Len*8)/native-unsigned >>, Pos + Pad div 8 + Len}.

marshal_int(Len, Value, Pos) when is_integer(Value) ->
    Pad = pad(Len, Pos),
    {<< 0:Pad, Value:(Len*8)/native-signed >>, Pos + Pad div 8 + Len}.


marshal_string(LenType, Value, Pos) when is_list(Value) ->
    Length = length(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    lager:info("marshal_string 1 Value1=~p,Pos1=~p", [Value1,Pos1]),
    {[Value1, Value, 0], Pos1 + Length + 1};

marshal_string(LenType, Value, Pos) when is_binary(Value) ->
    Length = size(Value),
    {Value1, Pos1} = marshal(LenType, Length, Pos),
    lager:info("marshal_string 2 Value1=~p,Pos1=~p", [Value1,Pos1]),
    {[Value1, Value, 0], Pos1 + Length + 1}.


marshal_array(SubType, Array, Pos) ->
    lager:info("marshal_array 1"),
    marshal_array(SubType, Array, Pos, []).

marshal_array(_SubType, [], Pos, Res) ->
    lager:info("marshal_array 2"),
    {Res, Pos};
marshal_array(SubType, [Value|R], Pos, Res) ->
    lager:info("marshal_array 3"),
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    lager:info("marshal_array 3 Value1=~p, Pos1=~p",[Value,Pos]),
    marshal_array(SubType, R, Pos1, [Res, Value1]).


marshal_dict(KeyType, ValueType, Value, Pos) when is_tuple(Value) ->
    Array = dict:to_list(Value),
    marshal_dict(KeyType, ValueType, Array, Pos);

marshal_dict(KeyType, ValueType, Value, Pos) when is_list(Value) ->
    marshal({array, {struct, [KeyType, ValueType]}}, Value, Pos).


marshal_struct(SubTypes, Values, Pos) ->
    Pad = pad(8, Pos),
    {Values1, Pos1} = marshal_struct(SubTypes, Values, Pos + Pad div 8, []),
    lager:info("marshal_struct 1 Values1=~p, Pos1=~p",[Values1,Pos1]),
    if
	    Pad == 0 ->
	        {Values1, Pos1};
	    Pad > 0 ->
	        {[<< 0:Pad >>, Values1], Pos1}
    end.

marshal_struct([], [], Pos, Res) ->
    lager:info("marshal_struct 2"),
    {Res, Pos};
marshal_struct([SubType|R], [Value|V], Pos, Res) ->
    lager:info("marshal_struct 3"),
    {Value1, Pos1} = marshal(SubType, Value, Pos),
    lager:info("marshal_struct 3 Value1=~p, Pos1=~p",[Value1,Pos1]),
    marshal_struct(R, V, Pos1, [Res, Value1]).



marshal_struct_signature([], Res) ->
    Res;
marshal_struct_signature([SubType|R], Res) ->
    marshal_struct_signature(R, [Res, marshal_signature(SubType)]).

%%%
%%% Private unmarshaling
%%%
unmarshal_data(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
unmarshal_data(Data, Acc) ->
    try unmarshal_message(Data) of
	{#dbus_message{}=Msg, Rest} -> unmarshal_data(Rest, [Msg | Acc]);
	_ ->
	    lager:error("Error parsing data~n", []),
	    throw({error, dbus_parse_error})
    catch
	{'EXIT', Err} -> 
	    throw({error, {dbus_parse_error, Err}})
    end.


unmarshal_message(Data) when is_binary(Data) ->
<<<<<<< HEAD
    {Header, BinBody, Data1} = unmarshal_header(Data),
    lager:info("unmarshal_message Header=~p, BinBody=~p, Data1=~p",[Header,BinBody,Data1]),
    Signature =
	    case dbus_message:find_field(?HEADER_SIGNATURE, Header) of
	        #dbus_variant{type=signature, value=Signature1} -> Signature1;
	        undefined -> <<>>
	    end,
    Types = unmarshal_signature(Signature),
    lager:info("unmarshal_message Signature=~p, Types=~p",[Signature,Types]),
    {<<>>, Body, _Pos} = unmarshal_list(Types, BinBody),
    {#dbus_message{header=Header, body=Body}, Data1}.

unmarshal_header(Bin) ->
    {Data1, HeaderData, Pos} = unmarshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], Bin),
    lager:info("unmarshal_header Data1=~p, HeaderData=~p, Pos=~p",[Data1,HeaderData,Pos]),
    [[[[[[[[],$l], Type], Flags], ?DBUS_VERSION_MAJOR], Size], Serial], Fields] = HeaderData,
    Fields1 = lists:flatmap(fun({[[], Code], Content}) -> [{Code, Content}] end, Fields),
    lager:info("unmarshal_header   Fields1 = ~p",[Fields1]),
    Header = #dbus_header{type=Type, flags=Flags, serial=Serial, fields=Fields1},
    lager:info("unmarshal_header  Header=~p",[Header]),
    Pad = pad(8, Pos),
    lager:info("unmarshal_list ~p ~p ~p ~p", [Pos, Pad, Size, size(Data1)]),
    <<0:Pad, Body:Size/binary, Data2/binary>> = Data1,
    {Header, Body, Data2}.
=======
    {Header, BodyBin, Rest} = unmarshal_header(Data),
    BodySig =
	case dbus_message:find_field(?FIELD_SIGNATURE, Header) of
	    #dbus_variant{type=signature, value=Val} ->
		unmarshal_signature(Val);
	    undefined -> 
		[]
	end,
    case unmarshal_list(BodySig, BodyBin) of
	{Body, <<>>, _Pos} ->
	    {#dbus_message{header=Header, body=Body}, Rest};
	{_Body, Rest2, _Pos} ->
	    lager:error("Garbage parsing message body: ~p~n", [Rest2]),
	    throw({error, body_parse_error})
    end.

unmarshal_header(Bin) ->
    case unmarshal_list(?HEADER_SIGNATURE, Bin) of
	{[$l, Type, Flags, ?DBUS_VERSION_MAJOR, Size, Serial, Fields], Rest, Pos} ->
	    Header = #dbus_header{type=Type, flags=Flags, serial=Serial, fields=Fields},
	    Pad = pad(8, Pos),
	    <<0:Pad, Body:Size/binary, Rest2/binary>> = Rest,
	    {Header, Body, Rest2};
	{Term, _Rest, _Pos} ->
	    lager:error("Error parsing header data: ~p~n", [Term]),
	    throw({error, malformed_header})
    end.
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486


unmarshal(Type, <<>>, _Pos) ->
    lager:info("unmarshal 1"),
    throw({error, Type});
unmarshal(byte, Data, Pos) ->
    lager:info("unmarshal 2"),
    << Value:8, Data1/binary >> = Data,
    {Value, Data1, Pos + 1};

unmarshal(boolean, Data, Pos) ->
    lager:info("unmarshal 3"),
    {Int, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    Bool =
	    case Int of
	        1 -> true;
	        0 -> false
	    end,
    {Bool, Data1, Pos1};

unmarshal(uint16, Data, Pos) ->
    lager:info("unmarshal 4"),
    unmarshal_uint(2, Data, Pos);

unmarshal(uint32, Data, Pos) ->
    lager:info("unmarshal 5"),
    unmarshal_uint(4, Data, Pos);

unmarshal(uint64, Data, Pos) ->
    lager:info("unmarshal 6"),
    unmarshal_uint(8, Data, Pos);

unmarshal(int16, Data, Pos) ->
    lager:info("unmarshal 7"),
    unmarshal_int(2, Data, Pos);

unmarshal(int32, Data, Pos) ->
    lager:info("unmarshal 8"),
    unmarshal_int(4, Data, Pos);

unmarshal(int64, Data, Pos) ->
    lager:info("unmarshal 9"),
    unmarshal_int(8, Data, Pos);

unmarshal(double, Data, Pos) ->
    lager:info("unmarshal 10"),
    Pad = pad(8, Pos),
    << 0:Pad, Value:64/native-float, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + 8,
    {Value, Data1, Pos1};

unmarshal(signature, Data, Pos) ->
    lager:info("unmarshal 11"),
    unmarshal_string(byte, Data, Pos);

unmarshal(string, Data, Pos) ->
    lager:info("unmarshal 12"),
    unmarshal_string(uint32, Data, Pos);

unmarshal(object_path, Data, Pos) ->
    lager:info("unmarshal 13"),
    unmarshal_string(uint32, Data, Pos);

unmarshal({array, SubType}, Data, Pos) when true ->
<<<<<<< HEAD
    lager:info("unmarshal 14"),
    {Length, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    unmarshal_array(SubType, Length, Data1, Pos1);
=======
    {Length, Rest, NewPos} = unmarshal(uint32, Data, Pos),
    unmarshal_array(SubType, Length, Rest, NewPos);

>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486
unmarshal({struct, SubTypes}, Data, Pos) ->
    lager:info("unmarshal 15"),
    Pad = pad(8, Pos),
    << 0:Pad, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8,
    {Res, Data2, Pos2} = unmarshal_struct(SubTypes, Data1, Pos1),
    {list_to_tuple(Res), Data2, Pos2};

unmarshal({dict, KeyType, ValueType}, Data, Pos) ->
    lager:info("unmarshal 16"),
    {Length, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    {Res, Data2, Pos2} = unmarshal_array({struct, [KeyType, ValueType]}, Length, Data1, Pos1),
    {Res, Data2, Pos2};

unmarshal(variant, Data, Pos) ->
    lager:info("unmarshal 17"),
    {Signature, Data1, Pos1} = unmarshal(signature, Data, Pos),
<<<<<<< HEAD
    lager:info("unmarshal Signature=~p, Data1=~p, Pos1=~p",[Signature,Data, Pos]),
    Type = unmarshal_signature(Signature),
    lager:info("unmarshal 17 Type=~p",[Type]),
=======
    Type = unmarshal_signature(Signature),
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486
    {Value, Data2, Pos2} = unmarshal(Type, Data1, Pos1),
    lager:info("unmarshal 17 Value=~p,Data1=~p,Pos1=~p",[Value,Data,Pos]),
    {#dbus_variant{type=Type, value=Value}, Data2, Pos2}.


unmarshal_uint(Len, Data, Pos) when is_integer(Len) ->
    lager:info("unmarshal_uint 1"),
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    lager:info("unmarshal_uint 1 Pad=~p",[Pad]),
    << 0:Pad, Value:Bitlen/native-unsigned, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {Value, Data1, Pos1}.

unmarshal_int(Len, Data, Pos) ->
    Bitlen = Len * 8,
    Pad = pad(Len, Pos),
    << 0:Pad, Value:Bitlen/native-signed, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {Value, Data1, Pos1}.


unmarshal_signature(<<>>, Acc) ->
    {Acc, <<>>};

unmarshal_signature(<<$a, ${, KeySig, Rest/bits>>, Acc) ->
    {KeyType, <<>>} = unmarshal_signature(KeySig),
    {[ValueType], Rest2} = unmarshal_signature(Rest, []),
    unmarshal_signature(Rest2, [Acc, {dict, KeyType, ValueType}]);

unmarshal_signature(<<$a, Rest/bits>>, Acc) ->
    {[Type | Types], <<>>} = unmarshal_signature(Rest, []),
    {[Acc, [{array, Type}], Types], <<>>};

unmarshal_signature(<<$(, Rest/bits>>, Acc) ->
    {Types, Rest2} = unmarshal_signature(Rest, []),
    unmarshal_signature(Rest2, [Acc, {struct, Types}]);

unmarshal_signature(<<$), Rest/bits>>, Acc) ->
    {Acc, Rest};

unmarshal_signature(<<$}, Rest/bits>>, Acc) ->
    {Acc, Rest};

unmarshal_signature(<<C, Rest/bits>>, Acc) ->
    Type = unmarshal_type(C),
    unmarshal_signature(Rest, [Acc, Type]).

unmarshal_type(<<$y>>) -> byte;
unmarshal_type(<<$b>>) -> boolean;
unmarshal_type(<<$n>>) -> int16;
unmarshal_type(<<$q>>) -> uint16;
unmarshal_type(<<$i>>) -> int32;
unmarshal_type(<<$u>>) -> uint32;
unmarshal_type(<<$x>>) -> int64;
unmarshal_type(<<$t>>) -> uint64;
unmarshal_type(<<$d>>) -> double;
unmarshal_type(<<$s>>) -> string;
unmarshal_type(<<$o>>) -> object_path;
unmarshal_type(<<$g>>) -> signature;
unmarshal_type(<<$r>>) -> struct;
unmarshal_type(<<$v>>) -> variant;
unmarshal_type(<<$e>>) -> dict_entry;
unmarshal_type(<<$a>>) -> array;
unmarshal_type(_C)      -> throw({error, {bad_type, _C}}).


%unmarshal_struct_signature([$)|R], Res) ->
%    {Res, R};
%unmarshal_struct_signature([Signature|R], Res) ->
%    Type = unmarshal_signature(Signature),
%    unmarshal_struct_signature(R, Res ++ [Type]).
    


unmarshal_struct(SubTypes, Data, Pos) ->
    lager:info("unmarshal_struct 1"),
    unmarshal_struct(SubTypes, Data, [], Pos).


<<<<<<< HEAD
unmarshal_struct([], Data, Res, Pos) ->    
    lager:info("unmarshal_struct 2"),
    {Res, Data, Pos};

unmarshal_struct([SubType|S], Data, Res, Pos) ->
    lager:info("unmarshal_struct 3"),
=======
unmarshal_struct([], Data, Acc, Pos) ->
    {lists:reverse(Acc), Data, Pos};

unmarshal_struct([SubType|S], Data, Acc, Pos) ->
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486
    {Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    unmarshal_struct(S, Data1, [Value | Acc], Pos1).

unmarshal_array(SubType, Length, Data, Pos) ->
    lager:info("unmarshal_array 1"),
    Pad = pad(padding(SubType), Pos),
    << 0:Pad, Rest/binary >> = Data,
    NewPos = Pos + Pad div 8,
    unmarshal_array(SubType, Length, Rest, [], NewPos).

unmarshal_array(_SubType, 0, Data, Res, Pos) ->
    lager:info("unmarshal_array 2"),
    {Res, Data, Pos};
unmarshal_array(SubType, Length, Data, Res, Pos) when is_integer(Length), Length > 0 ->
    lager:info("unmarshal_array 3"),
    {Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    Size = Pos1 - Pos,
    unmarshal_array(SubType, Length - Size, Data1, Res ++ [Value], Pos1).

unmarshal_list(Type, Data) when is_atom(Type), is_binary(Data) ->
    unmarshal(Type, Data, 0);
unmarshal_list(Types, Data) when is_list(Types), is_binary(Data) ->
<<<<<<< HEAD
    lager:info("unmarshal_list 1 "),
    unmarshal_list(Types, Data, 0).

unmarshal_list(Types, Data, Pos) when is_list(Types) ->
    lager:info("unmarshal_list 2 "),
    unmarshal_list(Types, Data, [], Pos).

unmarshal_list([], Data, Res, Pos) ->
    lager:info("unmarshal_list 3 "),
    {Data, Res, Pos};
unmarshal_list([Type|T], Data, Res, Pos) ->
    lager:info("unmarshal_list 4 "),
    {Value, Data1, Pos1} = unmarshal(Type, Data, Pos),
    lager:info("unmarshal_list 4 TYPE=~p",[Type]),
    unmarshal_list(T, Data1, [Res, Value], Pos1).
=======
    unmarshal_list(Types, Data, [], 0).

unmarshal_list([], Rest, Acc, Pos) ->
    {lists:reverse(Acc), Rest, Pos};
unmarshal_list([Type|T], Data, Acc, Pos) ->
    {Value, Rest, Pos1} = unmarshal(Type, Data, Pos),
    unmarshal_list(T, Rest, [Value | Acc], Pos1).
>>>>>>> 751d7c75caeb4cb4bcb83b698bcc9b43cd7ae486


unmarshal_string(LenType, Data, Pos) ->
    lager:info("unmarshal_string 1"),
    {Length, Data1, Pos1} = unmarshal(LenType, Data, Pos),
    << String:Length/binary, 0, Data2/binary >> = Data1,
    Pos2 = Pos1 + Length + 1,
    lager:info("unmarshal_string 1 Pos2=~p,Data2=~p",[Pos2,Data2]),
    {String, Data2, Pos2}.

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
    lager:info("pad 1"),
    ((Size - (Pos rem Size)) rem Size) * 8;
pad(Type, Pos) when is_atom(Type); 
		            array =:= element(1, Type);
		            struct =:= element(1, Type)->
    lager:info("pad 2"),
    pad(padding(Type), Pos).
