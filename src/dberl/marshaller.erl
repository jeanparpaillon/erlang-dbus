-module(dberl.marshaller).

-import(error_logger).
-import(file).
-import(io).
-import(lists).

-include("dbus.hrl").

-compile([export_all]).

%% api
-export([
	 marshal_message/1,
%% 	 marshal_message/2,
	 marshal_list/2,
	 unmarshal_data/1,
	 unmarshal_signature/1
	]).

-export([test/0]).

test() ->
    Data = read_test(),
    unmarshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], Data).

read_test() ->
    {ok, File} = file:open("/tmp/bytes", [binary, read]),
    {ok, Data} = file:read(File, 1024),
    ok = file:close(File),
    Data.
    
unmarshal_data(Data) ->
    unmarshal_data(Data, []).

unmarshal_data(<<>>, Res) ->
    {ok, Res, <<>>};
unmarshal_data(Data, Res) ->
    case catch unmarshal_message(Data) of
	{ok, Header, Data1} ->
	    unmarshal_data(Data1, Res ++ [Header]);
	{'EXIT', Reason} ->
	    error_logger:info_msg("unmarshal_data: ~p~n", [Data]),
	    {ok, Res, Data}
    end.


%% marshal_message(Header) ->
%%     marshal_message(Header, <<>>).

marshal_message(Header) when is_record(Header, header) ->
    Body = Header#header.body,
    Body1 =
	if
	    is_list(Body) ->
		list_to_binary(Body);
	    is_binary(Body) ->
		Body
	end,
	    
%%     HeaderList = message:to_list(Header),
    HeaderList = [$l,
		  Header#header.type,
		  Header#header.flags,
		  ?DBUS_VERSION_MAJOR,
		  size(Body1),
		  Header#header.serial,
		  Header#header.headers],
    {ok, HeaderData} = marshal_header(HeaderList),
    {ok, [ HeaderData, Body1 ]}.

marshal_header(Header) when is_list(Header) ->
    {ok, Value, Pos} = marshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], Header),
    Pad = padding(8, Pos),
    if
	Pad == 0 ->
	    {ok, Value};
	Pad > 0 ->
	    {ok, [Value, <<0:Pad>>]}
    end.


%% unmarshal_header(Data) when is_list(Data) ->
%%     unmarshal_header(list_to_binary(Data));

unmarshal_message(Data) ->
    {ok, Header, BinBody, Data1} = unmarshal_header(Data),
    Signature =
	case message:header_find(?HEADER_SIGNATURE, Header) of
	    {ok, {?HEADER_SIGNATURE, #variant{type=signature, value=Signature1}}} ->
		Signature1;
	    error ->
		""
	end,
    Types = unmarshal_signature(Signature),
    {ok, <<>>, Body, _Pos} = unmarshal_list(Types, BinBody),
    Header1 = Header#header{body=Body},
    {ok, Header1, Data1}.

unmarshal_header(Bin) ->
    {ok, Data1, HeaderData, Pos} = unmarshal_list([byte, byte, byte, byte, uint32, uint32, {array, {struct, [byte, variant]}}], Bin),
    [$l, Type, Flags, ?DBUS_VERSION_MAJOR, Size, Serial, Headers] = HeaderData,
    Header = #header{type=Type,
		     flags=Flags,
		     serial=Serial,
		     headers=Headers},
    Pad = padding(8, Pos),
%%     io:format("unmarshal_list ~p ~p ~p ~p~n", [Pos, Pad, Size, size(Data1)]),
    <<0:Pad, Body:Size/binary, Data2/binary>> = Data1,
    {ok, Header, Body, Data2}.
    

%% marshal(Type, Value)

marshal_list(Types, Value) ->
    marshal_list(Types, Value, 0, []).

marshal_list([], [], Pos, Res) ->
    {ok, Res, Pos};
marshal_list([Type|T], [Value|V], Pos, Res) ->
    {ok, Res1, Pos1} = marshal(Type, Value, Pos),
    marshal_list(T, V, Pos1, Res ++ [Res1]).

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
    Pad = padding(8, Pos),
    {ok, << 0:Pad, Value:64/native-float >>, Pos + Pad div 8+ 8};
marshal(string, Value, Pos) when is_binary(Value) ->
    marshal(string, binary_to_list(Value), Pos);
marshal(string, Value, Pos) when is_atom(Value) ->
    marshal(string, atom_to_list(Value), Pos);
marshal(string, Value, Pos) when is_list(Value) ->
    marshal_string(uint32, Value, Pos);
marshal(object_path, Value, Pos) ->
    marshal(string, Value, Pos);
marshal(signature, Value, Pos) ->
    marshal_string(byte, Value, Pos);
marshal({array, byte}=Type, Value, Pos) when is_binary(Value) ->
    marshal(Type, binary_to_list(Value), Pos);
marshal({array, SubType}, Value, Pos) when is_list(Value) ->
    Pad = padding(4, Pos),
    Pos0 = Pos + Pad div 8,
    Pos1 = Pos0 + 4,
    {ok, Value2, Pos2} = marshal_array(SubType, Value, Pos1),
    Length = Pos2 - Pos1,
    {ok, Value1, Pos1} = marshal(uint32, Length, Pos0),
    {ok, [<<0:Pad>>, Value1, Value2], Pos2};
marshal({struct, _SubTypes}=Type, Value, Pos) when is_tuple(Value) ->
    marshal(Type, tuple_to_list(Value), Pos);
marshal({struct, SubTypes}, Value, Pos) when is_list(Value) ->
    marshal_struct(SubTypes, Value, Pos);
marshal(variant, Value, Pos) when is_binary(Value) ->
    marshal_variant({array, byte}, Value, Pos);
marshal(variant, #variant{type=Type, value=Value}, Pos) ->
    marshal_variant(Type, Value, Pos);
marshal(variant, true=Value, Pos) ->
    marshal_variant(boolean, Value, Pos);
marshal(variant, false=Value, Pos) ->
    marshal_variant(boolean, Value, Pos);
marshal(variant, Value, Pos) when is_integer(Value), Value < 0 ->
    marshal_int_variant(Value, Pos);
marshal(variant, Value, Pos) when is_integer(Value), Value >= 0 ->
    marshal_uint_variant(Value, Pos).


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
    {ok, Value1, Pos1} = marshal(signature, marshal_signature(Type), Pos),
    {ok, Value2, Pos2} = marshal(Type, Value, Pos1),
    {ok, [Value1, Value2], Pos2}.


marshal_uint(Len, Value, Pos) when is_integer(Value) ->
    Pad = padding(Len, Pos),
    {ok, << 0:Pad, Value:(Len*8)/native-unsigned >>, Pos + Pad div 8 + Len}.

marshal_int(Len, Value, Pos) when is_integer(Value) ->
    Pad = padding(Len, Pos),
    {ok, << 0:Pad, Value:(Len*8)/native-signed >>, Pos + Pad div 8 + Len}.


marshal_string(LenType, Value, Pos) when is_list(Value) ->
    marshal_string(LenType, list_to_binary(Value), Pos);

marshal_string(LenType, Value, Pos) when is_binary(Value) ->
    Length = size(Value),
    {ok, Value1, Pos1} = marshal(LenType, Length, Pos),
    {ok, [Value1, Value, 0], Pos1 + Length + 1}.


marshal_array(SubType, Array, Pos) ->
    marshal_array(SubType, Array, Pos, []).

marshal_array(_SubType, [], Pos, Res) ->
    {ok, Res, Pos};
marshal_array(SubType, [Value|R], Pos, Res) ->
    {ok, Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_array(SubType, R, Pos1, Res ++ [Value1]).


marshal_struct(SubTypes, Values, Pos) ->
    Pad = padding(8, Pos),
%%     io:format("marshal_struct ~p ~p ~n", [Pos, Pad]),
    {ok, Values1, Pos1} = marshal_struct(SubTypes, Values, Pos + Pad div 8, []),
    if
	Pad == 0 ->
	    {ok, Values1, Pos1};
	Pad > 0 ->
	    {ok, [<< 0:Pad >>, Values1], Pos1}
    end.

marshal_struct([], [], Pos, Res) ->
    {ok, Res, Pos};
marshal_struct([SubType|R], [Value|V], Pos, Res) ->
    {ok, Value1, Pos1} = marshal(SubType, Value, Pos),
    marshal_struct(R, V, Pos1, Res ++ [Value1]).


marshal_signature(byte) ->
    "y";
marshal_signature(boolean) ->
    "b";
marshal_signature(int16) ->
    "n";
marshal_signature(uint16) ->
    "q";
marshal_signature(int32) ->
    "i";
marshal_signature(uint32) ->
    "u";
marshal_signature(int64) ->
    "x";
marshal_signature(uint64) ->
    "t";
marshal_signature(double) ->
    "d";
marshal_signature(string) ->
    "s";
marshal_signature(object_path) ->
    "o";
marshal_signature(signature) ->
    "g";
marshal_signature({array, Type}) ->
    [$a, marshal_signature(Type)];
marshal_signature({struct, SubTypes}) ->
    [$(, marshal_struct_signature(SubTypes, []), $)];
marshal_signature(variant) ->
    "v";
marshal_signature(dict_entry) ->
    exit(todo);
marshal_signature([]) ->
    "";
marshal_signature([Type|R]) ->
    marshal_signature(Type) ++ marshal_signature(R).


marshal_struct_signature([], Res) ->
    Res;
marshal_struct_signature([SubType|R], Res) ->
    marshal_struct_signature(R, Res ++ [marshal_signature(SubType)]).

%% unmarshal(Type, Binary) ->

unmarshal(_Type, <<>>, _Pos) ->
    error;
unmarshal(byte, Data, Pos) ->
    << Value:8, Data1/binary >> = Data,
    {ok, Value, Data1, Pos + 1};

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

unmarshal(signature, Data, Pos) ->
    unmarshal_string(byte, Data, Pos);

unmarshal(string, Data, Pos) ->
    unmarshal_string(uint32, Data, Pos);

unmarshal(object_path, Data, Pos) ->
    unmarshal_string(uint32, Data, Pos);

unmarshal({array, SubType}, Data, Pos) when true ->
    {ok, Length, Data1, Pos1} = unmarshal(uint32, Data, Pos),
    unmarshal_array(SubType, Length, Data1, Pos1);
unmarshal({struct, SubTypes}, Data, Pos) ->
    Pad = padding(8, Pos),
%%     io:format("padding2 ~p~n", [Pad]),
    << 0:Pad, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8,
    {ok, Res, Data2, Pos2} = unmarshal_struct(SubTypes, Data1, Pos1),
    {ok, list_to_tuple(Res), Data2, Pos2};
%%     {ok, Res, Data2, Pos2};

unmarshal(variant, Data, Pos) ->
    {ok, Signature, Data1, Pos1} = unmarshal(signature, Data, Pos),
    [Type] = unmarshal_signature(Signature),
    {ok, Value, Data2, Pos2} = unmarshal(Type, Data1, Pos1),
    {ok, #variant{type=Type, value=Value}, Data2, Pos2}.


unmarshal_uint(Len, Data, Pos) when is_integer(Len) ->
    Bitlen = Len * 8,
    Pad = padding(Len, Pos),
    << 0:Pad, Value:Bitlen/native-unsigned, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.

unmarshal_int(Len, Data, Pos) ->
    Bitlen = Len * 8,
    Pad = padding(Len, Pos),
    << 0:Pad, Value:Bitlen/native-signed, Data1/binary >> = Data,
    Pos1 = Pos + Pad div 8 + Len,
    {ok, Value, Data1, Pos1}.

unmarshal_signature([], Res) ->
    Res;
unmarshal_signature([$a|R], Res) ->
    [Type | Types] = unmarshal_signature(R, []),
    Res ++ [{array, Type}] ++ Types;

unmarshal_signature([$(|R], Res) ->
    {Types, Rest} = unmarshal_struct_signature(R, []),

    unmarshal_signature(Rest, Res ++ [{struct, Types}]);

unmarshal_signature([Signature|R], Res) ->
    Type = unmarshal_signature(Signature),
    unmarshal_signature(R, Res ++ [Type]).


unmarshal_struct_signature([$)|R], Res) ->
    {Res, R};
unmarshal_struct_signature([Signature|R], Res) ->
    Type = unmarshal_signature(Signature),
    unmarshal_struct_signature(R, Res ++ [Type]).
    

unmarshal_signature(Signature) when is_binary(Signature) ->
    unmarshal_signature(binary_to_list(Signature));

unmarshal_signature(Signature) when is_list(Signature) ->
    unmarshal_signature(Signature, []);

unmarshal_signature($y) ->
    byte;
unmarshal_signature($b) ->
    boolean;
unmarshal_signature($n) ->
    int16;
unmarshal_signature($q) ->
    uint16;
unmarshal_signature($i) ->
    int32;
unmarshal_signature($u) ->
    uint32;
unmarshal_signature($x) ->
    int64;
unmarshal_signature($t) ->
    uint64;
unmarshal_signature($d) ->
    double;
unmarshal_signature($s) ->
    string;
unmarshal_signature($o) ->
    object_path;
unmarshal_signature($g) ->
    signature;
%% unmarshal_signature($r) ->
%%     struct;
unmarshal_signature($v) ->
    variant;
%% unmarshal_signature($e) ->
%%     dict_entry;
unmarshal_signature(Signature) ->
    throw({bad_signature, Signature}).

unmarshal_struct(SubTypes, Data, Pos) ->
    unmarshal_struct(SubTypes, Data, [], Pos).


unmarshal_struct([], Data, Res, Pos) ->
    {ok, Res, Data, Pos};

unmarshal_struct([SubType|S], Data, Res, Pos) ->
    {ok, Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    unmarshal_struct(S, Data1, Res ++ [Value], Pos1).

unmarshal_array(SubType, Length, Data, Pos) ->
    unmarshal_array(SubType, Length, Data, [], Pos).

unmarshal_array(_SubType, 0, Data, Res, Pos) ->
    {ok, Res, Data, Pos};
unmarshal_array(SubType, Length, Data, Res, Pos) when is_integer(Length), Length > 0 ->
%%     io:format("unmarshal_array ~p~n", [[SubType, Length, Data, Res, Pos]]),
    {ok, Value, Data1, Pos1} = unmarshal(SubType, Data, Pos),
    Size = Pos1 - Pos,
    unmarshal_array(SubType, Length - Size, Data1, Res ++ [Value], Pos1).

unmarshal_list(Types, Data) when is_list(Types), is_binary(Data) ->
    {ok, Data1, Res, Pos} = unmarshal_list(Types, Data, 0),
    {ok, Data1, Res, Pos}.

unmarshal_list(Types, Data, Pos) when is_list(Types) ->
    unmarshal_list(Types, Data, [], Pos).

unmarshal_list([], Data, Res, Pos) ->
    {ok, Data, Res, Pos};
unmarshal_list([Type|T], Data, Res, Pos) ->
    {ok, Value, Data1, Pos1} = unmarshal(Type, Data, Pos),
    unmarshal_list(T, Data1, Res ++ [Value], Pos1).


unmarshal_string(LenType, Data, Pos) ->
    {ok, Length, Data1, Pos1} = unmarshal(LenType, Data, Pos),
%%     io:format("unmarshal_string ~p ~p ~p~n", [LenType, Length, Data1]),
    << String:Length/binary, 0, Data2/binary >> = Data1,
    Pos2 = Pos1 + Length + 1,
    {ok, binary_to_list(String), Data2, Pos2}.

padding(Size, Pos) ->
    mod(Size - mod(Pos, Size), Size) * 8.

mod(N, T) ->
    D = N div T,
    N - D * T.

