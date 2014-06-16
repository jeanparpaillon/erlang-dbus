%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc message module. Builds error and result messages
%%

-module(dbus_message).

-include("dbus.hrl").

%% api

-export([
	 header_find/2,
	 header_fetch/2,
	 build_error/3,
	 build_method_return/3,
	 build_signal/4
	]).

build_error(Header, ErrorName, ErrorText) ->
    {_Type2, From} = dbus_message:header_fetch(?HEADER_SENDER, Header),
    Error = #variant{type=string, value=ErrorName},
    ReplySerial = #variant{type=uint32, value=Header#header.serial},

    {ok, ReplyBody, _Pos} = 
	dbus_marshaller:marshal_list([string], [ErrorText]),
    Headers = [
	       {?HEADER_ERROR_NAME, Error},
	       {?HEADER_REPLY_SERIAL, ReplySerial},
 	       {?HEADER_DESTINATION, From},
	       {?HEADER_SIGNATURE, #variant{type=signature, value="s"}}
	      ],

    ReplyHeader = #header{type=?TYPE_ERROR,
			  serial=Header#header.serial,
			  headers=Headers,
			  body=ReplyBody},
    {ok, ReplyHeader}.

build_method_return(Header, Types, Body) ->
    {_Type2, From} = dbus_message:header_fetch(?HEADER_SENDER, Header),
    ReplySerial = #variant{type=uint32, value=Header#header.serial},
    Signature = dbus_marshaller:marshal_signature(Types),

    {ok, BinBody, _Pos} = 
	dbus_marshaller:marshal_list(Types, Body),
    Headers = [
	       {?HEADER_REPLY_SERIAL, ReplySerial},
 	       {?HEADER_DESTINATION, From},
	       {?HEADER_SIGNATURE, #variant{type=signature, value=Signature}}
	      ],

    ReplyHeader = #header{type=?TYPE_METHOD_RETURN,
			  serial=Header#header.serial,
			  headers=Headers,
			  body=BinBody},
    {ok, ReplyHeader}.

build_signal(Path, Iface_name, Signal, Args) when is_atom(Path),
						  is_atom(Iface_name),
						  is_record(Signal, signal),
						  is_list(Args) ->
    Signal_name = Signal#signal.name,
    Signature = Signal#signal.out_sig,
    Types = Signal#signal.out_types,

    Signature = dbus_marshaller:marshal_signature(Types),

    {ok, Body, _Pos} = 
	dbus_marshaller:marshal_list(Types, Args),
    Headers = [
	       {?HEADER_PATH, #variant{type=object_path, value=Path}},
	       {?HEADER_INTERFACE, #variant{type=string, value=Iface_name}},
	       {?HEADER_MEMBER, #variant{type=string, value=Signal_name}},
	       {?HEADER_SIGNATURE, #variant{type=signature, value=Signature}}
	      ],

    Header = #header{type=?TYPE_SIGNAL,
		     headers=Headers,
		     body=Body},
    {ok, Header}.

header_fetch(Code, Header) ->
    {ok, Field} = header_find(Code, Header),
    Field.

header_find(Code, Header) ->
    Headers = Header#header.headers,

    case lists:keysearch(Code, 1, Headers) of
	{value, Field} ->
	    {ok, Field};
	_ ->
	    error
    end.

