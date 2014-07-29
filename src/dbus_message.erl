%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc message module. Builds error and result messages
%%
-module(dbus_message).
-compile({parse_transform, lager_transform}).

-include("dbus.hrl").
-include("dbus_introspectable.hrl").

-export([call/4,
	 call/5,
	 signal/5,
	 signal/6,
	 error/3,
	 return/3]).

-export([get_serial/1,
	 set_serial/2,
	 find_field/2,
	 get_field/2,
	 get_field_value/2,
	 set_body/4]).

-export([introspect/2]).

%%%
%%% API
%%%
-spec call(Destination :: dbus_name(), 
	   Path        :: dbus_name(), 
	   Interface   :: dbus_name(),
	   Member      :: dbus_name()) -> dbus_message().
call(Destination, Path, Interface, Member) ->
    call(Destination, Path, Interface, Member, []).

-spec call(Destination :: dbus_name(), 
	   Path        :: dbus_name(), 
	   Interface   :: dbus_name(),
	   Member      :: dbus_name(),
	   Opts        :: [dbus_option()]) -> dbus_message().
call(Destination, Path, Interface, Member, Opts) ->
    Fields = [
	      {?FIELD_PATH, #dbus_variant{type=object_path, value=Path}},
	      {?FIELD_DESTINATION, #dbus_variant{type=string, value=Destination}},
	      {?FIELD_INTERFACE, #dbus_variant{type=string, value=Interface}},
	      {?FIELD_MEMBER, #dbus_variant{type=string, value=Member}}
	     ],
    Header = #dbus_header{type=?TYPE_METHOD_CALL,
			  flags=process_flags(Opts),
			  size=0,
			  fields=Fields},
    #dbus_message{header=Header, body= <<>>}.


-spec signal(Destination :: dbus_name(),
	     Path        :: dbus_name(),
	     Interface   :: dbus_name(),
	     Signal      :: dbus_signal(),
	     Args        :: [dbus_arg()]) -> dbus_message().
signal(Destination, Path, Interface, Signal, Args) ->
    signal(Destination, Path, Interface, Signal, Args, []).

-spec signal(Destination :: dbus_name(),
	     Path        :: dbus_name(),
	     Interface   :: dbus_name(),
	     Signal      :: dbus_signal(),
	     Args        :: [dbus_arg()],
	     Opts        :: [dbus_option()]) -> dbus_message().
signal(Destination, Path, Interface, 
       #dbus_signal{name=SigName, out_sig=Signature, out_types=Types}, Args, Opts)
  when is_list(Args) ->
    Signature = dbus_marshaller:marshal_signature(Types),
    {ok, Body, _Pos} = dbus_marshaller:marshal_list(Types, Args),
    Fields = [
	      {?FIELD_PATH, #dbus_variant{type=object_path, value=Path}},
	      {?FIELD_INTERFACE, #dbus_variant{type=string, value=Interface}},
	      {?FIELD_MEMBER, #dbus_variant{type=string, value=SigName}},
	      {?FIELD_SIGNATURE, #dbus_variant{type=signature, value=Signature}},
	      {?FIELD_DESTINATION, #dbus_variant{type=string, value=Destination}}
	     ],
    Header = #dbus_header{type=?TYPE_SIGNAL, 
			  flags=process_flags(Opts),
			  fields=Fields},
    {ok, #dbus_message{header=Header, body=Body}}.


-spec error(Orig      :: dbus_message(),
	    ErrName   :: binary(),
	    ErrText   :: binary()) -> dbus_message().
error(#dbus_message{}=Orig, ErrName, ErrText) when is_binary(ErrName), 
						   is_binary(ErrText) ->
    From = get_field(?FIELD_SENDER, Orig),
    Error = #dbus_variant{type=string, value=ErrName},
    Serial = #dbus_variant{type=uint32, value=get_serial(Orig)},

    {ok, Body, _Pos} = dbus_marshaller:marshal_list([string], [ErrText]),
    Fields = [
	      {?FIELD_ERROR_NAME, Error},
	      {?FIELD_REPLY_SERIAL, Serial},
	      {?FIELD_DESTINATION, From},
	      {?FIELD_SIGNATURE, #dbus_variant{type=signature, value="s"}}
	     ],
    Header = #dbus_header{type=?TYPE_ERROR,
			  serial=Serial,
			  fields=Fields},
    #dbus_message{header=Header, body=Body}.

-spec return(Orig       :: dbus_message(),
	     Types      :: [dbus_type()],
	     Body       :: term()) -> dbus_message().
return(#dbus_message{}=Orig, Types, Body) when is_list(Types),
					       is_binary(Body) ->
    From = get_field(?FIELD_SENDER, Orig),
    Serial = #dbus_variant{type=uint32, value=get_serial(Orig)},
    Signature = dbus_marshaller:marshal_signature(Types),

    {ok, BinBody, _Pos} = dbus_marshaller:marshal_list(Types, Body),
    Fields = [
	      {?FIELD_REPLY_SERIAL, Serial},
	      {?FIELD_DESTINATION, From},
	      {?FIELD_SIGNATURE, #dbus_variant{type=signature, value=Signature}}
	     ],
    Header = #dbus_header{type=?TYPE_METHOD_RETURN,
			  serial=Serial,
			  fields=Fields},
    #dbus_message{header=Header, body=BinBody}.

-spec get_serial(dbus_message()) -> integer().
get_serial(#dbus_message{header=#dbus_header{serial=Serial}}) ->
    Serial.

-spec set_serial(integer(), dbus_message()) -> dbus_message().
set_serial(Serial, #dbus_message{header=Header}=Message) ->
    Message#dbus_message{header=Header#dbus_header{serial=Serial}}.

-spec find_field(Code :: integer(), dbus_header() | dbus_message()) -> dbus_variant() | undefined.
find_field(Code, #dbus_message{header=Header}) ->
    find_field(Code, Header);
find_field(Code, #dbus_header{fields=Fields}) ->
    case proplists:get_value(Code, Fields) of
	undefined -> undefined;
	Val -> Val
    end.

-spec get_field(Code :: integer(), Header :: #dbus_header{}) -> dbus_variant().
get_field(Code, #dbus_message{header=Header}) ->
    get_field(Code, Header);
get_field(Code, #dbus_header{fields=Fields}) ->
    case proplists:get_value(Code, Fields) of
	undefined ->
	    throw({no_such_field, Code});
	Val -> 
	    Val
    end;
get_field(Code, _) ->
    throw({no_such_field, Code}).

-spec get_field_value(Code :: integer(), Header :: dbus_header()) -> term().
get_field_value(Code, #dbus_message{header=Header}) ->
    get_field_value(Code, Header);
get_field_value(Code, #dbus_header{}=Header) ->
    #dbus_variant{value=Val} = get_field(Code, Header),
    Val.

-spec set_body(Signature :: binary(),
	       Types     :: [dbus_type()],
	       Body      :: term(),
	       Message   :: dbus_message()) -> dbus_message().
set_body(Signature, Types, Body, #dbus_message{header=#dbus_header{fields=Fields}=Header}=Message) ->
    try	dbus_marshaller:marshal_list(Types, Body) of
	{Bin, _Pos} ->
	    Fields2 = case Signature of
			  <<>> -> 
				Fields;
			  undefined -> 
				Fields;
			  [] -> Fields;
			  O ->
				[{?FIELD_SIGNATURE, O} | Fields]
		      end,
	    Message#dbus_message{header=Header#dbus_header{fields=(Fields2)}, body=list_to_binary(Bin)}
    catch 
	'EXIT':Err ->
	    {error, {'org.freedesktop.DBus.InvalidParameters', Err}}
    end.	

%%%
%%% Common messages
%%%
-spec introspect(Service :: dbus_name(), Path :: dbus_name()) -> dbus_message().
introspect(Service, Path) ->
    dbus_message:call(Service, Path, ?DBUS_INTROSPECTABLE_IFACE, 'Introspect').

%%%
%%% Priv
%%%
process_flags(Opts) ->
    process_flags(Opts, 0).

process_flags([], Acc) ->
    Acc;
process_flags([no_reply_expected | Opts], Acc) ->
    process_flags(Opts, Acc bor ?NO_REPLY_EXPECTED);
process_flags([no_auto_start | Opts], Acc) ->
    process_flags(Opts, Acc bor ?NO_AUTO_START);
process_flags([_ | Opts], Acc) ->
    process_flags(Opts, Acc).
