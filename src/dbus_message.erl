%%
%% @copyright 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Build messages
%%
%% See <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-messages" >D-Bus Specification</a>
%%
%% @end
-module(dbus_message).

-include("dbus.hrl").
-include("dbus_introspectable.hrl").
-include("dbus_errors.hrl").

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
	 set_body/3,
	 set_body/4,
	 match/2,
	 type/1,
	 is_error/2]).

-export([introspect/2]).

-type type() :: ?TYPE_INVALID
	      | ?TYPE_METHOD_CALL
	      | ?TYPE_METHOD_RETURN
	      | ?TYPE_ERROR
	      | ?TYPE_SIGNAL.

-export_type([type/0]).

%%%
%%% API
%%%

%% @equiv call(Destination, Path, Interface, Member, [])
%% @end
-spec call(Destination :: dbus_name(),
	   Path        :: dbus_name(),
	   Interface   :: dbus_name(),
	   Member      :: dbus_name() | dbus_method()) -> dbus_message().
call(Destination, Path, Interface, #dbus_method{name=Name}) ->
    call(Destination, Path, Interface, Name);

call(Destination, Path, Interface, Member) ->
    call(Destination, Path, Interface, Member, []).


%% @doc Build a method call message
%% @end
-spec call(Destination :: dbus_name(),
	   Path        :: dbus_name(),
	   Interface   :: dbus_name(),
	   Member      :: dbus_name() | dbus_method(),
	   Opts        :: [dbus_option()]) -> dbus_message().
call(Destination, Path, Interface, #dbus_method{name=Name}, Opts) ->
    call(Destination, Path, Interface, Name, Opts);

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


%% @equiv signal(Destination, Path, Interface, Signal, Args, [])
%% @end
-spec signal(Destination :: dbus_name(),
	     Path        :: dbus_name(),
	     Interface   :: dbus_name(),
	     Signal      :: dbus_signal(),
	     Args        :: [dbus_arg()]) -> dbus_message().
signal(Destination, Path, Interface, Signal, Args) ->
    signal(Destination, Path, Interface, Signal, Args, []).


%% @doc Build a signal message
%% @end
-spec signal(Destination :: dbus_name(),
	     Path        :: dbus_name(),
	     Interface   :: dbus_name(),
	     Signal      :: dbus_signal(),
	     Args        :: [dbus_arg()],
	     Opts        :: [dbus_option()]) -> dbus_message().
signal(Destination, Path, Interface,
       #dbus_signal{name=SigName, out_sig=_Signature, out_types=Types}, Args, Opts)
  when is_list(Args) ->
    {Body, _Pos} = dbus_marshaller:marshal_list(Types, Args),
    Fields = destination(Destination, [
	      {?FIELD_PATH, #dbus_variant{type=object_path, value=Path}},
	      {?FIELD_INTERFACE, #dbus_variant{type=string, value=Interface}},
	      {?FIELD_MEMBER, #dbus_variant{type=string, value=SigName}},
	      {?FIELD_SIGNATURE, #dbus_variant{
              type=signature, value=dbus_marshaller:marshal_signature(Types)
            }
        }]),
    Header = #dbus_header{type=?TYPE_SIGNAL,
			  flags=process_flags(Opts),
			  fields=Fields},
    {ok, #dbus_message{header=Header, body=Body}}.

destination(undefined, Fields) -> 
  Fields;
destination(Destination, Fields) ->
  Fields ++ [{?FIELD_DESTINATION, #dbus_variant{type=string, value=Destination}}].

%% @doc Build an error message
%% @end
-spec error(Orig      :: dbus_message(),
	    ErrName   :: binary() | list(),
	    ErrText   :: binary() | list()) -> dbus_message().
error(#dbus_message{}=Orig, ErrName, ErrText) ->
    From = get_field(?FIELD_SENDER, Orig),
    {Body, _Pos} = dbus_marshaller:marshal_list([string], [ErrText]),
    Fields = [
	      {?FIELD_ERROR_NAME, #dbus_variant{type=string, value=ErrName}},
	      {?FIELD_REPLY_SERIAL, #dbus_variant{type=uint32, value=get_serial(Orig)}},
	      {?FIELD_DESTINATION, #dbus_variant{type=string, value=From}},
	      {?FIELD_SIGNATURE, #dbus_variant{type=signature, value="s"}}
	     ],
    Header = #dbus_header{type=?TYPE_ERROR,
			  fields=Fields},

    #dbus_message{header=Header, body=Body}.


%% @doc Build a return message
%% @end
-spec return(Orig       :: dbus_message(),
	     Types      :: [dbus_type()],
	     Body       :: term()) -> dbus_message().
return(#dbus_message{}=Orig, Types, Body) when is_list(Types) ->
    From = get_field(?FIELD_SENDER, Orig),
    Signature = dbus_marshaller:marshal_signature(Types),
    {BinBody, _Pos} = dbus_marshaller:marshal_list(Types, Body),
    Fields = [
	      {?FIELD_REPLY_SERIAL, #dbus_variant{type=uint32, value=get_serial(Orig)}},
	      {?FIELD_DESTINATION, #dbus_variant{type=string, value=From}},
	      {?FIELD_SIGNATURE, #dbus_variant{type=signature, value=Signature}}
	     ],
    Header = #dbus_header{type=?TYPE_METHOD_RETURN, fields=Fields},
    #dbus_message{header=Header, body=BinBody}.

%% @doc Get serial number from message
%% @end
-spec get_serial(dbus_message()) -> integer().
get_serial(#dbus_message{header=#dbus_header{serial=Serial}}) ->
    Serial.


%% @doc Set serial number of a message
%% @end
-spec set_serial(integer(), dbus_message()) -> dbus_message().
set_serial(Serial, #dbus_message{header=Header}=Message) ->
    Message#dbus_message{header=Header#dbus_header{serial=Serial}}.


%% @doc Find a specific field of a message
%%
%% Returns `undefined' if not found
%% @end
-spec find_field(Code :: integer(), dbus_header() | dbus_message()) -> dbus_variant() | undefined.
find_field(Code, #dbus_message{header=Header}) ->
    find_field(Code, Header);

find_field(Code, #dbus_header{fields=Fields}) ->
    proplists:get_value(Code, Fields, undefined).


%% @doc Get a specific field of a message.
%%
%% Throws error if not found.
%%
%% @throws {no_such_field, integer()}
%% @end
-spec get_field(Code :: integer(), Header :: #dbus_header{}) -> dbus_variant().
get_field(Code, #dbus_message{ header=Header }) ->
    get_field(Code, Header);

get_field(Code, #dbus_header{ fields=Fields }) ->
    case proplists:get_value(Code, Fields, undefined) of
	undefined ->
	    throw({no_such_field, Code});
	Val ->
	    Val
    end;

get_field(Code, _) ->
    throw({no_such_field, Code}).


%% @doc Set body of a message.
%%
%% @end
-spec set_body(Method    :: dbus_method(),
	       Body      :: term(),
	       Message   :: dbus_message()) -> dbus_message() | {error, dbus_err()}.
set_body(#dbus_method{in_sig=Signature, in_types=Types}, Body, Message) ->
    set_body(Signature, Types, Body, Message).


%% @doc Set body of a message.
%%
%% @end
-spec set_body(Signature :: binary(),
	       Types     :: [dbus_type()],
	       Body      :: term(),
	       Message   :: dbus_message()) -> dbus_message() | {error, dbus_err()}.
set_body(Signature, Types, Body, #dbus_message{header=#dbus_header{fields=Fields}=Header}=Message) ->
    try	dbus_marshaller:marshal_list(Types, Body) of
	{Bin, Pos} ->
	    Fields2 = case Signature of
			  <<>> ->       Fields;
			  undefined ->  Fields;
			  [] ->         Fields;
			  O ->		[{?FIELD_SIGNATURE, #dbus_variant{type=signature, value=O}} | Fields]
		      end,
	    Message#dbus_message{header=Header#dbus_header{fields=Fields2, size=Pos}, body=Bin}
    catch
	_:_ ->
	    {error, {'org.freedesktop.DBus.InvalidParameters', Signature}}
    end.


%% @doc Check message headers matches some values.
%%
%% '_' means the header exists with any value
%%
%% @end
-spec match(HeaderMatches :: [{integer(), dbus_name() | '_'}], dbus_message()) -> boolean().
match(HeaderMatches, #dbus_message{header=#dbus_header{fields=Fields}}) when is_list(HeaderMatches) ->
    match(true, HeaderMatches, Fields).


%% @doc Get message type
%% @end
-spec type(dbus_message()) -> type().
type(#dbus_message{header=#dbus_header{type=T}}) ->
    T.

%% @doc Check message is an error and of the given type
%% @end
-spec is_error(dbus_message(), dbus_name()) -> boolean().
is_error(Msg, ErrName) ->
    case type(Msg) of
	?TYPE_ERROR ->
	    case get_field(?FIELD_ERROR_NAME, Msg) of
		ErrName -> true;
		_ -> false
	    end;
	_ ->
	    false
    end.

%% @doc Build `Introspect' method call message
%%
%% @end
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

match(Acc, [], _Msg) ->
    Acc;
match(Acc, [ {Code, '_'} | Tail ], Fields) ->
    case proplists:get_value(Code, Fields) of
	undefined -> false;
	#dbus_variant{} -> match(Acc, Tail, Fields)
    end;
match(Acc, [ {Code, Value} | Tail ], Fields) ->
    case proplists:get_value(Code, Fields) of
	undefined -> false;
	#dbus_variant{value=Value} -> match(Acc, Tail, Fields);
	#dbus_variant{} -> false
    end;
match(_Acc, [ _ | _Tail ], _Fields) ->
    false.
