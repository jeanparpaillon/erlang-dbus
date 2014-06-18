%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc proxy gen server representing a remote D-BUS object
%%
-module(dbus_proxy).
-compile([{parse_transform, lager_transform}]).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([
	 start_link/4,
	 stop/1,
	 interface/2,
	 call/2,
	 call/3,
	 call/4,
	 cast/3,
	 connect_signal/3
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  service,				% atom() | string()
	  path,					% atom() | string()
	  node,					% #node()
	  bus,					% bus connection
	  conn,
	  waiting=[]
	 }).

-define(DBUS_HELLO_METHOD, #method{name="Hello", args=[], result=#arg{direction=out, type="s"}, 
				   in_sig="", in_types=[]}).
-define(DBUS_ADD_MATCH, #method{name="AddMatch", args=[#arg{direction=in, type="s"}], 
				in_sig="s", in_types=[string]}).
-define(DBUS_REQUEST_NAME, #method{name="RequestName", args=[#arg{direction=in, type="s"}, 
							     #arg{direction=in, type="u"}, 
							     #arg{direction=out, type="u"}], 
				   in_sig="su", in_types=[string,uint32]}).
-define(DBUS_RELEASE_NAME, #method{name="ReleaseName", args=[#arg{direction=in, type="s"}, 
							     #arg{direction=out, type="u"}], 
				   in_sig="s", in_types=[string]}).
-define(DBUS_IFACE, #interface{name="org.freedesktop.DBus",
			       methods=[?DBUS_HELLO_METHOD, ?DBUS_ADD_MATCH, 
					?DBUS_REQUEST_NAME, ?DBUS_RELEASE_NAME]}).

-define(DBUS_INTROSPECT_METHOD, #method{name="Introspect", args=[], result=#arg{direction=out, type="s"}, 
					in_sig="", in_types=[]}).
-define(DBUS_INTROSPECTABLE_IFACE, #interface{name="org.freedesktop.DBus.Introspectable", 
					      methods=[?DBUS_INTROSPECT_METHOD]}).

-define(DEFAULT_DBUS_NODE, #node{elements=[], interfaces=[?DBUS_IFACE, ?DBUS_INTROSPECTABLE_IFACE]}).

start_link(Bus, Conn, Service, Path) when is_atom(Service),
					  is_atom(Path) ->
    start_link(Bus, Conn, atom_to_list(Service), atom_to_list(Path));
start_link(Bus, Conn, Service, Path) when is_pid(Conn),
					  is_pid(Bus),
					  is_list(Service),
					  is_list(Path) ->
    gen_server:start_link(?MODULE, [Bus, Conn, Service, Path], []).


stop(Proxy) ->
    gen_server:cast(Proxy, stop).

interface(Proxy, IfaceName) when is_pid(Proxy), is_list(IfaceName) ->
    interface(Proxy, list_to_atom(IfaceName));
interface(Proxy, IfaceName) when is_pid(Proxy), is_atom(IfaceName) ->
    Iface = {interface, Proxy, IfaceName},
    {ok, Iface}.


call(Interface, MethodName) ->
    call(Interface, MethodName, []).

call(Interface, MethodName, Args) ->
    call(Interface, MethodName, Args, []).

call({interface, Proxy, IfaceName}, MethodName, Args, Options) ->
    case gen_server:call(Proxy, {method, IfaceName, MethodName, Args, Options}) of
	ok ->
	    ok;
	{ok, Result} ->
	    {ok, Result};
	{error, Reason} ->
	    throw(Reason)
    end.


cast({interface, Proxy, IfaceName}, MethodName, Args) ->
    gen_server:cast(Proxy, {method, IfaceName, MethodName, Args}).

connect_signal({interface, Proxy, IfaceName}, SignalName, Tag) ->
    gen_server:cast(Proxy, {connect_signal, IfaceName, SignalName, Tag, self()}).

%%
%% gen_server callbacks
%%
init([Bus, Conn, "org.freedesktop.DBus", "/"]) ->
    {ok, #state{bus=Bus, conn=Conn, service="org.freedesktop.DBus", path="/", node=?DEFAULT_DBUS_NODE}};
init([Bus, Conn, Service, Path]) ->
    Node = do_introspect(Conn, Service, Path),
    {ok, #state{bus=Bus, conn=Conn, service=Service, path=Path, node=Node}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({method, IfaceName, MethodName, Args, Options}, From, State) ->
    lager:debug("in gen_server call ~p~n", [MethodName]),
    Method =
	case dbus_introspect:find_interface(IfaceName, State#state.node) of
	    {ok, Iface} ->
		case dbus_introspect:find_method(MethodName, Iface) of
		    {ok, Method1} ->
			Method1;
		    error ->
			{error, {'org.freedesktop.DBus.UnknownMethod',  [MethodName], IfaceName, State#state.node}}
		end;
	    error ->
		{error, {'org.freedesktop.DBus.UnknownInterface',  [IfaceName]}}
	end,

    case Method of
	{error, _}=Error ->
	    {reply, Error, State}; 
	_ ->
	    do_method(IfaceName, Method, Args, Options, From, State)
    end;

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({connect_signal, IfaceName, SignalName, Tag, Pid}, State) ->
    Match = [{type, signal},
	     {sender, State#state.service},
	     {interface, IfaceName},
	     {member, SignalName},
	     {path, State#state.path}],

    bus:add_match(State#state.bus, Match, Tag, Pid),

    {noreply, State};
handle_cast(Request, State) ->
    lager:error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({reply, Header, {tag, From, Options}}, State) ->
    reply(From, {ok, Header#header.body}, Options),
    {noreply, State};

handle_info({error, Header, {tag, From, Options}}, State) ->
    {_Type1, ErrorName} = dbus_message:header_fetch(?HEADER_ERROR_NAME, Header),
    ErrorName1 = list_to_atom(ErrorName#variant.value),

    reply(From, {error, {ErrorName1, Header#header.body}}, Options),
    {noreply, State};

handle_info(Info, State) ->
    lager:error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.

reply(From, Reply, Options) ->
    case lists:keysearch(reply, 1, Options) of
	{value, {reply, Pid, Ref}} ->
	    Pid ! {reply, Ref, Reply};
	_ ->
	    gen_server:reply(From, Reply)
    end,
    ok.

do_method(IfaceName, Method, Args, Options, From, State) ->
    MethodName = Method#method.name,
    Signature = Method#method.in_sig,
    Types = Method#method.in_types,

    Service = State#state.service,
    Path = State#state.path,
    Conn = State#state.conn,

    Headers = [
	       {?HEADER_PATH, #variant{type=object_path, value=Path}},
	       {?HEADER_INTERFACE, #variant{type=string, value=IfaceName}},
	       {?HEADER_MEMBER, #variant{type=string, value=MethodName}},
	       {?HEADER_DESTINATION, #variant{type=string, value=Service}},
	       {?HEADER_SIGNATURE, #variant{type=signature, value=Signature}}
	      ],

    case catch dbus_marshaller:marshal_list(Types, Args) of
	{ok, Body, _Pos} ->
	    Header = #header{type=?TYPE_METHOD_CALL,
			     headers=Headers,
			     body=Body},

	    ok = dbus_connection:call(Conn, Header, {tag, From, Options}),
	    case lists:keysearch(reply, 1, Options) of
		{value, {reply, _Pid, _Ref}} ->
		    {reply, ok, State};
		_ ->
		    {noreply, State}
	    end;
	{'EXIT', Reason} ->
	    {reply, {error, {'org.freedesktop.DBus.InvalidParameters', Reason}}, State}
    end.

do_introspect(Conn, Service, Path) ->
    lager:debug("Introspecting: ~p:~p~n", [Service, Path]),
    case dbus_connection:call(Conn, dbus_introspect:build_introspect(Service, Path)) of
	{ok, Header} ->
	    [XmlBody] = Header#header.body,
	    {ok, dbus_introspect:from_xml_string(XmlBody)};
	{error, Header} ->
	    {_Type1, Err} = dbus_message:header_fetch(?HEADER_ERROR_NAME, Header),
	    Err1 = list_to_atom(Err#variant.value),
	    {error, {Err1, Header#header.body}}
    end.
