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
-define(SERVICE, <<"org.freedesktop.DBus">>).
-define(PATH, <<"/">>).

-define(DBUS_HELLO_METHOD, #dbus_method{name = <<"Hello">>, args=[], result=#dbus_arg{direction=out, type = <<"s">>}, 
					in_sig = <<>>, in_types=[]}).
-define(DBUS_ADD_MATCH, #dbus_method{name = <<"AddMatch">>, args=[#dbus_arg{direction=in, type= <<"s">>}], 
				     in_sig = <<"s">>, in_types=[string]}).
-define(DBUS_REQUEST_NAME, #dbus_method{name = <<"RequestName">>, args=[#dbus_arg{direction=in, type = <<"s">>}, 
								  #dbus_arg{direction=in, type = <<"u">>}, 
								  #dbus_arg{direction=out, type = <<"u">>}], 
					in_sig = <<"su">>, in_types=[string,uint32]}).
-define(DBUS_RELEASE_NAME, #dbus_method{name = <<"ReleaseName">>, args=[#dbus_arg{direction=in, type = <<"s">>}, 
								  #dbus_arg{direction=out, type = <<"u">>}], 
					in_sig = <<"s">>, in_types=[string]}).
-define(DBUS_IFACE, #dbus_iface{name = <<"org.freedesktop.DBus">>,
				methods=[?DBUS_HELLO_METHOD, ?DBUS_ADD_MATCH, 
					 ?DBUS_REQUEST_NAME, ?DBUS_RELEASE_NAME]}).

-define(DBUS_INTROSPECT_METHOD, #dbus_method{name = <<"Introspect">>, args=[], result=#dbus_arg{direction=out, type = <<"s">>}, 
					     in_sig = <<>>, in_types=[]}).
-define(DBUS_INTROSPECTABLE_IFACE, #dbus_iface{name = <<"org.freedesktop.DBus.Introspectable">>, 
					       methods=[?DBUS_INTROSPECT_METHOD]}).

-define(DEFAULT_DBUS_NODE, #dbus_node{elements=[], interfaces=[?DBUS_IFACE, ?DBUS_INTROSPECTABLE_IFACE]}).

-spec start_link(Bus :: pid(), Conn :: pid(), Service :: dbus_name(), Path :: dbus_name()) -> {ok, pid()} | {error, term()}.
start_link(Bus, Conn, Service, Path) when is_pid(Conn),
					  is_pid(Bus) ->
    gen_server:start_link(?MODULE, [Bus, Conn, Service, Path], []).


stop(Proxy) ->
    gen_server:cast(Proxy, stop).

interface(Proxy, IfaceName) when is_pid(Proxy) ->
    Iface = {interface, Proxy, IfaceName},
    {ok, Iface}.


call(Interface, MethodName) ->
    call(Interface, MethodName, []).

call({interface, Proxy, IfaceName}, MethodName, Args) ->
    case gen_server:call(Proxy, {method, IfaceName, MethodName, Args}) of
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
init([Bus, Conn, ?SERVICE, ?PATH]) ->
    {ok, #state{bus = Bus, conn = Conn, service = ?SERVICE, path = ?PATH, node = ?DEFAULT_DBUS_NODE}};

init([Bus, Conn, Service, Path]) ->
    case do_introspect(Conn, Service, Path) of
	    {ok, Node} ->
	        {ok, #state{bus=Bus, conn=Conn, service=Service, path=Path, node=Node}};
	    {error, Err} ->
	        lager:error("Error introspecting object ~p: ~p~n", [Path, Err]),
	        {error, Err}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({method, IfaceName, MethodName, Args}, _From, #state{node=Node}=State) ->
    lager:debug("Calling ~p.~p on ~p~n", [IfaceName, MethodName, State#state.path]),
    Method =
	    case dbus_introspect:find_interface(IfaceName, Node) of
	        {ok, Iface} ->
		        case dbus_introspect:find_method(MethodName, Iface) of
		            {ok, Method1} ->
			            Method1;
		            error ->
			            {error, {<<"org.freedesktop.DBus.UnknownMethod">>,  [MethodName], IfaceName, State#state.node}}
		        end;
	        error ->
		        {error, {<<"org.freedesktop.DBus.UnknownInterface">>,  [IfaceName]}}
	    end,

    case Method of
	    {error, _}=Error ->
	        {reply, Error, State}; 
	    _ ->
	        do_method(IfaceName, Method, Args, State)
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


handle_info({reply, #dbus_message{body=Body}, {tag, From, Options}}, State) ->
    reply(From, {ok, Body}, Options),
    {noreply, State};

handle_info({error, #dbus_message{body=Body}=Msg, {tag, From, Options}}, State) ->
    ErrName = dbus_message:get_field_value(?FIELD_ERROR_NAME, Msg),
    reply(From, {error, {ErrName, Body}}, Options),
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

do_method(IfaceName, 
	  #dbus_method{name=Name, in_sig=Signature, in_types=Types}, 
	  Args, #state{service=Service, conn=Conn, path=Path}=State) ->
    Msg = dbus_message:call(Service, Path, IfaceName, Name),
    case dbus_message:set_body(Signature, Types, Args, Msg) of
	#dbus_message{}=M2 ->
	    {reply, dbus_connection:call(Conn, M2), State};
	{error, Err} ->
	    {reply, {error, Err}, State}
    end.

do_introspect(Conn, Service, Path) ->
    lager:debug("Introspecting: ~p:~p~n", [Service, Path]),
    case dbus_connection:call(Conn, dbus_message:introspect(Service, Path)) of
	{ok, #dbus_message{body=Body}} ->
	    try dbus_introspect:from_xml_string(Body) of
		    #dbus_node{}=Node -> 
			{ok, Node}
	        catch
		        _:Err ->
		            lager:error("Error parsing introspection infos: ~p~n", [Err]),
		            {error, parse_error}
	        end;
	{error, #dbus_message{body=Body}=Msg} ->
	    Err = dbus_message:get_field_value(?FIELD_ERROR_NAME, Msg),
	    {error, {Err, Body}}
    end.
