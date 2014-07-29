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
-include("dbus_object_manager.hrl").

-behaviour(gen_server).

%% api
-export([
	 start_link/4,
	 start_link/5,
	 stop/1,
	 call/4,
	 cast/4,
	 connect_signal/3,
	 has_interface/2
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
	  waiting=[],
	  objman_state
	 }).

%%%
%%% @doc Try to connect "/"
%%%
-spec start_link(Bus :: pid(), Conn :: pid(), Service :: dbus_name(), Opts :: [dbus_proxy_opt()]) -> 
			{ok, pid()} | {error, term()}.
start_link(Bus, Conn, Service, Opts) when is_pid(Conn), is_pid(Bus) ->
    gen_server:start_link(?MODULE, [Bus, Conn, Service, <<"/">>, Opts], []).


-spec start_link(Bus :: pid(), Conn :: pid(), Service :: dbus_name(), Path :: binary(), 
		 Opts :: [dbus_proxy_opt()]) -> {ok, pid()} | {error, term()}.
start_link(Bus, Conn, Service, Path, Opts) when is_pid(Conn), is_pid(Bus), is_binary(Path) ->
    gen_server:start_link(?MODULE, [Bus, Conn, Service, Path, Opts], []).


-spec stop(pid()) -> ok.
stop(Proxy) ->
    gen_server:cast(Proxy, stop).


-spec call(Proxy :: pid(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> 
		  ok | {ok, term()} | {error, term()}.
call(Proxy, IfaceName, MethodName, Args) when is_pid(Proxy) ->
    case gen_server:call(Proxy, {method, IfaceName, MethodName, Args}) of
	ok ->
	    ok;
	{ok, Result} ->
	    {ok, Result};
	{error, Reason} ->
	    throw(Reason)
    end.


-spec cast(Proxy :: pid(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> ok.
cast(Proxy, IfaceName, MethodName, Args) ->
    gen_server:cast(Proxy, {method, IfaceName, MethodName, Args}).


-spec connect_signal(Proxy :: pid(), IfaceName :: dbus_name(), SignalName :: dbus_name()) -> ok.
connect_signal(Proxy, IfaceName, SignalName) ->
    gen_server:call(Proxy, {connect_signal, IfaceName, SignalName, self()}).

-spec has_interface(Proxy :: pid(), InterfaceName :: dbus_name()) -> true | false.
has_interface(Proxy, InterfaceName) ->
    gen_server:call(Proxy, {has_interface, InterfaceName}).

%%
%% gen_server callbacks
%%
init([Bus, Conn, Service, Path, Opts]) ->
    case proplists:get_value(node, Opts) of
	undefined ->
	    case do_introspect(Conn, Service, Path) of
		{ok, Node} ->
		    State = #state{bus=Bus, conn=Conn, service=Service, path=Path, node=Node},
		    case dbus_introspect:find_interface(Node, ?DBUS_OBJECT_MANAGER_IFACE) of
			{ok, _I} ->
			    case proplists:get_value(manager, Opts) of
				undefined ->
				    {ok, State};
				Manager ->
				    do_init_manager(Manager, proplists:get_value(env, Opts, []), State)
			    end;
			{error, _} ->
			    {ok, State}
		    end;
		{error, Err} ->
		    lager:error("Error introspecting object ~p: ~p~n", [Path, Err]),
		    {stop, Err}
	    end;
	#dbus_node{}=Node ->
	    {ok, #state{bus=Bus, conn=Conn, service=Service, path=Path, node=Node}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({method, IfaceName, MethodName, Args}, _From, #state{node=Node}=State) ->
    lager:debug("Calling ~p:~p.~p(~p)~n", [State#state.path, IfaceName, MethodName, Args]),
    case dbus_introspect:find_method(Node, IfaceName, MethodName) of
	{ok, Method} ->
	    do_method(IfaceName, Method, Args, State);
	{error, _}=Err ->
	    {reply, Err, State}
    end;

handle_call({connect_signal, IfaceName, SignalName, Pid}, _From, 
	    #state{bus=Bus, path=Path, service=Service}=State) ->
    Match = [{type, signal},
	     {sender, Service},
	     {interface, IfaceName},
	     {member, SignalName},
	     {path, Path}],
    Ret = dbus_bus:add_match(Bus, Match, Pid),
    {reply, Ret, State};

handle_call({has_interface, IfaceName}, _From, #state{node=Node}=State) ->
    case dbus_introspect:find_interface(Node, IfaceName) of
	{ok, _I} -> {reply, true, State};
	{error, _Err} -> {reply, false, State}
    end;

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

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
	  #dbus_method{name=Name, in_sig=Signature, in_types=Types}=_M, 
	  Args, #state{service=Service, conn=Conn, path=Path}=State) ->
    Msg = dbus_message:call(Service, Path, IfaceName, Name),
    case dbus_message:set_body(Signature, Types, Args, Msg) of
	#dbus_message{}=M2 ->
	    case dbus_connection:call(Conn, M2) of
		{ok, #dbus_message{body=Res}} ->
		    {reply, {ok, Res}, State};
		{error, Err} ->
		    {reply, {error, Err}, State}
	    end;
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

do_init_manager(Manager, Env, #state{service=Service, conn=Conn, path=Path}=State) ->
    lager:info("Fetch managed objects~n", []),
    case do_callback(Manager, init, [self(), Env]) of
	{ok, MState} ->
	    Msg = dbus_message:call(Service, Path, ?DBUS_OBJECT_MANAGER_IFACE, 'GetManagedObjects'),
	    case dbus_connection:call(Conn, Msg) of
		{ok, #dbus_message{body=Objects}} ->
		    case do_callback(Manager, add_objects, [Objects, MState]) of
			{ok, MState2} ->
			    {ok, State#state{objman_state=MState2}};
			{error, Err} ->
			    {stop, Err}
		    end;
		{error, Err} ->
		    {stop, Err}
	    end;
	{error, Err} ->
	    {stop, Err}
    end.

do_callback(Mod, Fun, Args) ->
    try 
	erlang:apply(Mod, Fun, Args)
    catch 
	Class:Err ->
	    {error, {Class, Err}}
    end.
