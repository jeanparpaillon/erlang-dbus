%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc proxy gen server representing a remote D-BUS object
%%
-module(dbus_proxy).

-include("dbus_client.hrl").
-include("dbus_object_manager.hrl").

-behaviour(gen_server).

%% api
-export([
	 start_link/3,
	 start_link/4,
	 stop/1,
	 call/4,
	 cast/4,
	 connect_signal/3,
	 has_interface/2,
	 get_managed_objects/1
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
	  conn,
	  bus_proxy,
	  waiting=[],
	  objects,
	  owner
	 }).

%%%
%%% @doc Try to connect "/"
%%%
-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Opts :: [dbus_proxy_opt()]) -> 
			{ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Opts) when is_pid(Conn) ->
    gen_server:start_link(?MODULE, [Conn, Service, <<"/">>, self(), Opts], []).


-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Path :: binary(), 
		 Opts :: [dbus_proxy_opt()]) -> {ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Path, Opts) when is_pid(Conn), is_binary(Path) ->
    gen_server:start_link(?MODULE, [Conn, Service, Path, self(), Opts], []).


-spec stop(dbus_proxy()) -> ok.
stop(Proxy) ->
    gen_server:cast(Proxy, stop).


-spec call(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> 
		  ok | {ok, term()} | {error, term()}.
call(Proxy, IfaceName, MethodName, Args) when is_pid(Proxy) ->
    case gen_server:call(Proxy, {method, IfaceName, MethodName, Args}) of
	ok ->
	    ok;
	{ok, Result} ->
	    {ok, Result};
	{error, Err} ->
	    {error, Err}
    end.


-spec cast(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> ok.
cast(Proxy, IfaceName, MethodName, Args) ->
    gen_server:cast(Proxy, {method, IfaceName, MethodName, Args}).


-spec connect_signal(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), SignalName :: dbus_name()) -> ok.
connect_signal(Proxy, IfaceName, SignalName) ->
    gen_server:call(Proxy, {connect_signal, IfaceName, SignalName}).

-spec has_interface(Proxy :: dbus_proxy(), InterfaceName :: dbus_name()) -> true | false.
has_interface(Proxy, InterfaceName) ->
    gen_server:call(Proxy, {has_interface, InterfaceName}).

-spec get_managed_objects(Proxy :: dbus_proxy()) -> {ok, term()} | {error, term()}.
get_managed_objects(Proxy) ->
    gen_server:call(Proxy, get_managed_objects).

%%
%% gen_server callbacks
%%
init([Conn, Service, Path, Owner, Opts]) ->
    case proplists:get_value(node, Opts) of
	undefined ->
	    case do_introspect(Conn, Service, Path) of
		{ok, Node} ->
		    State = #state{conn=Conn, service=Service, 
				   path=Path, node=Node, owner=Owner,
				   bus_proxy=proplists:get_value(bus_proxy, Opts)},
		    case dbus_introspect:find_interface(Node, ?DBUS_OBJECT_MANAGER_IFACE) of
			{ok, _I} ->
			    case proplists:get_bool(manager, Opts) of
				false ->
				    {ok, State};
				true ->
				    do_init_manager(State)
			    end;
			{error, _} ->
			    {ok, State}
		    end;
		{error, Err} ->
		    ?error("Error introspecting object ~p: ~p~n", [Path, Err]),
		    {stop, Err}
	    end;
	#dbus_node{}=Node ->
	    {ok, #state{conn=Conn, service=Service, path=Path, 
			node=Node, owner=Owner,
			bus_proxy=proplists:get_value(bus_proxy, Opts)}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({method, IfaceName, MethodName, Args}, _From, #state{node=Node}=State) ->
    ?debug("Calling ~p:~p.~p(~p)~n", [State#state.path, IfaceName, MethodName, Args]),
    case dbus_introspect:find_method(Node, IfaceName, MethodName) of
	{ok, Method} ->
	    do_method(IfaceName, Method, Args, State);
	{error, _}=Err ->
	    {reply, Err, State}
    end;

handle_call({connect_signal, IfaceName, SignalName}, _From, 
	    #state{path=Path, service=Service, bus_proxy=DBus}=State) ->
    Match = [{type, signal},
	     {sender, Service},
	     {interface, IfaceName},
	     {member, SignalName},
	     {path, Path}],
    case dbus_proxy:call(DBus, 'org.freedesktop.DBus', 'AddMatch', [build_match(Match, <<>>)]) of
	ok ->
	    {reply, ok, State};
	{error, Err} ->
	    {stop, {error, Err}, State}
    end;

handle_call({has_interface, IfaceName}, _From, #state{node=Node}=State) ->
    case dbus_introspect:find_interface(Node, IfaceName) of
	{ok, _I} -> {reply, true, State};
	{error, _Err} -> {reply, false, State}
    end;

handle_call(get_managed_objects, _From, #state{objects=Objects}=State) ->
    {reply, {ok, Objects}, State};

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({reply, #dbus_message{body=Body}, {tag, From, Options}}, State) ->
    reply(From, {ok, Body}, Options),
    {noreply, State};

handle_info({error, #dbus_message{body=Body}=Msg, {tag, From, Options}}, State) ->
    ErrName = dbus_message:get_field_value(?FIELD_ERROR_NAME, Msg),
    reply(From, {error, {ErrName, Body}}, Options),
    {noreply, State};

handle_info(Info, State) ->
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
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
		{ok, #dbus_message{body= <<>>}} ->
		    {reply, ok, State};
		{ok, #dbus_message{body=Res}} ->
		    {reply, {ok, Res}, State};
		{error, #dbus_message{body=Body}=Ret} ->
		    #dbus_variant{value=Code} = dbus_message:get_field(?FIELD_ERROR_NAME, Ret),
		    {reply, {error, {Code, Body}}, State}
	    end;
	{error, Err} ->
	    {reply, {error, Err}, State}
    end.

do_introspect(Conn, Service, Path) ->
    ?debug("Introspecting: ~p:~p~n", [Service, Path]),
    case dbus_connection:call(Conn, dbus_message:introspect(Service, Path)) of
	{ok, #dbus_message{body=Body}} ->
	    try dbus_introspect:from_xml_string(Body) of
		    #dbus_node{}=Node -> 
			{ok, Node}
	        catch
		        _:Err ->
		            ?error("Error parsing introspection infos: ~p~n", [Err]),
		            {error, parse_error}
	        end;
	{error, #dbus_message{body=Body}=Msg} ->
	    Err = dbus_message:get_field_value(?FIELD_ERROR_NAME, Msg),
	    {error, {Err, Body}}
    end.

do_init_manager(#state{service=Service, conn=Conn, path=Path}=State) ->
    ?info("Fetch managed objects~n", []),
    do_connect_manager(State),
    Msg = dbus_message:call(Service, Path, ?DBUS_OBJECT_MANAGER_IFACE, 'GetManagedObjects'),
    case dbus_connection:call(Conn, Msg) of
	{ok, #dbus_message{body=Objects}} ->
	    {ok, State#state{objects=Objects}};
	{error, Err} ->
	    {stop, Err}
    end.


do_connect_manager(#state{service=Service, path=Path, bus_proxy=DBus}) ->
    Match = [{type, signal},
	     {sender, Service},
	     {path_namespace, Path}],
    dbus_proxy:call(DBus, 'org.freedesktop.DBus', 'AddMatch', [build_match(Match, <<>>)]).


build_match([], << ",", Match/binary >>) ->
    Match;
build_match([{Key, Value} | Rules], Acc) when is_atom(Key) ->
    build_match([{atom_to_binary(Key, utf8), Value} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) when is_atom(Value) ->
    build_match([{Key, atom_to_binary(Value, utf8)} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) ->
    build_match(Rules, << Acc/binary, ",", Key/binary, "='", Value/binary, "'">>).
