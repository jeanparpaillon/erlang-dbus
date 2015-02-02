%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc proxy gen server representing a remote D-BUS object
%%
-module(dbus_proxy).

-include("dbus_client.hrl").

-behaviour(gen_server).

%% api
-export([
	 start_link/2,
	 start_link/3,
	 start_link/4,
	 stop/1,
	 call/2,
	 call/4,
	 cast/2,
	 cast/4,
	 connect_signal/1,
	 connect_signal/3,
	 has_interface/2,
	 node/1
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
	  node            :: dbus_node(),	% #node()
	  conn            :: dbus_connection(),
	  waiting=[]
	 }).

%%%
%%% @doc Try to connect "/"
%%%
-spec start_link(Conn :: dbus_connection(), Service :: dbus_name()) -> 
			{ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service) ->
    gen_server:start_link(?MODULE, [Conn, Service, <<"/">>], []).


-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Path :: binary()) -> 
			{ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Path) when is_binary(Path) ->
    gen_server:start_link(?MODULE, [Conn, Service, Path], []).


-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Path :: binary(), Node :: dbus_node()) -> 
			{ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Path, #dbus_node{}=Node) when is_binary(Path) ->
    gen_server:start_link(?MODULE, [Conn, Service, Path, Node], []).


-spec stop(dbus_proxy()) -> ok.
stop(Proxy) ->
    gen_server:cast(Proxy, stop).


-spec call(Proxy :: dbus_proxy(), Msg :: dbus_message()) -> {ok, term()} | {error, term()}.
call(Proxy, #dbus_message{}=Msg) ->
    gen_server:call(Proxy, {call, Msg}).

-spec call(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> 
		  ok | {ok, term()} | {error, term()}.
call(Proxy, IfaceName, MethodName, Args) when is_pid(Proxy) ->
    gen_server:call(Proxy, {method, IfaceName, MethodName, Args}).


-spec cast(Proxy :: dbus_proxy(), Msg :: dbus_message()) -> ok | {error, term()}.
cast(Proxy, #dbus_message{}=Msg) ->
    gen_server:call(Proxy, {cast, Msg}).

-spec cast(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> ok.
cast(Proxy, IfaceName, MethodName, Args) ->
    gen_server:cast(Proxy, {method, IfaceName, MethodName, Args}).


%%%
%%% Connect to every signal (ie for object manager)
%%%
-spec connect_signal(Proxy :: dbus_proxy()) -> 
			    ok | {error, term()}.
connect_signal(Proxy) ->
    gen_server:call(Proxy, connect_signal).

-spec connect_signal(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), SignalName :: dbus_name()) -> 
			    ok | {error, term()}.
connect_signal(Proxy, IfaceName, SignalName) ->
    gen_server:call(Proxy, {connect_signal, IfaceName, SignalName}).

-spec has_interface(Proxy :: dbus_proxy(), InterfaceName :: dbus_name()) -> true | false.
has_interface(Proxy, InterfaceName) ->
    gen_server:call(Proxy, {has_interface, InterfaceName}).

-spec node(Proxy :: dbus_proxy()) -> {ok, dbus_node()} | {error, term()}.
node(Proxy) ->
    gen_server:call(Proxy, node).
 
%%
%% gen_server callbacks
%%
init([Conn, Service, Path]) ->
    case do_introspect(Conn, Service, Path) of
	{ok, Node} ->
	    {ok, #state{conn=Conn, service=Service, path=Path, node=Node}};
	{error, Err} ->
	    ?error("Error introspecting object ~p: ~p~n", [Path, Err]),
	    {stop, Err}
    end;

init([Conn, Service, Path, Node]) ->
    {ok, #state{conn=Conn, service=Service, path=Path, node=Node}}.

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

handle_call(connect_signal, _From, 
	    #state{conn={dbus_bus_connection, Conn}, path=Path, service=Service}=State) ->
        Match = [{type, signal},
		 {sender, Service},
		 {path_namespace, Path}],
    case dbus_proxy:call(Conn, 'org.freedesktop.DBus', 'AddMatch', [build_match(Match, <<>>)]) of
	ok -> {reply, ok, State};
	{error, Err} -> {stop, {error, Err}, State}
    end;

handle_call({connect_signal, IfaceName, SignalName}, _From, 
	    #state{conn={dbus_bus_connection, Conn}, path=Path, service=Service}=State) ->
    Match = [{type, signal},
	     {sender, Service},
	     {interface, IfaceName},
	     {member, SignalName},
	     {path, Path}],
    case dbus_proxy:call(Conn, 'org.freedesktop.DBus', 'AddMatch', [build_match(Match, <<>>)]) of
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

handle_call(node, _From, #state{node=Node}=State) ->
    {reply, Node, State};

handle_call({call, Msg}, _From, #state{conn=Conn}=State) ->
    Ret = dbus_connection:call(Conn, Msg),
    {reply, Ret, State};

handle_call({cast, Msg}, _From, #state{conn=Conn}=State) ->
    Ret = dbus_connection:cast(Conn, Msg),
    {reply, Ret, State};

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


terminate(_Reason, #state{conn=Conn}=_State) ->
    dbus_connection:close(Conn),
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


build_match([], << ",", Match/binary >>) ->
    Match;
build_match([{Key, Value} | Rules], Acc) when is_atom(Key) ->
    build_match([{atom_to_binary(Key, utf8), Value} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) when is_atom(Value) ->
    build_match([{Key, atom_to_binary(Value, utf8)} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) ->
    build_match(Rules, << Acc/binary, ",", Key/binary, "='", Value/binary, "'">>).
