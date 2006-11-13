-module(dberl.bus).

-import(error_logger).
-import(gen_server).
-import(io).
-import(lists).
-import(supervisor).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([connect/2, stop/1]).

-export([get_object/3,
	 wait_ready/1,
	 add_match/2,
	 export_service/2,
	 unexport_service/2
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  conn,
	  state,
	  waiting=[],
	  hello_ref,
	  id,
	  dbus_object,
	  owner
	 }).

connect(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port, self()], []).

stop(Bus) ->
    gen_server:cast(Bus, stop).

get_object(Bus, Service, Path) ->
    gen_server:call(Bus, {get_object, Service, Path}).
%%     proxy:start_link(Bus, Service, Path).

wait_ready(Bus) ->
    gen_server:call(Bus, wait_ready).

add_match(Bus, Match) ->
    gen_server:cast(Bus, {add_match, Match}).

export_service(Bus, ServiceName) ->
    gen_server:call(Bus, {export_service, ServiceName}).

unexport_service(Bus, ServiceName) ->
    gen_server:call(Bus, {unexport_service, ServiceName}).

%%
%% gen_server callbacks
%%
init([DbusHost, DbusPort, Owner]) ->
%%     process_flag(trap_exit),
    self() ! {setup, DbusHost, DbusPort},
    {ok, #state{owner=Owner}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_object, Service, Path}, _From, State) ->
    %% FIX async
    {ok, Obj} = proxy:start_link(self(), State#state.conn, Service, Path),
    
    {reply, {ok, Obj}, State};

handle_call(wait_ready, _From, #state{state=up}=State) ->
    {reply, ok, State};

handle_call(wait_ready, From, State) ->
    Waiting = [ From | State#state.waiting ],
    io:format("wait_ready received ~p~n", [Waiting]),
    {noreply, State#state{waiting=Waiting}};

handle_call({export_service, ServiceName}, _From, State) ->
    BusObj = State#state.dbus_object,

    {ok, BusIface} = proxy:interface(BusObj, 'org.freedesktop.DBus'),
    {ok, _Header1} = proxy:call(BusIface, 'RequestName', [ServiceName, 0]),
    {reply, ok, State};

handle_call({unexport_service, ServiceName}, _From, State) ->
    BusObj = State#state.dbus_object,

    {ok, BusIface} = proxy:interface(BusObj, 'org.freedesktop.DBus'),
    {ok, _Header1} = proxy:call(BusIface, 'ReleaseName', [ServiceName]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({add_match, Match}, State) ->
    DBusObj = State#state.dbus_object,
    Fold = fun({Key, Value}, Str) ->
		   Prefix =
		       if
			   Str == "" ->
			       "";
			   true ->
			       ", "
		       end,
		   KeyStr =
		       if
			   is_atom(Key) ->
			       atom_to_list(Key);
			   is_list(Key) ->
			       Key
		       end,
		   ValueStr =
		       if
			   is_atom(Value) ->
			       atom_to_list(Value);
			   is_list(Value) ->
			       Value
		       end,

			   
		   Item = Prefix ++ KeyStr ++ "='" ++ ValueStr ++ "'",
		   Str ++ Item
	   end,

    MatchStr = lists:foldl(Fold, "", Match),

    {ok, DBusIFace} = proxy:interface(DBusObj, 'org.freedesktop.DBus'),
    ok = proxy:call(DBusIFace, 'AddMatch', [MatchStr], [{reply, self(), add_match}]),

    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, DbusHost, DbusPort}, State) ->
    {ok, Conn} = connection:start_link(DbusHost, DbusPort, [list, {packet, 0}]),
%%     ConnSpec = {{conn, DbusHost, DbusPort},{dberl.connection,start_link,[DbusHost, DbusPort, [list, {packet, 0}], self()]}, permanent, 10000, worker, [connection]},
%%     {ok, Conn} = supervisor:start_child(dberl.sup, ConnSpec),
    {noreply, State#state{conn=Conn}};

handle_info({auth_ok, Conn}, #state{conn=Conn}=State) ->
    
    DBusRootNode = default_dbus_node(),
    {ok, DBusObj} =
	proxy:start_link(self(), Conn, 'org.freedesktop.DBus', '/', DBusRootNode),
    {ok, DBusIfaceObj} = proxy:interface(DBusObj, 'org.freedesktop.DBus'),
    ok = proxy:call(DBusIfaceObj, 'Hello', [], [{reply, self(), hello}]),
    io:format("Call returned~n"),

    Owner = State#state.owner,
    Owner ! {bus_ready, self()},

    {noreply, State#state{state=up,
			  dbus_object=DBusObj
			 }};


handle_info({reply, hello, {ok, Reply}}, State) ->
%%     DBusObj = State#state.dbus_object,
    error_logger:error_msg("Hello reply ~p~n", [Reply]),
    [Id] = Reply,

%%     {ok, DBusIntrospectable} = proxy:interface(DBusObj, 'org.freedesktop.DBus.Introspectable'),
%%     ok = proxy:call(DBusIntrospectable, 'Introspect', [], [{reply, self(), introspect}]),

%%     ok = proxy:introspect(DBusObj),

    reply_waiting(ok, State),
    {noreply, State#state{id=Id}};

%% handle_info({reply, introspect, {ok, Header}}, State) ->
%%     error_logger:error_msg("Introspect reply ~p~n", [Header]),

%%     xxxxx

%%     reply_waiting(ok, State),
%%     {noreply, State#state{waiting=[]}};

handle_info({reply, Ref, {error, Reason}}, #state{hello_ref=Ref}=State) ->
    {stop, {error, Reason}, State};

handle_info({reply, add_match, {ok, _Header}}, State) ->
    %% Ignore reply
    {noreply, State};

handle_info({dbus_method_call, Header, Conn}, State) ->
    dberl.service_reg ! {dbus_method_call, Header, Conn},
    {noreply, State};

handle_info({dbus_signal, _Header, Conn}, #state{conn=Conn}=State) ->
    io:format("Ignore signal~n", []),
    {noreply, State};

%% handle_info({'EXIT', Pid, Reason}, State) ->
%%     case lists:keysearch(Pid, 2, Services)

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


reply_waiting(Reply, State) ->
    Fun = fun(From) ->
		    io:format("reply_waiting ~p~n", [From]),
		    gen_server:reply(From, Reply)
	    end,

    lists:foreach(Fun, State#state.waiting).


default_dbus_node() ->
    HelloMethod = #method{name='Hello', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    AddMatch = #method{name='AddMatch', args=[#arg{direction=in, type="s"}], in_sig="s", in_types=[string]},
    RequestName = #method{name='RequestName', args=[#arg{direction=in, type="s"}, #arg{direction=in, type="u"}, #arg{direction=out, type="u"}], in_sig="su", in_types=[string,uint32]},
    ReleaseName = #method{name='ReleaseName', args=[#arg{direction=in, type="s"}, #arg{direction=out, type="u"}], in_sig="s", in_types=[string]},
    DBusIface = #interface{name='org.freedesktop.DBus', methods=[HelloMethod, AddMatch, RequestName, ReleaseName]},

    IntrospectMethod = #method{name='Introspect', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    DBusIntrospectableIface = #interface{name='org.freedesktop.DBus.Introspectable', methods=[IntrospectMethod]},

    DBusRootNode = #node{elements=[], interfaces=[DBusIface, DBusIntrospectableIface]},
    DBusRootNode.
