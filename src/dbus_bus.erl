%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc Bus gen_server
%%

-module(dbus_bus).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([
	 connect/1,
	 stop/1
	]).

-export([
%% 	 get_object/3,
	 wait_ready/1,
	 add_match/4,
	 export_service/2,
	 unexport_service/2,
	 get_service/2,
	 release_service/2,
	 cast/2
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-export([get_bus_id/1,
         env_to_bus_id/0,
	 str_to_bus_id/1,
	 parse_params/1,
	 parse_param/1]).

-record(state, {
	  conn,
	  state,
	  waiting=[],
	  hello_ref,
	  id,
	  dbus_object,
	  owner,
	  services=[],
	  signal_handlers=[]
	 }).

-define(DEFAULT_BUS_SYSTEM, "/var/run/dbus/system_bus_socket").
-define(SESSION_ENV, "DBUS_SESSION_BUS_ADDRESS").
-define(SERVER_DELIM, $;).
-define(TRANSPORT_DELIM, $:).
-define(PARAM_DELIM, $,).
-define(KEY_DELIM, $=).

connect(BusId) when is_record(BusId, bus_id) ->
    gen_server:start_link(?MODULE, [BusId, self()], []).

stop(Bus) ->
    gen_server:cast(Bus, stop).

%% get_object(Bus, Service, Path) ->
%%     gen_server:call(Bus, {get_object, Service, Path}).

wait_ready(Bus) ->
    gen_server:call(Bus, wait_ready).

add_match(Bus, Match, Tag, Pid) ->
    gen_server:cast(Bus, {add_match, Match, Tag, Pid}).

export_service(Bus, ServiceName) ->
    gen_server:call(Bus, {export_service, ServiceName}).

unexport_service(Bus, ServiceName) ->
    gen_server:call(Bus, {unexport_service, ServiceName}).

get_service(Bus, ServiceName) ->
    gen_server:call(Bus, {get_service, ServiceName, self()}).

release_service(Bus, Service) ->
    gen_server:call(Bus, {release_service, Service, self()}).

cast(Bus, Header) ->
    gen_server:cast(Bus, {cast, Header}).

%%
%% gen_server callbacks
%%
init([BusId, Owner]) ->
    %%process_flag(trap_exit, true),
    self() ! {setup, BusId},
    {ok, #state{owner=Owner}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% handle_call({get_object, Service, Path}, From, State) ->
%%     case dbus_proxy:start_link(self(), State#state.conn, Service, Path, From) of
%% 	{ok, Obj} ->
%% 	    {noreply, State};
%% 	E ->
%% 	    {reply, E, State}
%%     end;
    
handle_call(wait_ready, _From, #state{state=up}=State) ->
    {reply, ok, State};

handle_call(wait_ready, From, State) ->
    Waiting = [ From | State#state.waiting ],
    io:format("wait_ready received ~p~n", [Waiting]),
    {noreply, State#state{waiting=Waiting}};

handle_call({export_service, ServiceName}, _From, State) ->
    BusObj = State#state.dbus_object,

    {ok, BusIface} = dbus_proxy:interface(BusObj, "org.freedesktop.DBus"),
    {ok, _Header1} = dbus_proxy:call(BusIface, 'RequestName', [ServiceName, 0]),
    {reply, ok, State};

handle_call({unexport_service, ServiceName}, _From, State) ->
    BusObj = State#state.dbus_object,

    {ok, BusIface} = dbus_proxy:interface(BusObj, "org.freedesktop.DBus"),
    {ok, _Header1} = dbus_proxy:call(BusIface, 'ReleaseName', [ServiceName]),
    {reply, ok, State};

handle_call({get_service, ServiceName, Pid}, _From, State) ->
    Services = State#state.services,
    case lists:keysearch(ServiceName, 1, Services) of
	{value, {ServiceName, Service, Pids}} ->
	    true = link(Pid),
	    Pids1 = [Pid | Pids],
	    Value = {ServiceName, Service, Pids1},
	    Services1 = lists:keyreplace(Service, 2, Services, Value),
	    {reply, {ok, Service}, State#state{services=Services1}};
	false ->
	    {ok, Service} = dbus_remote_service:start_link(self(), State#state.conn,
						      ServiceName),
	    true = link(Pid),
	    Services1 = [{ServiceName, Service, [Pid]} | Services],
	    {reply, {ok, Service}, State#state{services=Services1}}
    end;

handle_call({release_service, Service, Pid}, _From, State) ->
    error_logger:info_msg("~p: ~p release_service ~p ~p~n",
                          [?MODULE, self(), Service, Pid]),
    handle_release_service(Service, Pid, State);

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({add_match, Match, Tag, Pid}, State) ->
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

    {ok, DBusIFace} = dbus_proxy:interface(DBusObj, "org.freedesktop.DBus"),
    ok = dbus_proxy:call(DBusIFace, 'AddMatch', [MatchStr], [{reply, self(), add_match}]),

    Signal_handlers = State#state.signal_handlers,
    {noreply, State#state{signal_handlers=[{Match, Tag, Pid}|Signal_handlers]}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({cast, Header}, State) ->
    Conn = State#state.conn,
    dbus_connection:cast(Conn, Header),
    {noreply, State};

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, BusId}, State) when is_record(BusId, bus_id) ->
    {ok, Conn} = dbus_connection:start_link(BusId, [list, {packet, 0}]),
%%     ConnSpec = {{conn, DbusHost, DbusPort},{dbus_connection,start_link,[DbusHost, DbusPort, [list, {packet, 0}], self()]}, permanent, 10000, worker, [connection]},
%%     {ok, Conn} = supervisor:start_child(dbus_sup, ConnSpec),
    {noreply, State#state{conn=Conn}};

handle_info({auth_ok, Conn}, #state{conn=Conn}=State) ->
    
    DBusRootNode = default_dbus_node(),
    {ok, DBusObj} =
	dbus_proxy:start_link(self(), Conn, "org.freedesktop.DBus", "/", DBusRootNode),
    {ok, DBusIfaceObj} = dbus_proxy:interface(DBusObj, "org.freedesktop.DBus"),
    ok = dbus_proxy:call(DBusIfaceObj, 'Hello', [], [{reply, self(), hello}]),
    io:format("Call returned~n"),

    Owner = State#state.owner,
    Owner ! {bus_ready, self()},

    {noreply, State#state{state=up,
			  dbus_object=DBusObj
			 }};


handle_info({reply, hello, {ok, Reply}}, State) ->
%%     DBusObj = State#state.dbus_object,
    error_logger:info_msg("Hello reply ~p~n", [Reply]),
    [Id] = Reply,

%%     {ok, DBusIntrospectable} = dbus_proxy:interface(DBusObj, "org.freedesktop.DBus.Introspectable"),
%%     ok = dbus_proxy:call(DBusIntrospectable, 'Introspect', [], [{reply, self(), introspect}]),

%%     ok = dbus_proxy:introspect(DBusObj),

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
    dbus_service_reg ! {dbus_method_call, Header, Conn},
    {noreply, State};

handle_info({dbus_signal, Header, Conn}, #state{conn=Conn}=State) ->
    io:format("Ignore signal ~p~n", [Header]),
    
    Signal_handlers = State#state.signal_handlers,
    Fun = fun({_Match, Tag, Pid}) ->
		  Pid ! {dbus_signal, Header, Tag}
	  end,
    lists:foreach(Fun, Signal_handlers),
    {noreply, State};

%% handle_info({'EXIT', Pid, Reason}, State) ->
%%     case lists:keysearch(Pid, 2, Services)

handle_info({proxy, ok, From, Obj}, State) ->
    gen_server:reply(From, {ok, Obj}),
    {noreply, State};

handle_info({proxy, Result, From, _Obj}, State) ->
    gen_server:reply(From, Result),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:info_msg("~p: EXIT ~p ~p~n", [?MODULE, Pid, Reason]),
    case handle_release_all_services(Pid, State) of
	{ok, State1} ->
	    {noreply, State1};
	{stop, State1} ->
	    {stop, normal, State1};
	{error, not_registered, State1} ->
	    if
		Reason /= normal ->
		    {stop, Reason, State1};
		true ->
		    {noreply, State1}
	    end
    end;

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
    HelloMethod = #method{name="Hello", args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    AddMatch = #method{name="AddMatch", args=[#arg{direction=in, type="s"}], in_sig="s", in_types=[string]},
    RequestName = #method{name="RequestName", args=[#arg{direction=in, type="s"}, #arg{direction=in, type="u"}, #arg{direction=out, type="u"}], in_sig="su", in_types=[string,uint32]},
    ReleaseName = #method{name="ReleaseName", args=[#arg{direction=in, type="s"}, #arg{direction=out, type="u"}], in_sig="s", in_types=[string]},
    DBusIface = #interface{name="org.freedesktop.DBus", methods=[HelloMethod, AddMatch, RequestName, ReleaseName]},

    IntrospectMethod = #method{name="Introspect", args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    DBusIntrospectableIface = #interface{name="org.freedesktop.DBus.Introspectable", methods=[IntrospectMethod]},

    DBusRootNode = #node{elements=[], interfaces=[DBusIface, DBusIntrospectableIface]},
    DBusRootNode.


handle_release_all_services(Pid, State) ->
    error_logger:info_msg("~p: handle_release_all_services ~p~n", [?MODULE, Pid]),
    throw(unimplemented).


handle_release_service(Service, Pid, State) ->
    Services = State#state.services,
    case lists:keysearch(Service, 2, Services) of
	{value, {ServiceName, _, Pids}} ->
            case lists:delete(Pid, Pids) of
                Pids ->
                    %% Pid was not in Pids!
                    %% Do nothing
                    {reply, {error, not_registered}, State};
                [] ->
                    %% No more Pids, remove service.
                    true = unlink(Pid),
                    error_logger:info_msg("~p: ~p Service terminated ~p~n", [?MODULE, self(), ServiceName]),
                    Services1 = lists:keydelete(Service, 2, Services),
                    if
                        Services1 == [] ->
                            %%error_logger:info_msg("~p: No more services stopping ~p bus~n", [?MODULE, State#state.name]),
                            %%{stop, normal, State};
                            error_logger:warning_msg("~p: No more services TODO stop bus~n", [?MODULE]),
                            {reply, ok, State#state{services=Services1}};
                        true ->
                            {reply, ok, State#state{services=Services1}}
                    end;
                Pids1->
                    %% Update tuple with new Pids.
                    true = unlink(Pid),
                    Value = {ServiceName, Service, Pids1},
                    Services1 = lists:keyreplace(Service, 2, Services, Value),
                    {reply, ok, State#state{services=Services1}}
            end;
	false ->
	    {reply, {error, not_registered}, State}
    end.

%%     Services = State#state.services,
%%     case lists:keysearch(Service, 2, Services) of
%% 	{value, {Path, _}} ->
%% 	    true = unlink(Service),
%% 	    error_logger:info_msg("~p: Service terminated ~p ~p~n", [?MODULE, Service, Path]),
%% 	    Services1 = lists:keydelete(Service, 2, Services),
%% 	    if
%% 		Services1 == [] ->
%% 		    error_logger:info_msg("~p: No more services stopping ~p service~n", [?MODULE, State#state.name]),
%% 		    {stop, State};
%% 		true ->
%% 		    {ok, State#state{services=Services1}}
%% 	    end;
%% 	false ->
%% 	    {error, not_registered, State}
%%     end.


get_bus_id(session) ->
    [BusId|_R] = env_to_bus_id(),
    BusId;
get_bus_id(system) ->
    #bus_id{scheme=unix,options=[{path,?DEFAULT_BUS_SYSTEM}]}.

env_to_bus_id() ->
    str_to_bus_id(os:getenv(?SESSION_ENV)).

str_to_bus_id(Addr) when is_list(Addr) ->
    list_to_bus_id(string:tokens(Addr, [?SERVER_DELIM]), []).

list_to_bus_id([], Acc) ->
    lists:reverse(Acc);
list_to_bus_id([L|Rest], Acc) ->
    list_to_bus_id(Rest, [to_bus_id(L) | Acc]).

to_bus_id(Server) when is_list(Server) ->
    {Transport, [?TRANSPORT_DELIM | Params]} =
	lists:splitwith(fun(A) -> A =/= ?TRANSPORT_DELIM end, Server),
    #bus_id{scheme=list_to_existing_atom(Transport),
	    options=parse_params(Params)}.

parse_params(Params) when is_list(Params) ->
    parse_params(string:tokens(Params, [?PARAM_DELIM]), []).

parse_params([], Acc) ->
    Acc;
parse_params([Param|Rest], Acc) ->
    parse_params(Rest, [parse_param(Param) | Acc]).

parse_param(Param) when is_list(Param) ->
    {Key, [?KEY_DELIM | Value]} =
	lists:splitwith(fun(A) -> A =/= ?KEY_DELIM end, Param),
    Key_name =
        case catch list_to_existing_atom(Key) of
            {'EXIT', {badarg, Reason}} ->
                Key;
            Key_atom -> Key_atom
        end,
    {Key_name, parse_value(Key_name, Value)}.

parse_value(port, Value) ->
    list_to_integer(Value);
parse_value(_, Value) ->
    Value.
