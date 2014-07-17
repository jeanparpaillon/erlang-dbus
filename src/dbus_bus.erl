%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Bus gen_server
%%
-module(dbus_bus).
-compile([{parse_transform, lager_transform}]).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([
	 connect/1,
	 stop/1
	]).

-export([
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

-export([get_bus_id/1]).

-record(state, {
	  conn,
	  state,
	  hello_ref,
	  id,
	  dbus_object,
	  owner,
	  services                        :: term(), % tid()
	  signal_handlers   = []
	 }).

-define(DEFAULT_BUS_SYSTEM, #bus_id{scheme=unix,options=[{path, "/var/run/dbus/system_bus_socket"}]}).
-define(SESSION_ENV, "DBUS_SESSION_BUS_ADDRESS").
-define(SERVER_DELIM, $;).
-define(TRANSPORT_DELIM, $:).
-define(PARAM_DELIM, $,).
-define(KEY_DELIM, $=).

connect(BusId) when is_record(BusId, bus_id) ->
    gen_server:start_link(?MODULE, [BusId, self()], []).

stop(Bus) ->
    gen_server:cast(Bus, stop).

add_match(Bus, Match, Tag, Pid) ->
    gen_server:cast(Bus, {add_match, Match, Tag, Pid}).

export_service(Bus, ServiceName) ->
    gen_server:call(Bus, {export_service, ServiceName}).

unexport_service(Bus, ServiceName) ->
    gen_server:call(Bus, {unexport_service, ServiceName}).

get_service(Bus, ServiceName) ->
    gen_server:call(Bus, {get_service, ServiceName}).

release_service(Bus, Service) ->
    gen_server:call(Bus, {release_service, Service}).

cast(Bus, #dbus_message{}=Msg) ->
    gen_server:cast(Bus, Msg).

%%
%% gen_server callbacks
%%
init([BusId, Owner]) ->
    case dbus_connection:start_link(BusId, [list, {packet, 0}]) of
	{ok, Conn} ->
	    dbus_connection:auth(Conn),
	    say_hello(Conn),
	    Reg = ets:new(services, [set, private]),
	    {ok, #state{owner=Owner, conn=Conn, services=Reg}};
	ignore ->
	    ignore;
	{error, Err} -> 
	    {stop, Err}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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

handle_call({get_service, Name}, {Pid, _}, #state{conn=Conn, services=Reg}=State) ->
    Srv = case ets:lookup(Reg, Name) of
	      [{Name, Service, Pids}] ->
		  ets:insert(Reg, {Name, Service, sets:add_element(Pid, Pids)}),
		  Service;
	      [] ->
		  {ok, Service} = dbus_remote_service:start_link(self(), Conn, Name),
		  ets:insert(Reg, {Name, Service, sets:from_list([Pid])}),
		  Service
	  end,
    true = link(Pid),
    {reply, {ok, Srv}, State};

handle_call({release_service, Service}, {Pid, _}, State) ->
    lager:debug("~p: ~p release_service ~p ~p~n", [?MODULE, self(), Service, Pid]),
    handle_release_service(Service, Pid, State);

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
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

    {ok, DBusIFace} = dbus_proxy:interface(DBusObj, 'org.freedesktop.DBus'),
    ok = dbus_proxy:call(DBusIFace, 'AddMatch', [MatchStr], [{reply, self(), add_match}]),

    Signal_handlers = State#state.signal_handlers,
    {noreply, State#state{signal_handlers=[{Match, Tag, Pid}|Signal_handlers]}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(#dbus_message{}=Msg, #state{conn=Conn}=State) ->
    dbus_connection:cast(Conn, Msg),
    {noreply, State};

handle_cast(Request, State) ->
    lager:error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, BusId}, State) ->
    case dbus_connection:start_link(BusId, [list, {packet, 0}]) of
	{ok, Conn} -> {noreply, State#state{conn=Conn}};
	ignore -> {noreply, State};
	{error, Err} -> {stop, Err, State}
    end;

handle_info({reply, hello, {ok, Reply}}, State) ->
    lager:debug("Hello reply ~p~n", [Reply]),
    [Id] = Reply,
    %reply_waiting(ok, State),
    {noreply, State#state{id=Id}};

handle_info({reply, Ref, {error, Reason}}, #state{hello_ref=Ref}=State) ->
    {stop, {error, Reason}, State};

handle_info({reply, add_match, {ok, _Header}}, State) ->
    %% Ignore reply
    {noreply, State};

handle_info({dbus_method_call, Header, Conn}, State) ->
    dbus_service_reg ! {dbus_method_call, Header, Conn},
    {noreply, State};

handle_info({dbus_signal, Header, Conn}, #state{conn=Conn, signal_handlers=Handlers}=State) ->
    lager:debug("Ignore signal ~p~n", [Header]),
    lists:foreach(fun({_Match, Tag, Pid}) ->
			  Pid ! {dbus_signal, Header, Tag}
		  end, Handlers),
    {noreply, State};

handle_info({proxy, ok, From, Obj}, State) ->
    gen_server:reply(From, {ok, Obj}),
    {noreply, State};

handle_info({proxy, Result, From, _Obj}, State) ->
    gen_server:reply(From, Result),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    lager:error("~p: EXIT ~p ~p~n", [?MODULE, Pid, Reason]),
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
    lager:error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


handle_release_all_services(Pid, _State) ->
    lager:error("~p: handle_release_all_services ~p~n", [?MODULE, Pid]),
    throw(unimplemented).


handle_release_service(Service, Pid, #state{services=Reg}=State) ->
    case ets:match_object(Reg, {'_', Service, '_'}) of
	[{Name, _, Pids}] ->
	    case sets:is_element(Pid, Pids) of
		true ->
		    true = unlink(Pid),
		    Pids2 = sets:del_element(Pid, Pids),
		    case sets:size(Pids2) of
			0 ->
			    % No more pids
			    {reply, ok, State};
			_ ->
			    % Update registery entry
			    ets:insert(Reg, {Name, Service, Pids2}),
			    {reply, ok, State}
		    end;
		false ->
		    {reply, {error, not_registered}, State}
	    end;
	[] ->
	    {reply, {error, not_registered}, State}
    end.

get_bus_id(session) ->
    [BusId|_R] = env_to_bus_id(),
    BusId;
get_bus_id(system) ->
    ?DEFAULT_BUS_SYSTEM.

%%%
%%% Priv
%%%
say_hello(Conn) ->
    {ok, DBusObj} = dbus_proxy:start_link(self(), Conn, <<"org.freedesktop.DBus">>, <<"/">>),
    {ok, DBusIfaceObj} = dbus_proxy:interface(DBusObj, <<"org.freedesktop.DBus">>),
    dbus_proxy:call(DBusIfaceObj, <<"Hello">>, []).

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
            {'EXIT', {badarg, _Reason}} ->
                Key;
            Key_atom -> Key_atom
        end,
    {Key_name, parse_value(Key_name, Value)}.

parse_value(port, Value) ->
    list_to_integer(Value);
parse_value(_, Value) ->
    Value.
