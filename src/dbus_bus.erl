%%
%% @copyright 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Bus gen_server (broken)
%%
%% @todo Finish implementation, cleanup, test, ...
%% @end
-module(dbus_bus).

-include("dbus.hrl").
-include("dbus_dbus.hrl").
-include("dbus_introspectable.hrl").

-behaviour(gen_server).

%% api
-export([
	 connect/1,
	 stop/1
	]).

-export([export_service/2,
	 unexport_service/2,
	 get_service/2,
	 release_service/2,
	 cast/2]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  conn,
	  conn_name,
	  state,
	  id,
	  dbus_object,
	  owner,
	  services                        :: term(), % tid()
	  signal_handlers                 :: term() % tid()
	 }).

-define(DEFAULT_DBUS_SERVICE, 'org.freedesktop.DBus').
-define(DEFAULT_DBUS_NODE, 
	#dbus_node{elements=[], 
		   interfaces=gb_trees:from_orddict([{'org.freedesktop.DBus', ?DBUS_DBUS}, 
						     {'org.freedesktop.DBus.Introspectable', ?DBUS_INTROSPECTABLE}])}).

connect(BusId) when is_record(BusId, bus_id) ->
    gen_server:start_link(?MODULE, [BusId, self()], []).

stop(Bus) ->
    gen_server:cast(Bus, stop).

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
	    Reg = ets:new(services, [set, private]),
	    SigH = ets:new(signal_handlers, [set, private]),
	    {ok, #state{owner=Owner, conn=Conn, services=Reg, signal_handlers=SigH}};
	ignore ->
	    ignore;
	{error, Err} -> 
	    {stop, Err}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({export_service, ServiceName}, _From, #state{dbus_object=BusObj}=State) ->
    {ok, _Msg} = dbus_proxy:call(BusObj, 'org.freedesktop.DBus', 'RequestName', [ServiceName, 0]),
    {reply, ok, State};

handle_call({unexport_service, ServiceName}, _From, #state{dbus_object=BusObj}=State) ->
    {ok, _Msg} = dbus_proxy:call(BusObj, 'org.freedesktop.DBus', 'ReleaseName', [ServiceName]),
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
    ?debug("~p: ~p release_service ~p ~p~n", [?MODULE, self(), Service, Pid]),
    handle_release_service(Service, Pid, State);

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(#dbus_message{}=Msg, #state{conn=Conn}=State) ->
    dbus_connection:cast(Conn, Msg),
    {noreply, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, BusId}, State) ->
    case dbus_connection:start_link(BusId, [list, {packet, 0}]) of
	{ok, Conn} -> {noreply, State#state{conn=Conn}};
	ignore -> {noreply, State};
	{error, Err} -> {stop, Err, State}
    end;

handle_info({reply, Ref, {error, Reason}}, #state{conn_name=Ref}=State) ->
    {stop, {error, Reason}, State};

handle_info({dbus_signal, Msg, Conn}, #state{conn=Conn, signal_handlers=_Handlers}=State) ->
    ?debug("Ignore signal ~p~n", [Msg]),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    ?error("~p: EXIT ~p ~p~n", [?MODULE, Pid, Reason]),
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
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


handle_release_all_services(Pid, _State) ->
    ?error("~p: handle_release_all_services ~p~n", [?MODULE, Pid]),
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

%%%
%%% Priv
%%%
