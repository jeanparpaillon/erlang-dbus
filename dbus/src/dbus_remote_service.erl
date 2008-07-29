%%
%% Unfinished
%%
-module(dbus_remote_service).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/3,
	 get_object/2,
	 release_object/2
	]).

%% gen_server callback2
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-record(state, {
	  name,
	  bus,
	  conn,
	  objects=[]
	 }).

start_link(Bus, Conn, ServiceName) ->
    gen_server:start_link(?MODULE, [Bus, Conn, ServiceName], []).

get_object(Service, Path) ->
    gen_server:call(Service, {get_object, Path, self()}).

release_object(Service, Object) ->
    gen_server:call(Service, {release_object, Object, self()}).

%%
%% gen_server callbacks
%%
init([Bus, Conn, ServiceName]) ->
    process_flag(trap_exit, true),
    State = #state{name=ServiceName, bus=Bus, conn=Conn},
    self() ! setup,
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_object, Path, Pid}, From, State) ->
    Objects = State#state.objects,
    case lists:keysearch(Path, 1, Objects) of
	{value, {Path, Object, Pids}} ->
	    true = link(Pid),
	    Pids1 = [Pid | Pids],
	    Value = {Path, Object, Pids1},
	    Objects1 = lists:keyreplace(Object, 2, Objects, Value),
	    {reply, {ok, Object}, State#state{objects=Objects1}};
	false ->
	    {ok, Object} = dbus_proxy:start_link(State#state.bus, State#state.conn,
					    State#state.name, Path, From),
	    true = link(Pid),
	    Objects1 = [{Path, Object, [Pid]} | Objects],
	    {noreply, State#state{objects=Objects1}}
    end;

handle_call({release_object, Object, Pid}, _From, State) ->
    case handle_release_object(Object, Pid, State) of
	{ok, State1} ->
	    {reply, ok, State1};
	{error, Reason, State1} ->
	    {reply, Reason, State1};
	{stop, State1} ->
	    {stop, normal, State1}
    end;

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(setup, State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    case handle_release_all_objects(Pid, State) of
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

handle_info({proxy, ok, From, Obj}, State) ->
    gen_server:reply(From, {ok, Obj}),
    {noreply, State};

handle_info({proxy, Result, From, _Obj}, State) ->
    gen_server:reply(From, Result),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.

%%
%% Return {ok, State}|{error, Reason, State}|{stop, State}
%%
%%     case handle_release_object(Object, Pid, State) of

handle_release_object(Object, _Pid, State) ->
    Objects = State#state.objects,
    case lists:keysearch(Object, 2, Objects) of
	{value, {Path, _}} ->
	    true = unlink(Object),
	    error_logger:info_msg("~p: Object terminated ~p ~p~n", [?MODULE, Object, Path]),
	    Objects1 = lists:keydelete(Object, 2, Objects),
	    if
		Objects1 == [] ->
		    error_logger:info_msg("~p: No more objects stopping ~p service~n", [?MODULE, State#state.name]),
		    {stop, State};
		true ->
		    {ok, State#state{objects=Objects1}}
	    end;
	false ->
	    {error, not_registered, State}
    end.

handle_release_all_objects(_Pid, _State) ->
    throw(unimplemented).
