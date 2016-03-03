%%
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright Copyright 2014 Jean Parpaillon
%% @doc Implements a remote service ...
%% 
%% @todo Remember what this module does ;)
%% @end
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
	  objects
	 }).

start_link(Bus, Conn, ServiceName) ->
    gen_server:start_link(?MODULE, [Bus, Conn, ServiceName], []).


-spec get_object(dbus_name(), binary()) -> {ok, pid()} | {error, term()}.					    
get_object(Service, Path) ->
    gen_server:call(Service, {get_object, Path}).

release_object(Service, Object) ->
    gen_server:call(Service, {release_object, Object}).

%%
%% gen_server callbacks
%%
init([Bus, Conn, ServiceName]) ->
    Reg = ets:new(objects, [set, private]),
    {ok, #state{name=ServiceName, bus=Bus, conn=Conn, objects=Reg}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({get_object, Path}, {Pid, _Tag}, 
	    #state{objects=Reg, bus=Bus, conn=Conn, name=Name}=State) ->
    case ets:lookup(Reg, Path) of
	[{Path, Object, Pids}] ->
	    ets:insert(Reg, {Path, Object, sets:add_element(Pid, Pids)}),
	    {reply, {ok, Object}, State};
	[] ->
	    case dbus_proxy:start_link(Bus, Conn, Name, Path) of
		{ok, Object} ->
		    ets:insert(Reg, {Path, Object, sets:from_list([Pid])}),
		    {reply, {ok, Object}, State};
		{error, Err} ->
		    ?error("Error starting object ~p: ~p~n", [Path, Err]),
		    {reply, {error, Err}, State}
	    end
    end;

handle_call({release_object, Object}, {Pid, _}, State) ->
    case handle_release_object(Object, Pid, State) of
	{ok, State1} ->
	    {reply, ok, State1};
	{error, Reason, State1} ->
	    {reply, Reason, State1};
	{stop, State1} ->
	    {stop, normal, ok, State1}
    end;

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
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
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.

handle_release_object(Object, Pid, #state{objects=Reg}=State) ->
    ?debug("~p: ~p handle_release_object ~p~n", [?MODULE, self(), Object]),
    case ets:match_object(Reg, {'_', Object, '_'}) of
	[{Path, _, Pids}] ->
	    case sets:is_element(Pid, Pids) of
		true ->
		    true = unlink(Pid),
		    Pids2 = sets:del_element(Pid, Pids),
		    case sets:size(Pids2) of
			0 ->
						% No more pids, remove object
			    ?debug("object terminated ~p ~p~n", [Object, Path]),
			    ets:delete(Reg, Path),
			    case ets:info(Reg, size) of
				0 ->
				    ?debug("No more object in service, stopping service ~p~n", [State#state.name]),
				    {stop, State};
				_ ->
				    {ok, State}
			    end;
			_ ->
						% Update registry entry
			    ets:insert(Reg, {Path, Object, Pids2}),
			    {ok, State}
		    end;
		false ->
						% Pid was not in Pids
		    {error, not_resgitered, State}
	    end;
	[] ->
	    {error, not_registered, State}
    end.

handle_release_all_objects(_Pid, _State) ->
    throw(unimplemented).
