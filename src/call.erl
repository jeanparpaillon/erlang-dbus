-module(call).

-include("dbus.hrl").

-compile([export_all]).

-behaviour(gen_server).

%% api
-export([start_link/3, stop/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  bus,
	  from,
	  pid
	 }).

start_link(Bus, From, Pid) ->
    {ok, Call} = gen_server:start_link(?MODULE, [Bus, From, Pid], []),
    {ok, Call}.

stop(Call) ->
    gen_server:cast(Call, stop).

reply(Call, Header, Body) ->
    gen_server:cast(Call, {reply, Header, Body}).

%%
%% gen_server callbacks
%%
init([Bus, From, Pid]) ->
    {ok, #state{bus=Bus, from=From, pid=Pid}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({reply, Header, Body}, State) ->
    Pid = State#state.pid,
    From = State#state.from,
    Pid ! {reply, Header, Body, From},
    {stop, normal, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    terminated.
