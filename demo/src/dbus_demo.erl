%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_demo).

-behaviour(gen_server).

-include("dbus.hrl").

%% gen_server callbacks
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

%% api
-export([
	 start_link/0, start_link/1
	]).

-define(SERVER, ?MODULE).

-record(state, {
	  bus,
	  bus_obj
	 }).

-export([test/0, run_test/1]).

test() ->
    Pid = case start_link() of
	      {ok, Pid1} -> Pid1;
	      {error, {already_started, Pid1}} -> Pid1
	  end,
    run_test(Pid).

start_link() ->
    [BusId|_R] = dbus_bus:env_to_bus_id(),
    start_link(BusId).

start_link(BusId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BusId], []).

run_test(Pid) ->
    gen_server:call(Pid, run_test).

%%
%% gen_server callbacks
%%
init([BusId]) ->
    self() ! {setup, BusId},
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(run_test, _From, State) ->
    error_logger:info_msg("do_test: ~p~n", [catch do_test(State)]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, BusId}, State) ->
    {ok, Bus} = dbus_bus_reg:get_bus(BusId),
    true = link(Bus),
    ok = dbus_bus:wait_ready(Bus),
    io:format("Ready~n"),

    {ok, Service} = dbus_bus:get_service(Bus, 'org.freedesktop.DBus'),

    {ok, BusObj} = dbus_remote_service:get_object(Service, '/'),
    true = link(BusObj),
    io:format("BusObj: ~p~n", [BusObj]),

    {noreply, State#state{bus=Bus,
			  bus_obj=BusObj
			 }};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.

do_test(_State) ->
    exit(unimplemented).
