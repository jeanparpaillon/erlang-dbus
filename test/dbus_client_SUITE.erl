%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  26 Jan 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(dbus_client_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-include("../include/dbus_client.hrl").

-define(SCRIPT, "example-service.py").
-define(SERVICE, "org.lizenn.dbus.SampleService").
-define(IFACE, "org.lizenn.dbus.SampleInterface").

%%% dbus_client callbacks
-export([init/2,
	 signal/5]).

%%%
%%% CT callbacks
%%%
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 all/0]).

%%%
%%% Test cases
%%%
-export([connect/1]).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    ServicePath = get_data_path(?SCRIPT, Config),
    Pid = os:cmd(ServicePath ++ " & echo $!"),
    application:ensure_all_started(dbus),
    [ {service_pid, Pid} | Config ].

end_per_suite(Config) ->
    application:stop(dbus),
    os:cmd("kill " ++ proplists:get_value(service_pid, Config)),
    ok.

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     connect 
    ].


%%%
%%% Test cases
%%%
connect(_Config) ->
    ?assertMatch({ok, Pid} when is_pid(Pid), dbus_client:start_link(session, ?MODULE, [], [])).

%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join([DataDir, Path]).

%%%
%%% dbus_client callbacks
%%%
init(_Bus, _Env) -> {ok, []}.

signal(_Path, _IfaceName, _Signal, _Args, State) -> {ok, State}.
