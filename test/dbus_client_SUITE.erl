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

-define(SCRIPT, "example-service.py").
-define(SERVICE, <<"org.lizenn.dbus.SampleService">>).
-define(IFACE, <<"org.lizenn.dbus.SampleInterface">>).

%%%
%%% CT callbacks
%%%
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 all/0,
	 groups/0,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2]).

%%%
%%% Test cases
%%%
-export([
	 connect_system/1,
	 connect_session/1,
	 connect_service/1
	]).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_all_started(dbus),
    Config.

end_per_suite(Config) ->
    application:stop(dbus),
    Config.

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
     {group, connect}
    ,{group, service}
    ].


groups() ->
    [
     {connect, [parallel, {repeat, 5}], [ connect_system, connect_session ]}
    ,{service, [], [ connect_service  ]}
    ].


init_per_group(connect, Config) ->
    Config;
init_per_group(_Name, Config) ->
    ServicePath = get_data_path(?SCRIPT, Config),
    Pid = os:cmd(ServicePath ++ " & echo $!"),
    [ {service_pid, Pid} | Config ].


end_per_group(connect, Config) ->
    Config;
end_per_group(_Name, Config) ->
    os:cmd("kill " ++ ?config(service_pid, Config)),
    ok.

init_per_testcase(connect_system, Config) ->
    Config;
init_per_testcase(connect_session, Config) ->
    Config;
init_per_testcase(_, Config) ->
    {ok, Bus} = dbus_bus_connection:connect(session),
    [ {bus, Bus} | Config ].

end_per_testcase(connect_system, Config) ->
    Config;
end_per_testcase(connect_session, Config) ->
    Config;
end_per_testcase(_, Config) ->
    dbus_connection:close(?config(bus, Config)),
    ok.

%%%
%%% Test cases
%%%
connect_session(_Config) ->
    {ok, Bus} = dbus_bus_connection:connect(session),
    ?assertMatch(ok, dbus_bus_connection:close(Bus)).

connect_system(_Config) ->
    {ok, Bus} = dbus_bus_connection:connect(system),
    ?assertMatch(ok, dbus_bus_connection:close(Bus)).

connect_service(Config) ->
    {ok, Service} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE),
    ?assert(is_pid(Service)),
    ok.

%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).
