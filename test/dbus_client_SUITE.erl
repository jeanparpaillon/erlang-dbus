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

-include("dbus.hrl").

-define(SCRIPT, "example-service.py").
-define(SERVICE, <<"net.lizenn.dbus.SampleService">>).
-define(IFACE, <<"net.lizenn.dbus.SampleInterface">>).

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
	 connect_service/1,
	 walk_node/1,
	 interface/1,
	 call_method/1
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
    ,{service, [], [
		    connect_service
		   ,walk_node
		   ,interface
		   ,call_method
		   ]}
    ].


init_per_group(connect, Config) ->
    Config;
init_per_group(_Name, Config) ->
    ServicePath = get_data_path(?SCRIPT, Config),
    Pid = os:cmd(ServicePath ++ " & echo $!"),
    [ {service_pid, Pid}, {connect, true} | Config ].


end_per_group(connect, Config) ->
    Config;
end_per_group(_Name, Config) ->
    os:cmd("kill " ++ ?config(service_pid, Config)),
    ok.

init_per_testcase(_, Config) ->
    case ?config(connect, Config) of
	true ->
	    {ok, Bus} = dbus_bus_connection:connect(session),
	    [ {bus, Bus} | Config ];
	_ -> Config
    end.

end_per_testcase(_, Config) ->
    case ?config(connect, Config) of
	true -> dbus_connection:close(?config(bus, Config));
	_ -> ok
    end,
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

walk_node(Config) ->
    {ok, S} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE),
    [CPath] = dbus_proxy:children(S),
    ?assertMatch(<<"/root">>, CPath),
    {ok, Child} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, CPath),
    ?assertMatch([<<"/root/child2">>, <<"/root/child1">>], dbus_proxy:children(Child)),
    ok.

interface(Config) ->
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    ?assertMatch(true, dbus_proxy:has_interface(O, ?IFACE)),
    ?assertMatch(false, dbus_proxy:has_interface(O, <<"toto">>)),
    ok.        

call_method(Config) ->
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    ?assertMatch({ok,[<<"Hello World">>,<<" from example-service.py">>]},
		 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"])),
    ?assertMatch({error, {'org.freedesktop.DBus.InvalidParameters', _}},
		 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownMethod', _}},
		 dbus_proxy:call(O, ?IFACE, <<"bad_method">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownInterface', _}},
		 dbus_proxy:call(O, <<"net.lizenn.dbus.BadInterface">>, <<"HelloWorld">>, [])),
    ok.    

%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).
