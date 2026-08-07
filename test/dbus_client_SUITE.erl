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

-define(PYTHON_SERVICE, "example-service.py").
-define(CPP_SERVICE, "example-service/example-service").
-define(SERVICE, <<"net.lizenn.dbus.SampleService">>).
-define(IFACE, <<"net.lizenn.dbus.SampleInterface">>).

%%%
%%% CT callbacks
%%%
-compile([export_all]).

-define(dbus_session_tcp_anonymous,
	{"session_tcp_anonymous.conf", "tcp:host=localhost,bind=*,port=55555,family=ipv4"}).
-define(dbus_session_unix_anonymous,
	{"session_unix_anonymous.conf", "unix:path=/tmp/dbus-test"}).
-define(dbus_session_unix_external,
	{"session_unix_external.conf", "unix:path=/tmp/dbus-test"}).

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
     {group, connect_tcp_anonymous}
    ,{group, connect_unix_anonymous}
    ,{group, connect_unix_external}
    ,{group, service_python}
    ,{group, service_cpp}
    ].


groups() ->
    [
     {connect_tcp_anonymous, [], [ connect_session ]}
    ,{connect_unix_anonymous, [], [ connect_session ]}
    ,{connect_unix_external, [], [ connect_session ]}
    ,{service, [], [
		    connect_service
		   ,walk_node
		   ,interface
		   ,call_method
		   ,large_string
		   %%,signal_all
		   ,signal
		   ]}
    ,{service_python, [], [{group, service}]}
    ,{service_cpp, [], [{group, service}]}
    ].


init_per_group(connect_tcp_anonymous, Config) ->
    start_dbus(Config, ?dbus_session_tcp_anonymous);
init_per_group(connect_unix_anonymous, Config) ->
    start_dbus(Config, ?dbus_session_unix_anonymous);
init_per_group(connect_unix_external, Config) ->
    application:set_env(dbus, external_cookie, system_user),
    start_dbus(Config, ?dbus_session_unix_external);
init_per_group(service_python, Config) ->
    Config0 = start_dbus(Config, ?dbus_session_unix_external),
    ServicePath = get_data_path(?PYTHON_SERVICE, Config0),
    ServicePid = start_cmd(ServicePath),
    timer:sleep(1000),
    [ {service_port, ServicePid}, {connect, true} | Config0 ];
init_per_group(service_cpp, Config) ->
    Config0 = start_dbus(Config, ?dbus_session_unix_external),
    ServicePath = get_data_path(?CPP_SERVICE, Config0),
    ServicePid = start_cmd(ServicePath),
    timer:sleep(1000),
    [ {service_port, ServicePid}, {connect, true} | Config0 ];
init_per_group(_Name, Config) ->
    Config.


end_per_group(Group, Config)
  when Group =:= connect_tcp_anonymous;
       Group =:= connect_unix_anonymous;
       Group =:= connect_unix_external ->
    stop_dbus(Config),
    {return_group_result, ok};
end_per_group(_Name, Config) ->
    stop_cmd(?config(service_port, Config)),
    stop_dbus(Config),
    {return_group_result, ok}.


init_per_testcase(_, Config) ->
    case proplists:get_value(connect, Config, false) of
        true ->
            {ok, Bus} = dbus_bus_connection:connect(session),
            [ {bus, Bus} | Config ];
        _ -> Config
    end.

end_per_testcase(_, Config) ->
    case proplists:get_value(connect, Config, false) of
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
    ?assertEqual(["/tmp/dbus-test"], filelib:wildcard("/tmp/dbus-test")),
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
    ?assertMatch({ok,[<<"Hello World">>,<<" from example-service">>]},
                 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"])),
    ?assertMatch({error, {'org.freedesktop.DBus.InvalidParameters', _}},
                 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownMethod', _}},
                 dbus_proxy:call(O, ?IFACE, <<"bad_method">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownInterface', _}},
                 dbus_proxy:call(O, <<"net.lizenn.dbus.BadInterface">>, <<"HelloWorld">>, [])),
    ok.

large_string(Config) ->
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    N = 100000,
    Bin = binary:copy(<<$x>>, N),
    ?assertMatch({ok, Bin},
		 dbus_proxy:call(O, ?IFACE, <<"GetString">>, [N])).

signal_all(Config) ->
    Fun = fun (_Sender, IfaceName, <<"SampleSignal">>, Path, _Args, Pid) ->
                  ?debug("### Received signal: SampleSignal"),
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal;
              (_Sender, IfaceName, <<"SampleSignal2">>, Path, _Args, Pid) ->
                  ?debug("### Received signal: SampleSignal2"),
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal2
          end,
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    dbus_proxy:connect_signal(O, {Fun, self()}),
    dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"]),
    Wait = fun([], _F) ->
                   ok;
              ([ Signal | Tail ], F) ->
                   receive Signal -> F(Tail, F)
                   after 100 -> ?assert(false)
                   end
           end,
    Wait([ got_signal, got_signal2 ], Wait),
    ok.


signal(Config) ->
    Fun = fun (_Sender, IfaceName, <<"SampleSignal">>, Path, _Args, Pid) ->
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal;
              (_Sender, ?IFACE, <<"SampleSignal2">>, _Path, _Args, Pid) ->
                  Pid ! got_signal2
          end,
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    dbus_proxy:connect_signal(O, ?IFACE, <<"SampleSignal">>, {Fun, self()}),
    dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"]),
    Wait = fun ([], [], _F) ->
                   ok;
               ([ Signal | Signals ], [], F) ->
                   receive Signal -> F(lists:delete(Signal, Signals), [], F)
                   after 100 -> ?assert(false)
                   end;
               ([], [ BadSignal | BadSignals ], F) ->
                   receive BadSignal -> ?assert(false)
                   after 100 -> F([], BadSignals, F)
                   end;
               ([ Signal | Signals ], [ BadSignal | BadSignals ], F) ->
                   receive Signal -> F(lists:delete(Signal, Signals), [ BadSignal | BadSignals ], F);
                           BadSignal -> ?assert(false)
                   after 100 -> F([ Signal | Signals ], BadSignals, F)
                   end
           end,
    Wait([got_signal], [got_signal2], Wait),
    ok.


%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).

start_dbus(Config, {DBusConfig, DBusEnv}) ->
    File = get_data_path(DBusConfig, Config),
    Port = start_cmd("dbus-daemon --config-file=" ++ File),
    os:putenv("DBUS_SESSION_BUS_ADDRESS", DBusEnv),
    [ {dbus, Port}, {dbus_env, DBusEnv} | Config ].

stop_dbus(Config) ->
    stop_cmd(?config(dbus, Config)),
    proplists:delete(dbus_env,
		     proplists:delete(dbus, Config)).


start_cmd(Cmd) ->
    spawn(?MODULE, init_cmd, [Cmd]).

init_cmd(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    loop(Port, Cmd).


loop(Port, Cmd) ->
    receive
	{Port, {exit_status, Status}} ->
	    throw({command_error, {Cmd, Status}});
	{command, exit} ->
	    {os_pid, Pid} = erlang:port_info(Port, os_pid),
	    erlang:port_close(Port),
	    os:cmd(io_lib:format("kill -9 ~p", [Pid]));
	{command, From, pid} ->
	    {os_pid, Pid} = erlang:port_info(Port, os_pid),
	    From ! {self(), Pid},
	    loop(Port, Cmd);
	_ ->
	    loop(Port, Cmd)
    end.


stop_cmd(Pid) ->
    Pid ! {command, exit}.
