%%
%% @copyright 2006-2009 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_example_client).

-include_lib("dbus/include/dbus.hrl").

-export([test/0]).

test() ->
    {ok, BusConn} = dbus_bus_connection:connect(session),
    {ok, Bus} = gen_server:start_link(dbus_bus, [BusConn, self()], []),
    run_test(Bus),
    ok = dbus_bus_connection:close(Bus).


run_test(Bus) ->
    {ok, Service} = dbus_bus:get_service(Bus, 'com.example.SampleService'),
    {ok, RemoteObject} = dbus_remote_service:get_object(Service, '/SomeObject'),
    io:format("RemoteObject: ~p~n", [RemoteObject]),
    {ok, Iface} = dbus_proxy:interface(RemoteObject, 'com.example.SampleInterface'),
    Var = <<"Hello from Erlang">>,
    {ok, Reply1} = dbus_proxy:call(Iface, 'HelloWorld', [Var]),
    io:format("HelloWorld 1: ~p~n", [Reply1]),

    Var1 = "Hello from Erlang no 2!",
    {ok, Reply2} = dbus_proxy:call(Iface, 'HelloWorld', [Var1]),
    io:format("HelloWorld 2: '~p'~n", [Reply2]),

    {ok, Reply3} = dbus_proxy:call(Iface, 'GetTuple', []),
    io:format("GetTyple: '~p'~n", [Reply3]),

    {ok, Reply4} = dbus_proxy:call(Iface, 'GetDict', []),
    io:format("GetDict: '~p'~n", [Reply4]),

    try dbus_proxy:call(Iface, 'RaiseException', []) of
        never ->
            throw(dead_code)
    catch
        {'com.example.DemoException', Reply5} ->
            io:format("RaiseException: error '~p'~n", [Reply5])
    end,

    ok = dbus_remote_service:release_object(Service, RemoteObject),
    ok = dbus_bus:release_service(Bus, Service),
    ok.
