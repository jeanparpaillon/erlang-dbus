%%
%% @copyright 2006-2009 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_example_client).

-include("dbus.hrl").

-export([test/0]).

test() ->
    [BusId|_R] = dbus_bus:env_to_bus_id(),
    io:format("Id: ~p~n", [BusId]),
    {ok, Bus} = get_bus(BusId),

    run_test(Bus),
    
    ok = dbus_bus_reg:release_bus(Bus).

get_bus(BusId) ->
    {ok, Bus} = dbus_bus_reg:get_bus(BusId),
    true = link(Bus),
    ok = dbus_bus:wait_ready(Bus),
    io:format("Ready~n"),
    {ok, Bus}.


run_test(Bus) ->
    {ok, Service} = dbus_bus:get_service(Bus, 'com.example.SampleService'),
    {ok, Remote_object} = dbus_remote_service:get_object(Service, '/SomeObject'),
    io:format("Remote_object: ~p~n", [Remote_object]),
    {ok, Iface} = dbus_proxy:interface(Remote_object, 'com.example.SampleInterface'),
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

    ok = dbus_remote_service:release_object(Service, Remote_object),
    ok = dbus_bus:release_service(Bus, Service),
    ok.
