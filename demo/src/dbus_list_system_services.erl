%%
%% @copyright 2006-2009 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_list_system_services).

-include("dbus.hrl").

-export([test/0]).

test() ->
    test([""]).

test([Name]) ->
    [BusId|_R] = dbus_bus:env_to_bus_id(),
    io:format("Id: ~p~n", [BusId]),
    {ok, Bus} = get_bus(BusId),

    run_test(Bus, Name),
    
    ok = dbus_bus_reg:release_bus(Bus).

get_bus(BusId) ->
    {ok, Bus} = dbus_bus_reg:get_bus(BusId),
    true = link(Bus),
    ok = dbus_bus:wait_ready(Bus),
    io:format("Ready~n"),
    {ok, Bus}.


run_test(Bus, _Name) ->
    {ok, Service} = dbus_bus:get_service(Bus, 'org.freedesktop.DBus'),
    {ok, Remote_object} = dbus_remote_service:get_object(Service, '/org/freedesktop/DBus'),
    io:format("Remote_object: ~p~n", [Remote_object]),
    {ok, Iface} = dbus_proxy:interface(Remote_object, 'org.freedesktop.DBus'),

    {ok, Reply1} = dbus_proxy:call(Iface, 'ListNames', []),
    io:format("HelloWorld 1: ~p~n", [Reply1]),

    ok = dbus_remote_service:release_object(Service, Remote_object),
    ok = dbus_bus:release_service(Bus, Service),
    ok.
