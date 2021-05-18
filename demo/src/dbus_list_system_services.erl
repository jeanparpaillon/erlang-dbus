%%
%% @copyright 2006-2009 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_list_system_services).

-include_lib("dbus/include/dbus.hrl").

-export([test/0]).

test() ->
    test([""]).

test([Name]) ->
    {ok, Bus} = dbus_bus_reg:get_bus(session),
    true = link(Bus),

    run_test(Bus, Name),

    ok = dbus_bus_reg:release_bus(Bus).

run_test(Bus, _Name) ->
    {ok, Service} = dbus_bus:get_service(Bus, 'org.freedesktop.DBus'),
    {ok, Remote_object} = dbus_remote_service:get_object(Service, '/org/freedesktop/DBus'),
    {ok, Iface} = dbus_proxy:interface(Remote_object, 'org.freedesktop.DBus'),
    {ok, Reply1} = dbus_proxy:call(Iface, 'ListNames', []),
    error_logger:info_msg("ListNames: ~p~n", [lists:sort(Reply1)]),

    ok = dbus_remote_service:release_object(Service, Remote_object),
    ok = dbus_bus:release_service(Bus, Service),
    ok.
