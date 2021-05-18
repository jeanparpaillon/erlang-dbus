-module(dbus_demo_test).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
  {ok, _} = application:ensure_all_started(dbus).

list_sytem_services_test() ->
  ok = dbus_list_system_services:test().

demo_test() ->
    ok = application:ensure_started(dbus_demo),
    dbus_demo:test(),
    dbus_example_client:test().

