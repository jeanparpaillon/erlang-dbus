-module(dbus_signal).
-moduledoc """
Defines a D-Bus signal structure
""".
-include("dbus.hrl").

-export([from_message/1]).

-record(dbus_signal, {
    path = undefined :: binary() | undefined,
    interface = undefined :: binary() | undefined,
    member = undefined :: binary() | undefined,
    body = undefined :: term() | undefined
}).
-opaque t() :: #dbus_signal{}.

-export_type([t/0]).

-spec from_message(dbus_message()) -> t().
from_message(Message) ->
    #dbus_signal{
        path = dbus_message:find_field(?FIELD_PATH, Message),
        interface = dbus_message:find_field(?FIELD_INTERFACE, Message),
        member = dbus_message:find_field(?FIELD_MEMBER, Message),
        body = dbus_message:get_body(Message)
    }.
