-module(dbus_signal).
-moduledoc """
Defines a D-Bus signal structure
""".
-include("dbus.hrl").

-export([cast/1]).

-record(dbus_signal, {
    path = undefined :: binary() | undefined,
    interface = undefined :: binary() | undefined,
    member = undefined :: binary() | undefined
}).
-opaque t() :: #dbus_signal{}.

-export_type([t/0]).

-spec cast(dbus_message()) -> t().
cast(Message) ->
    #dbus_signal{
        path = get_binary_field(?FIELD_PATH, Message),
        interface = get_binary_field(?FIELD_INTERFACE, Message),
        member = get_binary_field(?FIELD_MEMBER, Message)
    }.

%%%
%%% Priv
%%%
get_binary_field(Field, Message) ->
    case dbus_message:find_field(Field, Message) of
        Value when is_binary(Value) ->
            Value;
        _ ->
            error({missing_field, Field})
    end.
