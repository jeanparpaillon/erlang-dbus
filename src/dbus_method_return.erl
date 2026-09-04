-module(dbus_method_return).
-moduledoc """
Functions for D-Bus messages of type `METHOD_RETURN`.
""".
-include("dbus.hrl").

-export([
    cast/1
]).

-type t() :: term().

-spec cast(dbus_message()) -> t().
cast(Message) ->
    dbus_message:get_body(Message).
