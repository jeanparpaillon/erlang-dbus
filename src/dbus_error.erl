-module(dbus_error).
-moduledoc """
Defines a D-Bus error struct
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    cast/1
]).

-type t() :: binary() | {binary(), binary()}.
-export_type([t/0]).

-doc """
Given message, returns error as a `dbus_error:t()`.
""".
-spec cast(dbus_message()) -> t().
cast(Message) ->
    case dbus_message:get_type(Message) of
        error ->
            Name = get_err_name(Message),
            case dbus_message:get_body(Message) of
                undefined ->
                    Name;
                Body when is_binary(Body) ->
                    {Name, Body};
                Body ->
                    ?LOG_DEBUG("Ignoring invalid error body: ~p", [Body]),
                    Name
            end;
        _ ->
            error(invalid_error_message)
    end.

%%%
%%% Private Functions
%%%
get_err_name(Message) ->
    case dbus_message:find_field(?FIELD_ERROR_NAME, Message) of
        Name when is_binary(Name) ->
            Name;
        _ ->
            error(invalid_error_message)
    end.
