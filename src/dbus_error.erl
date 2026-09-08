-module(dbus_error).
-moduledoc """
Defines a D-Bus error struct
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    build/3,
    cast/1
]).

-type t() :: binary() | {binary(), binary()}.
-export_type([t/0]).

-spec build(
    Orig :: dbus_message(),
    ErrName :: binary() | list(),
    ErrText :: binary() | list()
) -> dbus_message().
build(#dbus_message{} = Orig, ErrName, ErrText) ->
    From = dbus_message:find_field(?FIELD_SENDER, Orig),
    Fields = [
        {?FIELD_ERROR_NAME, #dbus_variant{type = string, value = ErrName}},
        {?FIELD_REPLY_SERIAL, #dbus_variant{type = uint32, value = dbus_message:get_serial(Orig)}},
        {?FIELD_DESTINATION, #dbus_variant{type = string, value = From}},
        {?FIELD_SIGNATURE, #dbus_variant{type = signature, value = "s"}}
    ],
    Header = #dbus_header{
        type = ?TYPE_ERROR,
        fields = Fields
    },
    #dbus_message{header = Header, body_sig = [string], body = ErrText}.

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
