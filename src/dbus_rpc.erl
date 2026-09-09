-module(dbus_rpc).
-moduledoc """
This module defines a remote procedure call (RPC) interface to a D-Bus object.
""".
-include("dbus.hrl").

-export([
    call/2,
    call/3
]).

-define(DEFAULT_TIMEOUT, 5000).

-spec call(dbus_connection:connection(), dbus_message()) ->
    {ok, term()}
    | ok
    | {error, term()}.
call(Conn, Call) ->
    call(Conn, Call, ?DEFAULT_TIMEOUT).

-spec call(dbus_connection:connection(), dbus_message(), timeout()) ->
    {ok, term()}
    | ok
    | {error, term()}.
call(Conn, Call, Timeout) ->
    case dbus_message:get_type(Call) of
        method_call ->
            do_call(Conn, Call, Timeout);
        _ ->
            {error, {invalid_call, Call}}
    end.

do_call(Conn, Call, Timeout) ->
    case dbus_connection:send(Conn, Call) of
        {ok, Serial} ->
            case dbus_method_call:no_reply_expected(Call) of
                true ->
                    ok;
                false ->
                    wait_for_return(Conn, Serial, Timeout)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Selective, because the caller's mailbox is not this call's: a proxy has
%% subscribed to the connection before it calls, and the bus emits signals of
%% its own -- `NameAcquired' arrives right behind the reply to `Hello', often
%% framed from the same `recv'. Anything that is not the answer to this call
%% is left where it is, for the caller's own `handle_info/2' to see.
wait_for_return(Conn, Serial, Timeout) ->
    receive
        {dbus, Conn, method_return, Serial, Message} ->
            {ok, dbus_method_return:cast(Message)};
        {dbus, Conn, error, Serial, Message} ->
            {error, dbus_error:cast(Message)}
    after Timeout ->
        {error, timeout}
    end.
