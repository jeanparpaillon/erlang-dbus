-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".
-include("dbus.hrl").

-export_type([connection/0]).

-type connection() :: term().

-callback connect(Address :: dbus_address()) ->
    {ok, Connection :: connection()}
    | {error, Reason :: term()}.

-callback send(Connection :: connection(), Data :: iodata()) ->
    ok
    | {error, Reason :: term()}.

-callback recv(
    Connection :: connection(),
    Timeout :: timeout()
) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.

-callback close(Connection :: connection()) ->
    ok
    | {error, Reason :: term()}.

-callback support_unix_fd(Connection :: connection()) ->
    boolean().

-callback disable_unix_fd(Connection :: connection()) -> ok.

-callback set_mode(Connection :: connection(), raw | line) -> ok.

-export([
    resolve/1,
    connect/2,
    send/3,
    recv/3,
    close/2,
    support_unix_fd/2,
    disable_unix_fd/2,
    set_mode/3
]).

-spec resolve(dbus_address() | module()) ->
    {ok, module()} | {error, undefined}.
resolve(mod) when is_atom(mod) ->
    {ok, mod};
resolve(#dbus_address{} = Address) ->
    case dbus_address:scheme(Address) of
        <<"tcp">> -> {ok, dbus_transport_tcp};
        <<"unix">> -> {ok, dbus_transport_unix};
        _ -> {error, undefined}
    end.

-spec connect(module(), connection()) ->
    {ok, connection()}
    | {error, Reason :: term()}.
connect(T, S) ->
    T:connect(S).

-spec send(module(), connection(), iodata()) ->
    ok
    | {error, Reason :: term()}.
send(T, S, D) ->
    T:send(S, D).

-spec recv(module(), connection(), timeout()) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.
recv(T, S, Timeout) ->
    T:recv(S, Timeout).

-spec close(module(), connection()) ->
    ok.
close(T, S) ->
    T:close(S).

-spec support_unix_fd(module(), connection()) -> boolean().
support_unix_fd(T, S) ->
    T:support_unix_fd(S).

-spec disable_unix_fd(module(), connection()) -> ok.
disable_unix_fd(T, S) ->
    T:disable_unix_fd(S).

-spec set_mode(module(), connection(), raw | line) -> ok.
set_mode(T, S, Mode) ->
    T:set_mode(S, Mode).
