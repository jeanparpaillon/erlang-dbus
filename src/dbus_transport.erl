-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".
-include("dbus.hrl").

-type connection() :: {module(), socket:socket()}.
-export_type([connection/0]).

-callback connect(Address :: dbus_address()) ->
    {ok, connection()}
    | {error, Reason :: term()}.

-callback support_unix_fd(connection()) ->
    boolean().

-callback disable_unix_fd(connection()) -> ok.

-export([
    connect/1,
    send/2,
    recv/2,
    close/1,
    support_unix_fd/1,
    disable_unix_fd/1
]).

-spec connect(dbus_address()) ->
    {ok, connection()}
    | {error, Reason :: term()}.
connect(Address) ->
    case resolve(Address) of
        {ok, Transport} ->
            case Transport:connect(Address) of
                {ok, Conn} ->
                    {ok, {Transport, Conn}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undefined} ->
            {error, {invalid_transport, Address}}
    end.

-spec send(connection(), iodata()) ->
    ok
    | {error, Reason :: term()}.
send({_, S}, D) ->
    case socket:send(S, D) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec recv(connection(), timeout()) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.
recv({_, S}, Timeout) ->
    socket:recv(S, 0, [], Timeout).

-spec close(connection()) ->
    ok
    | {error, Reason :: term()}.
close({_, S}) ->
    socket:close(S).

-spec support_unix_fd(connection()) -> boolean().
%% The whole connection, not just the socket: the callback takes a
%% connection() and an adapter may keep per-connection state beside the
%% socket -- dbus_transport_unix does.
support_unix_fd({T, _} = Conn) ->
    T:support_unix_fd(Conn).

-spec disable_unix_fd(connection()) -> ok.
disable_unix_fd({T, _} = Conn) ->
    T:disable_unix_fd(Conn).

%%%
%%% Priv
%%%
-spec resolve(dbus_address() | module()) ->
    {ok, module()} | {error, undefined}.
resolve(Mod) when is_atom(Mod) ->
    {ok, Mod};
resolve(#dbus_address{} = Address) ->
    case dbus_address:scheme(Address) of
        <<"tcp">> -> {ok, dbus_transport_tcp};
        <<"unix">> -> {ok, dbus_transport_unix};
        _ -> {error, undefined}
    end.
