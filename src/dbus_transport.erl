-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".
-include("dbus.hrl").

%% What a transport module's `connect/1' hands back: the socket alone. It is
%% `connect/1' here that pairs it with the module it came from, and only the
%% pair is a connection() -- the other two callbacks take that pair, since
%% dispatching to a module needs its name.
-type socket() :: socket:socket().

-record(transport, {
    mod :: module(),
    sock :: socket:socket(),
    support_unix_fd :: boolean()
}).

-opaque connection() :: #transport{}.
-export_type([socket/0, connection/0]).

-callback connect(Address :: dbus_address()) ->
    {ok, socket()}
    | {error, Reason :: term()}.

-callback support_unix_fd() -> boolean().

-export([
    connect/1,
    send/2,
    recv/2,
    close/1,
    support_unix_fd/1
]).

-spec connect(dbus_address()) ->
    {ok, connection()}
    | {error, Reason :: term()}.
connect(Address) ->
    case resolve(Address) of
        {ok, Transport} ->
            case Transport:connect(Address) of
                {ok, Conn} ->
                    T = #transport{
                        mod = Transport,
                        sock = Conn,
                        support_unix_fd = Transport:support_unix_fd()
                    },
                    {ok, T};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undefined} ->
            {error, {invalid_transport, Address}}
    end.

-spec send(connection(), binary()) ->
    ok
    | {error, Reason :: term()}.
send(#transport{sock = S}, D) ->
    case socket:send(S, D) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec recv(connection(), timeout()) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.
recv(#transport{sock = S}, Timeout) ->
    socket:recv(S, 0, [], Timeout).

-spec close(connection()) ->
    ok
    | {error, Reason :: term()}.
close(#transport{sock = S}) ->
    socket:close(S).

-spec support_unix_fd(connection()) -> boolean().
support_unix_fd(#transport{support_unix_fd = Support}) ->
    Support.

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
        <<"nonce-tcp">> -> {ok, dbus_transport_nonce_tcp};
        <<"unix">> -> {ok, dbus_transport_unix};
        _ -> {error, undefined}
    end.
