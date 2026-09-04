-module(dbus_bus).
-moduledoc """
dbus_bus is the first class application : proxies bus interfaces
""".
-include_lib("kernel/include/logger.hrl").

-export([start_link/2]).

-export([
    init/2,
    handle_dbus/2
]).

-define(PATH, <<"/org/freedesktop/DBus">>).
-define(INTERFACE, <<"org.freedesktop.DBus">>).
-define(MEMBER_HELLO, <<"Hello">>).
-define(SERVICE_DBUS, <<"org.freedesktop.DBus">>).

-record(state, {
    conn :: dbus_connection:connection(),
    name :: binary() | undefined
}).

%% `m:dbus' starts the connection and this proxy as siblings, so what it has
%% to hand is the connection's registered name rather than its pid. The name
%% is resolved here because a connection is a pid everywhere below: messages
%% reach a subscriber tagged `{dbus, ConnPid, _, _}', and an atom held in
%% `#state.conn' matches none of them.
-spec start_link(dbus_connection:connection() | atom(), atom()) ->
    gen_server:start_ret().
start_link(Conn, Name) ->
    Opts = [{server_name, {local, Name}}],
    case resolve(Conn) of
        undefined -> {error, {no_connection, Conn}};
        Pid -> dbus_proxy:start_link(?MODULE, [], Pid, Opts)
    end.

init(Conn, _Args) ->
    Hello = dbus_method_call:build(
        ?MEMBER_HELLO,
        ?PATH,
        [],
        [
            {interface, ?INTERFACE},
            {destination, ?SERVICE_DBUS}
        ]
    ),
    case dbus_rpc:call(Conn, Hello) of
        {ok, Name} when is_binary(Name) ->
            ?LOG_INFO("Acquired bus name ~p", [Name]),
            {ok, #state{conn = Conn, name = Name}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to acquire bus name: ~p", [Reason]),
            {error, Reason}
    end.

resolve(Conn) when is_pid(Conn) -> Conn;
resolve(Conn) when is_atom(Conn) -> whereis(Conn).

handle_dbus(Message, State) ->
    ?LOG_INFO("Received message ~p", [Message]),
    {noreply, State}.
