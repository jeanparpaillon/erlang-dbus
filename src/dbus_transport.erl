-module(dbus_transport).
-moduledoc """
Describe callbacks for modules implementing connections.

Actually implemented by:

- `m:dbus_connection`
- `m:dbus_bus_connection`
""".

-include("dbus.hrl").
-include("dbus_client.hrl").

-callback close(Conn :: term()) -> ok.
-callback call(Conn :: term(), dbus_message()) -> {ok, term()} | {error, term()}.
-callback cast(Conn :: term(), dbus_message()) -> ok | {error, term()}.

-export([
    close/1,
    call/2,
    cast/2
]).

-doc "Close the connection.".
-spec close(dbus_connection()) -> ok.
close({Mod, Conn}) ->
    Mod:close(Conn).

-doc "Synchronously send a message.".
-spec call(dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
call({Mod, Conn}, #dbus_message{} = Msg) ->
    Mod:call(Conn, Msg).

-doc "Asynchronously send a message.".
-spec cast(dbus_connection(), dbus_message()) -> ok | {error, term()}.
cast({Mod, Conn}, #dbus_message{} = Msg) ->
    Mod:cast(Conn, Msg).
