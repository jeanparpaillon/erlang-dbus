-module(dbus_bus_connection).
-moduledoc """
Implements the connection to a D-Bus bus.

Actually, the following addresses classes are supported:

- `unix`
- `tcp`

Other classes are _ignored_, in particular `kernel`.
""".

-behaviour(dbus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").
-include("dbus_dbus.hrl").
-include("dbus_introspectable.hrl").

-export([get_bus_id/1,
         connect/1,
     connect/2,
     get_unique_name/1]).

%% dbus_connection callbacks
-export([close/1,
         call/2,
         cast/2]).

-define(DEFAULT_BUS_SYSTEM, #bus_id{scheme=unix, options=[{path, <<"/var/run/dbus/system_bus_socket">>}]}).
-define(SESSION_ENV, "DBUS_SESSION_BUS_ADDRESS").


-doc "Retrieve a `bus_id` from well-known names.".
-spec get_bus_id(dbus_known_bus()) -> bus_id() | {unsupported, [bus_id()]}.
get_bus_id(session) ->
    Ids = env_to_bus_id(),
    case lists:filter(fun (#bus_id{scheme=unix}) -> true;
                          (#bus_id{scheme=tcp}) -> true;
                          (_) -> false
                      end, Ids) of
        [] -> {unsupported, Ids};
        [Id | _] -> Id
    end;

get_bus_id(system) ->
    ?DEFAULT_BUS_SYSTEM.


-doc "Start a proxy to a bus.".
-spec connect(bus_id() | dbus_known_bus()) -> {ok, dbus_connection()} | {error, term()}.
connect(#bus_id{}=BusId) ->
    connect(BusId, undefined);

connect(BusName) when BusName =:= system;
              BusName =:= session ->
    connect(get_bus_id(BusName)).

connect(#bus_id{}=BusId, ServiceReg) ->
    case dbus_peer_connection:start_link(BusId, ServiceReg) of
        {ok, {dbus_peer_connection, PConn} = Conn} ->
            case dbus_peer_connection:auth(PConn) of
                {ok, undefined} ->
                    case dbus_proxy:start_link(Conn, ?DBUS_SERVICE, <<"/">>, ?DBUS_NODE) of
                        {ok, DBus} ->
                            ConnId = hello(DBus),
                            ?debug("Hello connection id: ~p~n", [ConnId]),
                dbus_peer_connection:set_unique_name(PConn, ConnId),
                            _ = dbus_peer_connection:set_controlling_process(PConn, DBus),
                            {ok, {?MODULE, DBus}};
                        {error, Err} -> {error, Err}
                    end;
                {error, Err} -> {error, Err}
            end;
        {error, Err} -> {error, Err}
    end;

connect(BusName, ServiceReg) when BusName =:= system;
                      BusName =:= session ->
    connect(get_bus_id(BusName), ServiceReg).


-doc "Stop the bus proxy.".
-spec close({?MODULE, dbus_connection()} | dbus_connection()) -> ok.
close({?MODULE, Bus}) ->     dbus_proxy:stop(Bus);
close(Bus) ->                dbus_proxy:stop(Bus).


-doc "Send a message to the bus connection, synchronously.".
-spec call({?MODULE, dbus_connection()} | dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
call({?MODULE, Bus}, Msg) -> dbus_proxy:call(Bus, Msg);
call(Bus, Msg) ->            dbus_proxy:call(Bus, Msg).


-doc "Send a message to the bus connection, asynchronously.".
-spec cast({?MODULE, dbus_connection()} | dbus_connection(), dbus_message()) -> ok | {error, term()}.
cast({?MODULE, Bus}, Msg) -> dbus_proxy:cast(Bus, Msg);
cast(Bus, Msg) ->            dbus_proxy:cast(Bus, Msg).

-doc "Get the DBUS connection unique name.".
-spec get_unique_name({?MODULE, dbus_connection()} | dbus_connection()) -> {ok, binary()} | {error, term()}.
get_unique_name({?MODULE, Bus}) -> dbus_proxy:get_unique_name(Bus);
get_unique_name(Bus) ->            dbus_proxy:get_unique_name(Bus).

%%%
%%% Priv
%%%
env_to_bus_id() ->
    case os:getenv(?SESSION_ENV) of
        false ->
            %% No session bus advertised at all: not an error here, get_bus_id/1
            %% reports it as {unsupported, []}.
            [];
        Addr ->
            case dbus_address:parse(list_to_binary(Addr)) of
                {ok, BusIds} ->
                    BusIds;
                {error, Reason} ->
                    %% A malformed environment is a configuration bug, not one
                    %% of the alternatives a caller can fall back from.
                    error({invalid_address, ?SESSION_ENV, Reason})
            end
    end.

hello(DBusObj) ->
    {ok, Ret} = dbus_proxy:call(DBusObj, 'org.freedesktop.DBus', 'Hello', []),
    Ret.
