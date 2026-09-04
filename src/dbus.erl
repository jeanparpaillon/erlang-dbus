-module(dbus).
-moduledoc """
This module holds the D-Bus proxy.

Connection to a D-Bus bus is the main application entry point for interacting with the bus.
""".
-include("dbus.hrl").

-behaviour(supervisor).

-export([
    start_link/1,
    start_link/2,
    get_conn/1,
    get_proxy/1,
    stop/1
]).

-export([init/1]).

% Bus address, for instance from DBUS_SYSTEM_BUS env var
-type address() ::
    system
    | session
    | binary().

-spec start_link(address()) -> gen_server:start_ret().
start_link(Address) ->
    start_link(Address, []).

-spec start_link(address(), [dbus_connection:option()]) -> gen_server:start_ret().
start_link(Address, ConnOpts) ->
    SupRef = sup_ref(Address),
    ConnRef = conn_ref(Address),
    ProxyRef = proxy_ref(Address),
    case dbus_address:parse(resolve_address(Address)) of
        {ok, Addresses} ->
            Args = #{
                addresses => Addresses,
                conn_ref => ConnRef,
                proxy_ref => ProxyRef,
                conn_opts => ConnOpts
            },
            supervisor:start_link({local, SupRef}, ?MODULE, Args);
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_conn(address()) -> gen_server:server_ref().
get_conn(Address) ->
    conn_ref(Address).

-spec get_proxy(address()) -> gen_server:server_ref().
get_proxy(Address) ->
    proxy_ref(Address).

-spec stop(address()) -> ok.
stop(Address) ->
    supervisor:stop(sup_ref(Address)).

%%%
%%% gen_server callbacks
%%%
init(#{
    conn_ref := ConnRef,
    proxy_ref := ProxyRef,
    addresses := Addresses,
    conn_opts := ConnOpts
}) ->
    %% Registration takes the `{local, Name}' form; the refs themselves are
    %% plain names, because that is what `gen_server:call/2' accepts.
    ConnOpts1 = [{server_ref, {local, ConnRef}} | ConnOpts],
    Children = [
        #{
            id => dbus_connection,
            start => {dbus_connection, start_link, [Addresses, ConnOpts1]}
        },
        #{
            id => dbus_bus,
            start => {dbus_bus, start_link, [ConnRef, {local, ProxyRef}]}
        }
    ],
    % Use a rest_for_one strategy so that if the connection process crashes,
    % the bus proxy process is also restarted.
    SupFlags = #{strategy => rest_for_one},
    {ok, {SupFlags, Children}}.

%%%
%%% Priv
%%%
derive_name(Address, Suffix) ->
    Base = base_name(Address),
    binary_to_atom(<<Base/binary, Suffix/binary>>, utf8).

base_name(system) -> <<"dbus_system">>;
base_name(session) -> <<"dbus_session">>;
base_name(Address) when is_binary(Address) -> Address.

sup_ref(Address) ->
    derive_name(Address, <<"_sup">>).

proxy_ref(Address) ->
    derive_name(Address, <<"_proxy">>).

conn_ref(Address) ->
    derive_name(Address, <<"_conn">>).

resolve_address(system) ->
    case os:getenv("DBUS_SYSTEM_BUS_ADDRESS") of
        false ->
            % As of https://dbus.freedesktop.org/doc/dbus-specification.html#message-bus-overview
            % If the environment variable is not set, fall back to the default system bus socket.
            <<"unix:path=/var/run/dbus/system_bus_socket">>;
        Addr ->
            Addr
    end;
resolve_address(session) ->
    case os:getenv("DBUS_SESSION_BUS_ADDRESS") of
        false ->
            {ok, Uid} = dbus_auth:detect_uid(),
            BinUid = integer_to_binary(Uid),
            <<"unix:path=/run/user/", BinUid/binary, "/bus">>;
        Addr ->
            Addr
    end;
resolve_address(Address) when is_binary(Address) ->
    Address.
