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
    SupName = sup_name(Address),
    ConnName = conn_name(Address),
    ProxyName = proxy_name(Address),
    case dbus_address:parse(resolve_address(Address)) of
        {ok, Addresses} ->
            Args = #{
                addresses => Addresses,
                conn_name => ConnName,
                proxy_name => ProxyName,
                conn_opts => ConnOpts
            },
            supervisor:start_link({local, SupName}, ?MODULE, Args);
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(pid()) -> ok.
stop(Bus) ->
    supervisor:stop(Bus).

%%%
%%% gen_server callbacks
%%%
init(#{
    conn_name := ConnName,
    proxy_name := ProxyName,
    addresses := Addresses,
    conn_opts := ConnOpts
}) ->
    ConnOpts1 = [{name, ConnName} | ConnOpts],
    ProxyOpts = [{name, ProxyName}],
    Children = [
        #{
            id => dbus_connection,
            start => {dbus_connection, start_link, [Addresses, ConnOpts1]}
        },
        #{
            id => dbus_proxy,
            start => {dbus_proxy, start_link, [ConnName, ProxyOpts]}
        }
    ],
    SupFlags = #{strategy => one_for_one},
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

sup_name(Address) ->
    derive_name(Address, <<"_sup">>).

proxy_name(Address) ->
    derive_name(Address, <<"_proxy">>).

conn_name(Address) ->
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
