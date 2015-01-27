%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_client_hrl).
-define(dbus_client_hrl, true).

-include("dbus.hrl").

-type dbus_proxy() :: pid().
-type dbus_proxy_opt() :: manager
			| {node, dbus_node()}
			| {bus_proxy, dbus_proxy()}.

-type dbus_client_opt() :: {manager, dbus_name()}
			 | {handler, atom()}.

-type dbus_client_handler() :: atom().

-type dbus_client_env() :: {objects, list()}
			 | {env, any()}.

-type(dbus_bus_name() :: system | session).

-record(dbus_bus_conn, {conn     :: dbus_connection(),
			conn_id  :: binary(),
			bus      :: dbus_proxy()}).
-type(dbus_bus_conn() :: #dbus_bus_conn{}).

-endif.
