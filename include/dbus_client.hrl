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
-type dbus_proxy_opt() :: {node, dbus_node()}
			| {bus_proxy, dbus_proxy()}.

-type(dbus_bus_name() :: system | session).

-endif.
