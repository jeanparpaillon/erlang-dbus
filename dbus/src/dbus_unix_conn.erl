%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc unix domain socket transport
%%
%% TODO
%%

-module(dbus_unix_conn).

%% api
-export([connect/2]).

-define(DEFAULT_BUS_SYSTEM, "/var/run/dbus/system_bus_socket").
-define(DRV, unixdom_drv).

connect(Path, Options) ->
    {ok, Port} = ?DRV:start(),
    {ok, ClntSock} = ?DRV:open(Port, Path, 0),
    {ok, ClntSockFd} = ?DRV:getfd(Port, ClntSock),
    {ok, Tcpsockfd} = ?DRV:receivefd(Port, ClntSockFd),
    ?DRV:shutdown(Port),
    dbus_tcp_conn:connect(Tcpsockfd, Options).
