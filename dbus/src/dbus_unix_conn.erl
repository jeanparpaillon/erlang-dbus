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

-define(DRV, unixdom_drv).

-define(IS_SERVER, 1).
-define(IS_ABSTRACT, 2).
-define(IS_NULLTERM, 4).

connect(BusOptions, Options) ->
    Path =
	case lists:keysearch(path, 1, BusOptions) of
	    {value, {_, Path1}} ->
		Path1;
	    _ ->
		case lists:keysearch(abstract, 1, BusOptions) of
		    {value, {_, Path2}} ->
			%[$\0 | Path2];
			Path2;
		    _ ->
			throw(no_path)
		end
	end,

    {ok, Port} = ?DRV:start(),
    {ok, ClntSock} = ?DRV:open(Port, Path, ?IS_ABSTRACT bor ?IS_NULLTERM),
    {ok, ClntSockFd} = ?DRV:getfd(Port, ClntSock),
%    io:format("shutdown~n", []),
%    ?DRV:shutdown(Port),
    {ok, Sock} = dbus_tcp_conn:connect(ClntSockFd, Options),
    {ok, Sock}.
