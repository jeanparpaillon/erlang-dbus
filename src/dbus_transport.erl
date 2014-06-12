%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc transport gen_server
%%
%% Glue module to dbus_tcp_conn and dbus_unix_conn transport modules
%%
%% Messages implemented by transport modules
%%
%% {received, Conn, Data}
%% {closed, Conn}

-module(dbus_transport).

%% api
-export([send/2, change_owner/3, setopts/2, stop/1, close/1]).

close(Conn) ->
    gen_server:cast(Conn, close).

send(Conn, Data) -> 
    gen_server:cast(Conn, {send, Data}).

change_owner(Conn, OldPid, NewPid) when is_pid(NewPid) ->   
    gen_server:call(Conn, {change_owner, OldPid, NewPid}).

setopts(Conn, Options) ->
    gen_server:call(Conn, {setopts, Options}).

stop(Conn) ->
    gen_server:cast(Conn, stop).
