%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free>
%% @doc transport gen_server
%%
%% Glue module to dbus_transprot_tcp and dbus_transport_unix transport modules
%%
%% Messages implemented by transport modules
%%
%% {received, Conn, Data}
%% {closed, Conn}

-module(dbus_transport).

%% api
-export([send/2, set_raw/2, stop/1, close/1, support_unix_fd/1]).

close(Conn) ->
    gen_server:cast(Conn, close).

send(Conn, Data) -> 
    gen_server:cast(Conn, {send, Data}).

set_raw(Conn, Raw) ->
    gen_server:call(Conn, {set_raw, Raw}).

stop(Conn) ->
    gen_server:cast(Conn, stop).

support_unix_fd(Conn) ->
    try gen_server:call(Conn, support_unix_fd) of
        true -> true;
        false -> false
    catch
        _:_ ->
            false
    end.
