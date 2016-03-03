%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free>
%% @doc Abstract transports
%%
%% Glue module to dbus_transprot_tcp and dbus_transport_unix transport modules
%%
%% Messages implemented by transport modules
%% * `{received, Conn, Data}'
%% * `{closed, Conn}'
%%
%% While unix socket and TCP transports are the commonly used transports,
%% <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#transports" >D-Bus specification</a>
%% describes additional transports. 
%%
%% @todo Implements remaining transports: launchd, systemd, nonce-secured TCP, unixexec, kernel
%% @end

-module(dbus_transport).

%% api
-export([send/2, set_raw/2, stop/1, close/1, support_unix_fd/1]).

%% @doc Close the transport
%% @end
-spec close(pid()) -> ok.
close(Conn) ->
    gen_server:cast(Conn, close).


%% @doc Send data to a transport
%% @end
-spec send(pid(), binary()) -> ok.
send(Conn, Data) -> 
    gen_server:cast(Conn, {send, Data}).


%% @doc Set transport in raw mode (used after authentication is done)
%% @end
-spec set_raw(pid(), boolean()) -> ok.
set_raw(Conn, Raw) ->
    gen_server:call(Conn, {set_raw, Raw}).


%% @doc Stop transport
%% @end
-spec stop(pid()) -> ok.
stop(Conn) ->
    gen_server:cast(Conn, stop).


%% @doc Check if this transport support UNIX FD passing
%% 
%% @end
-spec support_unix_fd(pid()) -> boolean().
support_unix_fd(Conn) ->
    try gen_server:call(Conn, support_unix_fd) of
        true -> true;
        false -> false
    catch
        _:_ ->
            false
    end.
