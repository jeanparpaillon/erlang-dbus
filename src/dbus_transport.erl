-module(dbus_transport).
-moduledoc """
Abstract transports.

Glue module to the `m:dbus_transport_tcp` and `m:dbus_transport_unix` transport
modules.

Messages implemented by transport modules:

- `{received, Conn, Data}`
- `{closed, Conn}`

While unix socket and TCP transports are the commonly used transports, the
[D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.html#transports)
describes additional transports.
""".

%% api
-export([send/2, set_raw/2, stop/1, close/1, support_unix_fd/1]).

-doc "Close the transport.".
-spec close(pid()) -> ok.
close(Conn) ->
    gen_server:cast(Conn, close).


-doc "Send data to a transport.".
-spec send(pid(), iodata()) -> ok.
send(Conn, Data) -> 
    gen_server:cast(Conn, {send, Data}).


-doc "Set transport in raw mode (used after authentication is done).".
-spec set_raw(pid(), boolean()) -> ok.
set_raw(Conn, Raw) ->
    gen_server:call(Conn, {set_raw, Raw}).


-doc "Stop transport.".
-spec stop(pid()) -> ok.
stop(Conn) ->
    gen_server:cast(Conn, stop).


-doc "Check if this transport support UNIX FD passing.".
-spec support_unix_fd(pid()) -> boolean().
support_unix_fd(Conn) ->
    try gen_server:call(Conn, support_unix_fd) of
        true -> true;
        false -> false
    catch
        _:_ ->
            false
    end.
