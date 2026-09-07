A native erlang implementation of D-Bus
==============================================

D-Bus is now largely used in a lot of applications for
language-independant, object-oriented RPC system.

The erlang platform needs an erlang native implementation.

[![CI](https://github.com/jeanparpaillon/erlang-dbus/actions/workflows/ci.yml/badge.svg)](https://github.com/jeanparpaillon/erlang-dbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/jeanparpaillon/erlang-dbus/branch/next/graph/badge.svg?token=mLKxwRF4GV)](https://codecov.io/gh/jeanparpaillon/erlang-dbus)
[![Hex.pm](https://img.shields.io/hexpm/v/dbus.svg)](https://hex.pm/packages/dbus)

# Example

```erlang
Bus = system,  % or `session` or any valid D-Bus address string
dbus:start_link(Bus),
BusProxy = dbus:get_proxy(Bus),
dbus_bus:request_name(BusProxy, <<"com.example">>).
```

# Unix file descriptor passing

Supported on `unix:` transports, and only there: `tcp:` and `nonce-tcp:` sockets
cannot carry a descriptor at all, so `NEGOTIATE_UNIX_FD` is never sent over them
and such a connection authenticates with `agree_unix_fd = false`. Capability is
not agreement -- the peer still has to answer `AGREE_UNIX_FD`, and a message
carrying descriptors on a connection where it did not is
`{error, unix_fd_not_negotiated}` with nothing written.

A message carries its descriptors on `#dbus_message.fds`. The value of an `h`
(`unix_fd`) in a body is an **index into that list**, not a descriptor;
`dbus_message:fd/2` resolves one. The `UNIX_FDS` header field is synthesised
from `fds` while marshalling, so a caller sets `fds` and never the field. At
most `?MAX_UNIX_FDS` (16) descriptors travel with one message.

Received descriptors belong to the **owner process**: they are open in this OS
process, they count against `RLIMIT_NOFILE`, and the library will not close
them. Consuming one -- `socket:open/1`, a port program, a NIF -- or
`dbus_fd:close/1` is the owner's job, and `dbus_fd:dup/1` is how one outlives
the message it arrived with. The descriptors nobody takes delivery of are the
library's and are closed with a logged warning: a message that fails to parse,
descriptors arriving when `AGREE_UNIX_FD` was never exchanged, and the queue
left over when a connection dies.

Descriptors **sent** stay the sender's. `sendmsg(2)` gives the peer a copy of
the open file description, not the number, so the descriptors in a message
passed to `dbus_connection:send/2` are still open here when it returns.

# Documentation

* [API documentation](doc/README.md)
* [D-Bus Specifications](https://dbus.freedesktop.org/doc/dbus-specification.html)
