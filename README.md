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
* [Manual](https://github.com/jeanparpaillon/erlang-dbus/wiki)
* [D-Bus Specifications](https://dbus.freedesktop.org/doc/dbus-specification.html)

## Current status

The status: 
* Consuming D-Bus services: ok
* Providing D-Bus services: ok
* Tests for both are working!
* Connect through TCP and UNIX socket: ok
* Unix file descriptor passing, over UNIX sockets: ok

# Building

`rebar3 compile` builds one NIF, `c_src/dbus_fd.c` into `priv/dbus_fd.so`, so a C
compiler and `make` are needed here and in anything that depends on this
application. It is `close(2)`/`dup(2)` for the file descriptors a message carries
over a UNIX socket -- see `m:dbus_fd` for why OTP cannot close those on its own.

# Issue format

`submit_issue.sh` parses these files, so the header is structural, not decorative.
It takes the **title from the first line** (stripping `# `), reads the four `**Key:**`
lines, and uses **everything from the first `##` heading onward** as the issue body.

```markdown
# Fix parsing env var

**Workstream:** 2.3 - Fixes
**Context:** [arch.md](doc/connections.md) 
**Requires:** [1.2]
**State:** draft

## What

...

## Checklist

- [ ] ...

## Acceptance

- [ ] ...
```

Rules the script enforces, or that follow from how it works:

- **Filename is the issue number with dots as underscores** — `3_1.md` is issue 3.1.
- **The number must be in the title.** `Requires:` refs are resolved with
  `gh issue list --search "<ref> in:title"` to build `--blocked-by`, so a title without
  its number cannot be depended on.
- **`Requires:` refs are `[N.N]` in square brackets**, comma-separated or in prose;
  the extractor only sees the bracketed forms. Use `none — <why>` when there are no
  dependencies.
- **Submit in dependency order.** Refs resolve against issues that already exist on
  GitHub; an issue submitted before its dependency gets no `--blocked-by` link and the
  script says nothing about it.
- **`Workstream:` and `Context:` are re-emitted as `##` headings** at the top of the
  body. Do not repeat them in the prose.
- **`State:`** — `draft` until the issue is submitted.
- Requires `gh` and `gum`.
