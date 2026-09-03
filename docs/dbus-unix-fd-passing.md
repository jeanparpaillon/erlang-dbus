# D-Bus Unix FD Passing in Erlang

## Summary

Erlang's `socket` module is sufficient to implement D-Bus Unix file-descriptor passing at the OS/socket layer. No additional Unix-socket library, NIF, or port driver is required on current OTP for the actual `SCM_RIGHTS` transfer.

However, a transport implementation that only uses `socket:send/2` and `socket:recv/...` is not sufficient. Unix FD passing requires ancillary/control messages, exposed by Erlang through `socket:sendmsg/...` and `socket:recvmsg/...`.

The transport API therefore needs to carry file descriptors alongside the byte stream.

## D-Bus semantics

D-Bus file descriptors are transferred **out of band** over a Unix domain socket using `SCM_RIGHTS`.

At the D-Bus message level:

- the `UNIX_FDS` header field contains the number of attached file descriptors;
- a D-Bus value of type `h` is an index into the attached FD array;
- the actual OS descriptors are transferred as ancillary data with the message bytes;
- Unix FD passing must first have been enabled through D-Bus authentication negotiation (`NEGOTIATE_UNIX_FD` / `AGREE_UNIX_FD`).

Transport capability and D-Bus negotiation are separate concerns:

- `dbus_transport_unix` provides the physical ability to transfer descriptors;
- the authentication/session layer negotiates whether that ability may be used;
- message handling validates `UNIX_FDS` and resolves `h` indices.

## Transport API

The transport carries descriptors alongside the bytes:

```erlang
-type fd() :: non_neg_integer().

-spec send(connection(), iodata(), [fd()]) ->
    ok | {error, term()}.

-spec recv(connection(), timeout()) ->
    {ok, Data :: binary(), [fd()]}
    | {error, term()}.
```

`send/2` stays, as `send/3` with no descriptors: the NUL byte that opens a
connection and the whole SASL conversation have none to pass, and there is no
reason to make every caller write `[]`.

### Not transport callbacks

An earlier draft of this document made `send` and `recv` callbacks of the
`m:dbus_transport` behaviour, on the grounds that "Unix and TCP no longer have
identical I/O semantics". That is the wrong cut, and this document now says so:

- The three transport modules do not own their I/O. Since the transport was
  simplified they own `connect/1` and nothing else; `send`, `recv` and `close`
  live once in `m:dbus_transport` and run against a `t:socket:socket/0`, which
  is what all three return. Callbacks would put two functions back into each
  module -- and `nonce-tcp`, which is `tcp` plus one write, would delegate both
  to `m:dbus_transport_tcp` the way it once delegated `support_unix_fd/1`.
- The distinction is not `unix` versus `tcp`, it is fd-capable versus not, and
  that is already a boolean on the connection: `#transport.support_unix_fd`,
  filled at `connect/1` from the module's `support_unix_fd/0`. Dispatch on it.

So `m:dbus_transport` grows two clauses rather than the behaviour growing two
callbacks:

```erlang
send(#transport{sock = S, support_unix_fd = false}, Data, []) ->
    socket:send(S, Data);
send(#transport{support_unix_fd = false}, _Data, [_ | _]) ->
    {error, unix_fd_not_supported};
send(#transport{sock = S}, Data, Fds) ->
    sendmsg(S, Data, Fds).
```

`recv/2` reads with `socket:recv/4` on a transport that cannot carry
descriptors and with `socket:recvmsg/5` on one that can, and returns `[]` for
the descriptors in the first case.

### Capability is not agreement

`support_unix_fd` says the transport *can* carry descriptors; `AGREE_UNIX_FD`
says the peer has agreed that it *may*. They are different facts and they live
in different places: the capability on `#transport{}`, where authentication
reads it to decide whether to send `NEGOTIATE_UNIX_FD` at all, and the
agreement in `#state.agree_unix_fd` in `m:dbus_connection`, which is also the
layer that writes and reads the `UNIX_FDS` header field.

The transport is therefore not told the negotiation result. Refusing to send
descriptors that were never negotiated, and refusing a message that arrives
carrying descriptors when they were not, are both `m:dbus_connection`'s to
make. This is the same boundary as the rule at the end of this document:
the transport moves bytes and descriptors, it does not interpret them.

## TCP transport

TCP cannot carry D-Bus Unix FDs. `support_unix_fd/0` is `false` for both
`m:dbus_transport_tcp` and `m:dbus_transport_nonce_tcp`, so the clauses above
send with `socket:send/2`, refuse a non-empty descriptor list outright, and
receive with `socket:recv/4` -- returning `[]` descriptors, always.

Conceptually:

```text
dbus_transport_tcp
    socket:send / socket:recv
    FDs always []
```

## Unix transport

`m:dbus_transport_unix` is the one module whose `support_unix_fd/0` is `true`,
which is what selects the `sendmsg`/`recvmsg` path in `m:dbus_transport`. A
`rights` control message is the Unix `SCM_RIGHTS` ancillary message:

```erlang
sendmsg(S, <<First:1/binary, Rest/binary>>, Fds) ->
    Msg = #{
        iov => [First],
        ctrl => [
            #{
                level => socket,
                type => rights,
                data => encode_fds(Fds)
            }
        ]
    },
    case socket:sendmsg(S, Msg, [], infinity) of
        ok -> socket:send(S, Rest);
        {error, _} = E -> E
    end.
```

**The descriptors go with one byte, and the payload follows as plain bytes.**
An earlier draft of this document sent the whole payload in the `iov` and
resent whatever `sendmsg` left over. That is not enough, and the reason is
worth recording, because the naive version passes every small test:

- `socket:sendmsg/4` **loops by itself** under an `infinity` timeout. The
  `{ok, RestData}` the OTP documentation describes is what a caller sees with
  a deadline; with `infinity` the partial write is finished inside `socket`,
  through a continuation.
- **That continuation carries the same control message.** The `Cont`
  documentation says only `iov` is taken from the map, but the `SCM_RIGHTS`
  attached to the first attempt is sent again with each further one. A 4 MiB
  payload sent in a single `send/3` arrives with *twenty* copies of every
  descriptor -- one per segment -- which the test in `m:dbus_transport`
  (`partial_write_test_`) pins.

One byte is written whole or not at all, so there is no remainder for a
continuation to duplicate, and the rest of the payload is an ordinary
`socket:send/2` that loops safely because it carries no ancillary data.

Two more consequences:

- **The `iov` must not be empty.** `SCM_RIGHTS` with no payload byte is not
  delivered on Linux. Every D-Bus message is at least sixteen bytes, so there
  is always a byte to carry the descriptors -- but it is why they cannot be
  flushed on their own, and why `send/3` with an empty payload and a non-empty
  descriptor list is an error.
- **The first byte, not the last.** The kernel delivers ancillary data no
  later than the end of the segment it came with, so attaching it to the front
  keeps the guarantee the receiving side already relies on: descriptors arrive
  at or before the message that declares them, never after. What it does cost
  is a read boundary -- the first `recv/2` after a descriptor returns that one
  byte and the descriptors -- which is nothing new, since a transport read was
  never a message.

Conceptually:

```text
dbus_transport_unix
    socket:sendmsg / socket:recvmsg
    SCM_RIGHTS <-> [FD]
```

## Receiving FDs

Once descriptors may arrive, the fd-capable path receives with
`socket:recvmsg/5` so that ancillary messages are not dropped:

```erlang
recv(#transport{sock = S, support_unix_fd = true}, Timeout) ->
    case socket:recvmsg(S, 0, ?CTRL_SIZE, [], Timeout) of
        {ok, #{iov := Iov, ctrl := Ctrl, flags := Flags}} ->
            ...
        {error, _} = E ->
            E
    end.
```

Three details that a `socket:recv/4` caller never had to think about:

- **The control buffer is sized by the caller, and overflowing it loses
  descriptors.** A too-small buffer makes the kernel truncate the ancillary
  data and close the descriptors it dropped; the only trace is `ctrunc` in the
  returned `flags`. Size the buffer for the per-message limit -- `libdbus`
  allows 16 descriptors per message and `dbus-daemon` exposes the number as
  `max_message_unix_fds` (`/usr/share/dbus-1/system.conf` shows the stock
  value commented out at 16) -- and treat `ctrunc` as a protocol error rather
  than a short read, because the descriptors are already gone.
- **`ctrl` is a list.** One `recvmsg` may return several `rights` messages;
  their descriptors concatenate, in order.
- **Boundaries still are not message boundaries.** A Unix socket is a byte
  stream: one `sendmsg` is not one `recvmsg` and neither is one D-Bus message.
  What is true, and is what makes a queue sufficient, is that the kernel
  delivers ancillary data no later than the last byte of the segment it was
  attached to -- so descriptors arrive at or before the end of the message
  that declares them, never after, and never reordered against it. The
  connection layer accumulates bytes and descriptors as two queues and takes
  `UNIX_FDS` of them off the front as each message is framed.

## Encoding `SCM_RIGHTS`

OTP does not decode `rights` into a list of descriptors. `t:socket:cmsg_recv/0`
types the field as `data := binary()`, with no `value` alongside it -- unlike
`timestamp` or `ip_tos`, which OTP does decode. On the send side
`t:socket:cmsg_send/0` takes a `native_value()`, i.e. a binary here.

So the payload is an array of native C `int`s, and the codec is two lines:

```erlang
encode_fds(Fds) ->
    << <<Fd:32/native-signed>> || Fd <- Fds >>.

decode_fds(Bin) when byte_size(Bin) rem 4 =:= 0 ->
    [Fd || <<Fd:32/native-signed>> <= Bin].
```

Native byte order, native width, signed: this is memory the kernel wrote, not
a network-format integer array, and `-1` is a value `int` can hold. A payload
whose size is not a multiple of four is malformed and must not be parsed into
descriptors that were never sent.

## Resource ownership

Received `SCM_RIGHTS` descriptors are new descriptors in the emulator's file
table. They count against `RLIMIT_NOFILE` and nothing reclaims them.

**OTP has no `close(2)`.** `socket:open/2` adopts an existing descriptor and
`socket:close/1` then closes it, but only for a *socket* -- and the descriptors
D-Bus actually carries are usually not sockets: portal-style APIs hand over
files, pipes and `memfd`s. There is no `file` or `prim_file` equivalent. That is
why `m:dbus_fd` exists: a NIF over `close(2)` and `dup(2)`, so the library owns
the one primitive Erlang is missing rather than asking the application to supply
it. An earlier draft of this section proposed a configurable closer in the
application environment; a NIF that is always there is better, because the
discard paths below are exactly the ones nobody configures for.

That gives a contract with two halves:

- **The owner owns what it receives.** `m:dbus_connection` puts the descriptors
  in `#dbus_message.fds` and hands them to the owner process with the message.
  From that point they are the application's, to consume with whatever can
  consume a descriptor -- `socket:open/2`, a port program, a NIF -- or to
  `dbus_fd:close/1`. `dbus_fd:dup/1` is how one outlives the message it came
  with.
- **The paths where nobody takes delivery are the library's**: a message that
  fails to parse, descriptors that arrive when `AGREE_UNIX_FD` was never
  exchanged, descriptors still queued when the connection dies.
  `m:dbus_connection` closes those with `dbus_fd:close/1` and logs the count and
  the reason at warning -- a discard nobody can see is indistinguishable from a
  leak.
- **Sent descriptors stay the sender's.** `sendmsg(2)` gives the peer a copy of
  the open file description, not the number, so `dbus_connection:send/2` leaves
  the caller's descriptors open.
- **Validate the count before allocating anything to it**: `UNIX_FDS` above the
  per-message limit is refused as a protocol error, not honoured.

Index validation is a separate matter and belongs with the value, not the
transfer: an `h` in a body is an index, and it is checked against the message's
descriptor array when it is resolved.

## Recommended layer boundary

```text
D-Bus message
    UNIX_FDS header
    h values (indices)
           |
           v
connection / message layer
    message framing
    validates FD count and indices
           |
           v
dbus_transport
    bytes + [FD]
           |
       +---+---+
       |       |
       v       v
     Unix     TCP
   SCM_RIGHTS  no FDs
```

The key design rule is:

> `dbus_transport` transports bytes and descriptors; it does not interpret D-Bus `h` values or the `UNIX_FDS` header.

## Conclusion

The Erlang `socket` module provides everything required for the actual Unix FD transfer:

- Unix domain sockets;
- `sendmsg`;
- `recvmsg`;
- ancillary/control messages;
- `SCM_RIGHTS` through control messages of type `rights`.

No additional native transport library is required. What this document once
listed as work to be done in `erlang-dbus` itself is done, and the sections
above now describe the code rather than a plan:

1. `m:dbus_marshaller` knows the `h` type code and synthesises the `UNIX_FDS`
   header field from `#dbus_message.fds`;
2. `m:dbus_transport`'s `send/3` and `recv/2` are fd-aware, dispatching on
   `#transport.support_unix_fd` rather than on a new behaviour callback;
3. native `SCM_RIGHTS` descriptor data is encoded and decoded there;
4. `m:dbus_connection` accumulates descriptors as a queue while framing the
   byte stream, and enforces `AGREE_UNIX_FD` both ways;
5. `UNIX_FDS` is validated against the per-message limit, and `h` indices are
   validated by `dbus_message:fd/2` when they are resolved;
6. received descriptors are handed to the owner, and the paths where nobody
   takes delivery close them through the `m:dbus_fd` NIF -- OTP cannot close a
   non-socket descriptor.

`dbus_transport_unix:support_unix_fd/0` therefore returns `true`, which is what
makes `m:dbus_auth_client_mech` send `NEGOTIATE_UNIX_FD`. The end-to-end
evidence is `test/dbus_unix_fd_SUITE.erl`: a descriptor sent through a real
`dbus-daemon` and echoed back names the same open file, and the same suite over
`tcp:` shows a connection that authenticates normally with
`agree_unix_fd = false`.
