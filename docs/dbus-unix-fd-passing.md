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

## Suggested transport API

The transport should expose FDs explicitly:

```erlang
-type fd() :: non_neg_integer().

-spec send(connection(), iodata(), [fd()]) ->
    ok | {error, term()}.

-spec recv(connection(), timeout()) ->
    {ok, binary(), [fd()]} |
    {error, term()}.
```

It is preferable for `send` and `recv` to be transport behaviour callbacks because Unix and TCP no longer have identical I/O semantics.

```erlang
-callback send(connection(), iodata(), [fd()]) ->
    ok | {error, term()}.

-callback recv(connection(), timeout()) ->
    {ok, binary(), [fd()]} |
    {error, term()}.
```

The generic transport layer can then dispatch to the implementation:

```erlang
send({T, _} = Conn, Data, Fds) ->
    T:send(Conn, Data, Fds).

recv({T, _} = Conn, Timeout) ->
    T:recv(Conn, Timeout).
```

## TCP transport

TCP cannot carry D-Bus Unix FDs:

```erlang
send({dbus_transport_tcp, S}, Data, []) ->
    socket:send(S, Data);

send({dbus_transport_tcp, _}, _Data, [_ | _]) ->
    {error, unix_fd_not_supported}.
```

Its receive operation always returns an empty FD list.

Conceptually:

```text
dbus_transport_tcp
    socket:send / socket:recv
    FDs always []
```

## Unix transport

For ordinary data without FDs, `socket:send/2` remains sufficient. When descriptors are attached, use `socket:sendmsg/...` and a `rights` control message.

Conceptually:

```erlang
send({dbus_transport_unix, S}, Data, []) ->
    socket:send(S, Data);

send({dbus_transport_unix, S}, Data, Fds) ->
    Msg = #{
        iov => [iolist_to_binary(Data)],
        ctrl => [
            #{
                level => socket,
                type => rights,
                data => encode_fds(Fds)
            }
        ]
    },
    socket:sendmsg(S, Msg).
```

`rights` corresponds to the Unix `SCM_RIGHTS` ancillary message.

Conceptually:

```text
dbus_transport_unix
    socket:sendmsg / socket:recvmsg
    SCM_RIGHTS <-> [FD]
```

## Receiving FDs

Once FD passing can occur, the Unix transport should receive data with `socket:recvmsg/...` so that ancillary messages are not lost.

A received message has approximately this shape:

```erlang
#{
    iov := [...],
    ctrl := [
        #{
            level := socket,
            type := rights,
            data := FdData
        }
    ]
}
```

The transport converts the `rights` data into an Erlang list of descriptors:

```erlang
recv({dbus_transport_unix, S}, Timeout) ->
    case socket:recvmsg(S, 0, CtrlSize, [], Timeout) of
        {ok, #{iov := Iov, ctrl := Ctrl}} ->
            {
                ok,
                iolist_to_binary(Iov),
                decode_rights(Ctrl)
            };
        Error ->
            Error
    end.
```

Using `recvmsg` consistently for the Unix transport is preferable once ancillary data may occur. Unix sockets are byte streams: one `sendmsg` call must not be assumed to correspond to one `recvmsg` call or one complete D-Bus message. The connection/message layer must therefore accumulate both bytes and received descriptors while reconstructing D-Bus message boundaries.

## Encoding `SCM_RIGHTS`

OTP exposes `rights` ancillary data in its native representation rather than as a high-level `[Fd]` abstraction.

A small helper is therefore required:

```erlang
encode_fds(Fds) ->
    ...

decode_fds(Data) ->
    ...
```

The encoded values correspond to native C `int` file descriptors. This code should deliberately account for the platform's native representation rather than treating the control data as a portable network-format integer array.

This is Erlang code around the `socket` API; it does not require another socket implementation.

## Resource ownership

Received `SCM_RIGHTS` descriptors are new descriptors owned by the receiving process/application. They must have a clear lifecycle.

In particular:

- validate that the number of received descriptors matches the D-Bus `UNIX_FDS` header;
- validate every `h` index against the received FD array;
- avoid leaking descriptors when message parsing fails;
- define which layer becomes responsible for closing descriptors after successful delivery.

Descriptor ownership should be explicit in the higher-level D-Bus API.

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

The additional work belongs in `erlang-dbus` itself:

1. make the transport API FD-aware;
2. implement `sendmsg`/`recvmsg` in `dbus_transport_unix`;
3. encode/decode native `SCM_RIGHTS` descriptor data;
4. accumulate FDs correctly while framing the byte stream;
5. validate D-Bus `UNIX_FDS` and `h` values above the transport layer;
6. define descriptor ownership and cleanup.

No additional native transport library is required.
