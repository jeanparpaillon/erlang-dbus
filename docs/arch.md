erlang-dbus arch
================

# Modules

## Message Protocol, parsers

- `dbus_address` - D-Bus addresses parser
- `dbus_marshaller` - D-Bus wire (de)serialization
- `dbus_hex` - hex encoding/decoding
    Really needed ?
- `dbus_introspect` - D-Bus XML introspection module
- `dbus_message` - High level API for building messages : calls, signals,
  errors, etc

## Authentication layer / Session

- `dbus_auth` - the auth conversation, and the only place that frames lines
- `dbus_auth_anonymous` - ANONYMOUS
- `dbus_auth_cookie_sha1` - DBUS_COOKIE_SHA1
- `dbus_auth_external` - EXTERNAL

## Transport

Client side only: this library dials a bus, it does not implement one. There is
no `listen/1`, no `accept/2` and no `systemd:` scheme.

- `dbus_transport` - behaviour, plus `resolve/1` from scheme to module
- `dbus_transport_socket` - the core, and the only one: stream over `m:socket`,
  raw bytes, `SCM_RIGHTS` fd passing, close

Scheme adapters, which only have to produce a connected core:

- `dbus_transport_unix` - `unix:`, path or abstract
- `dbus_transport_tcp` - `tcp:`
- `dbus_transport_nonce_tcp` - `nonce-tcp:`, `tcp:` plus the nonce write

Address resolvers, which yield another address rather than a connection:

- `dbus_address_launchd` - `launchd:`
- `dbus_address_autolaunch` - `autolaunch:`

See [Transport](#transport) for what belongs in which layer.

## Connection

- `dbus_connection` - 

## Bus layer

- `dbus_bus_connection` - Connection to a D-Bus bus
- `dbus_bus_registry` - ?
- `dbus_bus` - broken ?

## Object / RPC

- `dbus_names` - well known binaries to atom
- `dbus_sup` - Top level supervisor (should be started only with service, or proxy)
- `gen_dbus` - D-Bus object behaviour (?)

### Application

#### Client

- `dbus_proxy` - D-Bus object proxy
- `dbus_properties_proxy` - 'org.freedesktop.DBus.Properties' proxy

#### Service

- `dbus_service` - 
- `dbus_remote_service` -
- `dbus_service_reg` - 

# Transport

## Client only

Three schemes, `unix:`, `tcp:` and `nonce-tcp:`, and every transport here
connects; none of them listens. That is a scope decision, not an oversight:
the schemes exist so this library can reach a bus, and the
server side of the spec — creating the socket, publishing the address, running
authentication as the verifier — is a different program. It removes `listen/1`
and `accept/2` from the behaviour, removes the `systemd:` scheme entirely
(`sd_listen_fds` hands over listeners, which is the one thing a client has no
use for), and turns the listen-only `unix:` address forms into an error rather
than a second code path. See [Listen-only address forms](#listen-only-address-forms).

## One core, thin adapters

The schemes a D-Bus address can carry are not all the same kind of thing, and
flattening them into one module per scheme duplicates the parts that are
genuinely shared.

**The stream core** owns a byte stream and implements everything but
`connect/1`: `send`, `recv`, `close`, `support_unix_fd`, `disable_unix_fd`. It
delivers whatever bytes arrived and never interprets them — see
[Framing is not the transport's job](#framing-is-not-the-transports-job).

**Scheme adapters** turn a `t:dbus_address/0` into a connected core. This is
where `unix:` and `tcp:` actually differ, and they differ *only* in
`connect/1` — in practice, only in which `t:socket:sockaddr/0` they build.

**Address resolvers** are not transports. `launchd:` and `autolaunch:` name a
procedure for *finding* an address — asking launchd for a socket path, reading
an X11 property or running `dbus-launch` — and what comes back is an address
that some adapter then connects normally, a `unix:` one in every case we
support. Modelling them as transports would force a `connect/1` that returns
another module's connection.

## Everything is a socket

There is one core because, with `unixexec:` dropped, every remaining scheme is
an ordinary stream socket. `unix:` is `{local, Path}` in domain `local` and
`tcp:` is an `inet`/`inet6` address. Nothing else about them differs: same
`socket:send/2`, same `socket:recv/3`, same close. A
scheme added later stays above the same core as long as it is a socket —
`nonce-tcp:` is exactly that, `tcp:` plus a 16-byte write inside `connect/1`,
and its adapter reuses the TCP one's address rules rather than restating
them.

`unixexec:` was the one scheme that could not be a socket — it spawns a program
and speaks to its stdin/stdout, which under BEAM is an `open_port/2` with a
completely different receive shape (messages, not a recv call), no
`socket:sockname/1`, and no way to pass a file descriptor. Supporting it meant
a second core reimplementing the whole behaviour over a port, with
`support_unix_fd/1` wired to `false` and `recv/2` rebuilt out of port messages.
It buys a scheme that no bus on this platform
publishes: it exists for `dbus-daemon --nofork` over ssh and for test
harnesses. Dropping the scheme drops the core with it.

So the layering above is two layers rather than three, and an adapter is now
close to a single function from address to sockaddr.

## Scheme mapping

| Scheme | Module | Layer | `connect/1` is |
|---|---|---|---|
| `unix:` | `dbus_transport_unix` | adapter | `path`, or `abstract` as `<<0, Name/binary>>`, to the socket core with fd passing enabled |
| `tcp:` | `dbus_transport_tcp` | adapter | `host`/`port`/`family` to the socket core, no fd passing |
| `nonce-tcp:` | `dbus_transport_nonce_tcp` | adapter | the `tcp:` connect, then the 16 bytes of `noncefile` before anything else |
| `launchd:` | `dbus_address_launchd` | resolver | — |
| `autolaunch:` | `dbus_address_autolaunch` | resolver | — |

Anything else stays `{error, undefined}` — `unixexec:` and `systemd:`
included, so an address carrying one fails resolution rather than failing late
inside a `connect/1` that cannot work. `guid=` is
scheme-independent: it is the connection's business, checked against the
`OK <guid>` the server sends at the end of authentication, and no transport
should look at it.

## Framing is not the transport's job

The transport carries raw bytes for its whole life. There is no `set_mode/2`,
no `raw | line` state, and no accumulator in `dbus_transport_socket`.

D-Bus does use two framings on one connection — CRLF-terminated ASCII lines
until `BEGIN`, binary messages after it — but the switch is not a property of
the byte stream, it is a step in the auth conversation, and only `dbus_auth`
knows when it happens. Putting a mode flag in the transport publishes that
knowledge to a layer that cannot act on it: every adapter and the core would
carry a state variable whose only writer is the auth module, and `recv/2`
would have two return shapes for callers to handle. Keeping it in `dbus_auth`
means the transport has one behaviour to test, and the line grammar has one
implementation regardless of which transport is under it.

So `dbus_auth` buffers: it calls `recv/2`, splits on `\r\n`, and keeps the tail
for the next line. **The tail is the part that must not be dropped.** A server
is free to put the `OK <guid>\r\n` and the first bytes of its reply to
`Hello` in one segment, so at the end of authentication `dbus_auth` can be
holding message bytes. It hands that remainder back to `dbus_connection` along
with the negotiated guid and the `NEGOTIATE_UNIX_FD` result, and the connection
prepends it to its own message buffer. A transport-level mode switch has the
same problem and no good place to put the answer, which is a second reason the
flag does not belong there.

## Why `m:socket` and not `m:gen_tcp`

`socket:sendmsg/2` with an `SCM_RIGHTS` control message is the only way to
implement `support_unix_fd/1` as anything but `false`, and `UNIX_FD` is a type
the bus really does hand us. `m:gen_tcp` cannot do it at any price, so it
cannot serve as the core even for `tcp:`, where it would otherwise be
sufficient — and having one core is the point of the previous section.
`socket:open/2` over an existing descriptor also makes an inherited socket
adoptable, should the client-only scope ever be revisited.

## Behaviour

```erlang
-module(dbus_transport).

-export_type([connection/0]).

-type connection() :: term().

-callback connect(Address :: dbus_address()) ->
    {ok, Connection :: connection()}
    | {error, Reason :: term()}.

-callback send(Connection :: connection(), Data :: iodata()) ->
    ok
    | {error, Reason :: term()}.

-callback recv(Connection :: connection(), Timeout :: timeout()) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.

-callback close(Connection :: connection()) ->
    ok
    | {error, Reason :: term()}.

-callback support_unix_fd(Connection :: connection()) ->
    boolean().

-callback disable_unix_fd(Connection :: connection()) -> ok.
```

and a second, smaller behaviour for the resolvers:

```erlang
-module(dbus_address_resolver).

-callback resolve(Address :: dbus_address()) ->
    {ok, [dbus_address()]} | {error, Reason :: term()}.
```

## Resolution

`dbus_transport:resolve/1` gains a third answer, so that the caller can tell a
transport from a redirection:

```erlang
-spec resolve(dbus_address() | module()) ->
    {ok, module()} | {redirect, module()} | {error, undefined}.
resolve(Mod) when is_atom(Mod) ->
    {ok, Mod};
resolve(#dbus_address{} = Address) ->
    case dbus_address:scheme(Address) of
        <<"unix">> -> {ok, dbus_transport_unix};
        <<"tcp">> -> {ok, dbus_transport_tcp};
        <<"nonce-tcp">> -> {ok, dbus_transport_nonce_tcp};
        <<"launchd">> -> {redirect, dbus_address_launchd};
        <<"autolaunch">> -> {redirect, dbus_address_autolaunch};
        _ -> {error, undefined}
    end.
```

`DBUS_SESSION_BUS_ADDRESS` holds alternatives to be tried in order, and a
resolver expands one address into more of them, so `dbus_connection` has to
loop over a list of candidates either way. Given that loop, `redirect` costs
nothing beyond re-entering it, and an `{error, undefined}` on an unsupported
scheme is just the next candidate. That is also what a resolver returning
something we do not implement comes to: `autolaunch:` reads
`_DBUS_SESSION_BUS_ADDRESS` off the X11 root window and is free to yield a
scheme of its own choosing; whatever comes back is re-entered as a candidate,
connected by its adapter if there is one and skipped like any other
unsupported one if there is not, rather than being a special case for the
resolver to handle.

## Listen-only address forms

`unix:dir=`, `unix:tmpdir=` and `unix:runtime=yes` tell a *server* to create a
socket and publish the resulting `path` or `abstract` address. They parse fine
and are useless to `connect/1`, which should reject them as such rather than
falling through to a missing-path error.

# Auth

`dbus_auth` drives the conversation *and* owns its framing: the transport hands
it raw bytes, it splits them on CRLF, and it keeps the remainder past the final
`OK <guid>` for `dbus_connection`. See
[Framing is not the transport's job](#framing-is-not-the-transports-job).

## Client auth

Spec:

```
MECH(CHALL) ->
    CONTINUE(RESP)
    OK(RESP)
    ERROR
```

Behaviour:

```erlang
-module(dbus_auth_client_mechanism).

-callback name() -> binary().

-callback init(map()) ->
    {ok, state()} | {error, term()}.

-callback initial_response(state()) ->
    {continue, binary(), state()}
    | {ok, binary(), state()}
    | {none, state()}
    | {error, term()}.

-callback challenge(binary(), state()) ->
    {continue, binary(), state()}
    | {ok, binary(), state()}
    | {error, term()}.
```

## Server auth behaviour

Spec:

```
MECH(RESP) ->
    CONTINUE(CHALL)
    OK
    REJECTED
```

Behaviour:

```erlang
-module(dbus_auth_server_mechanism).

-callback name() -> binary().

-callback init(map()) ->
    {ok, state()} | {error, term()}.

-callback response(binary(), state()) ->
    {continue, binary(), state()}
    | {ok, state()}
    | {rejected, term()}.
```