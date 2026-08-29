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

- `dbus_auth` - behaviour
- `dbus_auth_anonymous` - ANONYMOUS
- `dbus_auth_cookie_sha1` - DBUS_COOKIE_SHA1
- `dbus_auth_external` - EXTERNAL

## Transport

- `dbus_transport` - behaviour
- `dbus_transport_unix` - UNIX domain sockets (path or abstract)
- `dbus_transport_tcp` - TCP sockets

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

```erlang
-module(dbus_transport).

-export_type([
    address/0,
    option/0,
    connection/0
]).

-type address() :: map().
-type option() :: term().
-type connection() :: term().

-callback connect(Address :: address(), Options :: [option()]) ->
    {ok, Connection :: connection()} |
    {error, Reason :: term()}.

-callback send(Connection :: connection(), Data :: iodata()) ->
    ok |
    {error, Reason :: term()}.

-callback recv(Connection :: connection(), Length :: non_neg_integer(),
               Timeout :: timeout()) ->
    {ok, Data :: binary()} |
    {error, closed | timeout | term()}.

-callback close(Connection :: connection()) ->
    ok.
```

# Auth

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