# D-Bus Session Bus Address Parsing

## Overview

`DBUS_SESSION_BUS_ADDRESS` contains one or more **D-Bus server addresses** describing how a client can connect to the session bus.

A D-Bus address is not a URI. Its general form is:

```text
address-list := address (";" address)*
address      := transport ":" [key-value-list]
key-value-list := key "=" escaped-value ("," key "=" escaped-value)*
```

Example:

```text
unix:path=/run/user/1000/bus
```

Multiple addresses can be provided:

```text
unix:path=/tmp/dbus;tcp:host=localhost,port=12345
```

The addresses are alternatives. A client should try them in order until a connection succeeds. 

## Address Structure

An address consists of:

```text
<transport>:<key>=<value>,<key>=<value>,...
```

For example:

```text
tcp:host=127.0.0.1,port=12345,family=ipv4
```

The structural delimiters are:

| Character | Meaning                                  |
| --------- | ---------------------------------------- |
| `;`       | separates alternative addresses          |
| `:`       | separates transport name from parameters |
| `,`       | separates parameters                     |
| `=`       | separates parameter name from value      |
| `%`       | introduces an escaped byte               |

The parser should first parse this generic structure and only afterwards interpret parameters according to the transport.

---

## Supported Transports

### `unix:`

Unix domain socket transport. This is the most common transport for a session bus on Linux and other Unix-like systems.

Connectable forms include:

```text
unix:path=/run/user/1000/bus
```

and, on platforms supporting abstract Unix sockets:

```text
unix:abstract=/tmp/dbus-XYZ
```

Relevant parameters include:

```text
path
abstract
guid
```

The specification also defines Unix addresses intended for **server/listen configuration**:

```text
unix:dir=/some/directory
unix:tmpdir=/tmp
unix:runtime=yes
```

These instruct a server to create a socket and subsequently produce a connectable `path` or `abstract` address.

For `DBUS_SESSION_BUS_ADDRESS`, a client parser can parse these forms generically but should distinguish between syntactically valid addresses and addresses that can actually be used to establish a client connection.

---

### `tcp:`

TCP/IP transport.

Example:

```text
tcp:host=127.0.0.1,port=12345
```

Possible parameters include:

```text
host
port
family
guid
```

For example:

```text
tcp:host=localhost,port=12345,family=ipv4
```

`family` can constrain the address family, typically IPv4 or IPv6.

A client connection requires a usable host and non-zero port.

---

### `nonce-tcp:`

TCP transport with an additional nonce-based authentication step.

Example:

```text
nonce-tcp:host=localhost,port=12345,noncefile=/tmp/dbus-nonce
```

Relevant parameters include:

```text
host
port
family
noncefile
guid
```

Before normal D-Bus authentication, the client reads the nonce file and sends its contents to the server.

---

### `launchd:`

Transport used with macOS `launchd`.

Example:

```text
launchd:env=DBUS_LAUNCHD_SESSION_BUS_SOCKET
```

Relevant parameters include:

```text
env
guid
```

The environment variable identifies the socket managed by `launchd`.

---

### `unixexec:`

Starts another process and communicates with it using Unix file descriptors.

Example:

```text
unixexec:path=/usr/bin/example
```

Arguments can be represented using numbered `argv` parameters:

```text
unixexec:path=/usr/bin/example,argv1=foo,argv2=bar
```

Relevant parameters include:

```text
path
argv0
argv1
argv2
...
guid
```

---

### `autolaunch:`

A meta-transport allowing the D-Bus implementation to locate or launch a session bus automatically.

It is particularly relevant on Windows.

Example:

```text
autolaunch:
```

or with a scope:

```text
autolaunch:scope=...
```

Relevant parameters include:

```text
scope
guid
```

---

### `systemd:`

The specification defines a systemd socket-activation transport.

Conceptually:

```text
systemd:
```

This is a **listen-only transport**, not a normal client connection address.

A generic parser can recognize it, while the client connection layer should reject it as unsupported for connecting to a session bus.

---

## Address Escaping

D-Bus uses percent escaping for address parameter values.

Characters in the following set can occur literally:

```text
[-0-9A-Za-z_/.*]
```

Other bytes are represented as:

```text
%HH
```

where `HH` consists of two hexadecimal digits.

For example, a space becomes:

```text
%20
```

so:

```text
/tmp/my bus
```

is represented as:

```text
/tmp/my%20bus
```

Structural characters must also be escaped when they are part of a value.

For example:

```text
/tmp/foo,bar
```

becomes:

```text
/tmp/foo%2Cbar
```

This distinction is important because a literal comma separates parameters.

---

## Parsing Order

Percent-decoding must happen **after** parsing the structural delimiters.

A suitable parsing sequence is:

```text
1. Split on literal ';'
   -> alternative addresses

2. Split each address on its first literal ':'
   -> transport and parameter string

3. Split the parameter string on literal ','
   -> individual parameters

4. Split each parameter on its first literal '='
   -> key and escaped value

5. Percent-decode each value

6. Apply transport-specific validation
```

Do not percent-decode the complete string before parsing it.

For example:

```text
unix:path=/tmp/foo%2Cbar
```

must produce:

```text
path = "/tmp/foo,bar"
```

and **not** two parameters.

Similarly:

```text
%3B
%3A
%3D
%25
```

must not be interpreted as structural delimiters before decoding.

---

## Generic Parser Representation

The generic address parser should ideally be independent from individual transports.

For example:

```erlang
-type dbus_address() ::
    #{
        transport := binary(),
        parameters := [{binary(), binary()}]
    }.

-type dbus_address_list() :: [dbus_address()].
```

Given:

```text
unix:path=/run/user/1000/bus,guid=0123456789abcdef0123456789abcdef
```

the generic parser could return:

```erlang
#{
    transport => <<"unix">>,
    parameters => [
        {<<"path">>, <<"/run/user/1000/bus">>},
        {<<"guid">>, <<"0123456789abcdef0123456789abcdef">>}
    ]
}
```

This representation has the advantage that the address syntax parser does not need to know which transports are implemented.

---

## Transport Interpretation

A second layer can convert the generic representation into transport-specific data.

For example:

```erlang
-type transport_address() ::
      {unix, #{
          path := binary()
      }}
    | {unix_abstract, #{
          abstract := binary()
      }}
    | {tcp, #{
          host := binary(),
          port := non_neg_integer(),
          family => ipv4 | ipv6
      }}
    | {nonce_tcp, #{
          host := binary(),
          port := non_neg_integer(),
          noncefile := binary(),
          family => ipv4 | ipv6
      }}
    | {launchd, #{
          env := binary()
      }}
    | {unixexec, #{
          path := binary(),
          argv => [binary()]
      }}
    | {autolaunch, #{
          scope => binary()
      }}
    | {unknown, binary(), [{binary(), binary()}]}.
```

This separates two concerns:

```text
address syntax
      ↓
generic parsed address
      ↓
transport interpretation
      ↓
connection implementation
```

---

## Unknown Transports

The generic parser should **not reject an address merely because the transport is unknown**.

The address format is extensible and new transports can be introduced.

For example:

```text
future-transport:foo=bar,baz=quux
```

can still be parsed as:

```erlang
#{
    transport => <<"future-transport">>,
    parameters => [
        {<<"foo">>, <<"bar">>},
        {<<"baz">>, <<"quux">>}
    ]
}
```

The connection layer can subsequently return:

```erlang
{error, unsupported_transport}
```

This keeps syntax parsing independent of transport support.

---

## GUID Handling

`guid` is a generic server-address attribute rather than a property specific to `unix`, `tcp`, or another transport.

For example:

```text
unix:path=/run/user/1000/bus,guid=0123456789abcdef0123456789abcdef
```

A semantic representation can therefore separate it from transport options:

```erlang
#{
    transport => unix,
    options => #{
        path => <<"/run/user/1000/bus">>
    },
    guid => <<"0123456789abcdef0123456789abcdef">>
}
```

The GUID is represented as 32 hexadecimal characters encoding 16 bytes.

---

## Recommended Architecture for `erlang-dbus`

A clean implementation can be divided into three layers:

```text
DBUS_SESSION_BUS_ADDRESS
           |
           v
+-----------------------+
| Address syntax parser |
+-----------------------+
           |
           v
 #{
   transport => <<"unix">>,
   parameters => [...]
 }
           |
           v
+-----------------------+
| Transport validator   |
+-----------------------+
           |
           v
 {unix, #{path => ...}}
           |
           v
+-----------------------+
| Transport connector   |
+-----------------------+
           |
           v
       D-Bus socket
```

The syntax parser should handle:

* address lists;
* structural delimiters;
* percent escaping;
* generic key/value parameters;
* unknown transports.

The transport layer should handle:

* required parameters;
* mutually exclusive parameters;
* conversion of values such as TCP ports;
* client versus listen-only addresses;
* unsupported transports.

The connector should handle:

* Unix sockets;
* TCP sockets;
* nonce exchange;
* platform-specific transports;
* fallback to subsequent addresses when connection fails.

---

## Minimal Linux Implementation

For an initial Linux implementation, supporting these two forms covers the normal session-bus case:

```text
unix:path=/run/user/1000/bus
```

and:

```text
unix:abstract=...
```

However, the parser itself should still implement the generic:

```text
transport:key=value,key=value
```

grammar.

This allows additional transports to be implemented later without changing the address parser. 
