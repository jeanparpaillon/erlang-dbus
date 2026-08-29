# D-Bus Protocol Layers

D-Bus can be understood as a small protocol stack.

The comparison with the OSI network model is not exact: D-Bus is primarily an
IPC and RPC protocol, not a network protocol. However, the OSI model provides a
useful analogy for separating the responsibilities of a D-Bus implementation.

## Overview

| D-Bus layer | Rough OSI analogy | Responsibility |
|---|---|---|
| Transport | L4 — Transport | Establish an ordered byte stream |
| Connection / Authentication | L5 — Session | Establish an authenticated D-Bus session |
| Message Protocol | L6 — Presentation | Frame, marshal and unmarshal typed messages |
| Bus | L3 — Network, approximately | Route messages between connections |
| Object / RPC | L7 — Application | Address objects and invoke methods |
| Application API | L7 — Application | Define service-specific semantics |

A useful implementation model is:

    Transport
        ↓
    Connection
        ↓
    Message Protocol
        ↓
    Bus
        ↓
    Object / RPC
        ↓
    Application API

The important distinction is that a **D-Bus connection does not necessarily
involve a message bus**. D-Bus can also be used directly between two peers.

---

## 1. Transport Layer

**OSI analogy:** Layer 4 — Transport

The transport layer provides an ordered byte stream between two D-Bus peers.

Examples of D-Bus transports include:

- Unix-domain sockets
- TCP
- nonce-TCP
- `unixexec`
- platform-specific transports

A D-Bus address selects the transport and provides the parameters needed to
establish it.

For example:

    unix:path=/run/user/1000/bus

or:

    tcp:host=localhost,port=1234

Conceptually:

    D-Bus peer
        │
        │ byte stream
        │
    ───────────────
     Unix / TCP / ...
    ───────────────
        │
        │
    D-Bus peer

The transport layer should not need to understand D-Bus messages. Its role is
essentially:

    connect(Address) -> Stream

and then:

    read(Stream)
    write(Stream, Bytes)

A D-Bus address parser therefore belongs naturally close to this layer.

---

## 2. Connection Layer

**OSI analogy:** Layer 5 — Session

Once the transport has been established, D-Bus performs authentication and
protocol negotiation.

A simplified connection establishment sequence is:

    transport connect
          │
          ▼
       AUTH ...
          │
          ▼
       OK <guid>
          │
          ├── NEGOTIATE_UNIX_FD
          │
          └── AGREE_UNIX_FD
          │
          ▼
        BEGIN
          │
          ▼
    D-Bus message stream

The authentication protocol runs before normal D-Bus messages are exchanged.

After `BEGIN`, communication switches to the binary D-Bus message protocol.

This layer is therefore conceptually similar to an OSI **session layer**:
it establishes the protocol relationship over an already-established
transport.

A connection can be modeled as:

    connection =
        transport
        + authentication state
        + negotiated capabilities
        + D-Bus message stream

### Direct peer-to-peer connections

A D-Bus connection does not imply the existence of a message bus.

Two applications can communicate directly:

    Process A
        │
        │ D-Bus connection
        │
    Process B

After authentication, the connection is essentially symmetric and full-duplex.

This distinction is important when designing a D-Bus library:

    Connection ≠ Bus

---

## 3. Message Protocol Layer

**OSI analogy:** Layer 6 — Presentation

Above the connection is the D-Bus binary message protocol.

This layer defines how structured D-Bus data is represented on the byte stream.

A conceptual message looks like:

    +--------------------------------+
    | D-Bus Message                  |
    +--------------------------------+
    | type: METHOD_CALL              |
    | serial: 42                     |
    | path: /foo/bar                 |
    | interface: com.example.Foo     |
    | member: DoSomething            |
    | signature: "si"                |
    +--------------------------------+
    | Body                           |
    +--------------------------------+
    | "hello"                        |
    | 123                            |
    +--------------------------------+

Responsibilities include:

- message framing
- byte order
- alignment
- type signatures
- serialization
- deserialization
- header encoding
- body encoding

The main D-Bus message types are:

    METHOD_CALL
    METHOD_RETURN
    ERROR
    SIGNAL

Messages also contain header fields such as:

    PATH
    INTERFACE
    MEMBER
    DESTINATION
    SENDER
    SIGNATURE
    REPLY_SERIAL

This layer maps reasonably well to OSI Layer 6 because it defines the wire
representation of structured application data.

---

## 4. Bus Layer

**OSI analogy:** Layer 3 — Network, approximately

The message bus is a special D-Bus application connected to multiple D-Bus
peers.

Its main responsibility is routing messages between those connections.

For example:

      Application A             Application B
            │                         │
       Connection                Connection
            │                         │
            └────────┐       ┌────────┘
                     ▼       ▼
                  +-------------+
                  | Message Bus |
                  |             |
                  | routing     |
                  | names       |
                  | matching    |
                  +-------------+
                         │
                    Connection
                         │
                         ▼
                   Application C

The comparison with the OSI network layer is useful because both provide an
addressing and routing function.

Approximately:

| Network concept | D-Bus equivalent |
|---|---|
| Network router | Message bus |
| Network endpoint address | Unique bus name |
| Stable service address | Well-known bus name |
| Destination address | `DESTINATION` header |
| Routing table | Bus name → connection mapping |
| Multicast-like subscription | Signals + match rules |

This analogy should not be interpreted literally. A D-Bus bus name is not an
IP address and the message bus does not route network packets.

### Unique names

Each connection to a message bus receives a unique name such as:

    :1.42

The unique name identifies that particular connection.

Conceptually:

    :1.42
       │
       ▼
    Connection #42

### Well-known names

Applications can additionally acquire stable service names:

    org.example.Scanner

The bus then maintains a mapping such as:

    org.example.Scanner
            │
            ▼
          :1.42
            │
            ▼
      Connection #42

This is somewhat analogous to resolving a stable service name to a particular
network endpoint.

### Hello

A newly established connection is initially just a D-Bus connection to the
message bus.

It becomes a normal bus participant by calling:

    org.freedesktop.DBus.Hello()

The bus then assigns its unique name.

Conceptually:

    Transport
        ↓
    Authentication
        ↓
    D-Bus Connection
        ↓
    Hello()
        ↓
    Bus Connection
        ↓
    Unique name (:1.x)

Therefore:

    D-Bus connection
        +
    bus registration / naming semantics
        =
    connection participating in a message bus

---

## 5. Object and RPC Layer

**OSI analogy:** Layer 7 — Application

Once a message reaches the correct connection, D-Bus provides another level of
addressing inside the target application.

For example:

    destination = org.example.Scanner
    path        = /org/example/Scanner1
    interface   = org.example.Scanner1
    member      = Scan

These fields progressively identify:

1. the service
2. the connection implementing the service
3. an object within the service
4. an interface implemented by the object
5. an operation on that interface

Conceptually:

    Bus
     │
     └── org.example.Scanner
              │
              └── :1.42
                    │
                    ├── /org/example/Scanner1
                    │       │
                    │       ├── org.example.Scanner1
                    │       │       ├── Scan()
                    │       │       └── Cancel()
                    │       │
                    │       └── org.freedesktop.DBus.Properties
                    │
                    └── /org/example/Job/42
                            │
                            └── org.example.Job1

The object path behaves somewhat like a hierarchical resource identifier:

    /org/example/Scanner1

An object can expose several interfaces, and each interface can contain:

- methods
- signals
- properties

This is firmly an application/RPC layer.

---

## 6. Application API Layer

**OSI analogy:** Layer 7 — Application

At the highest level are the actual interfaces implemented by applications.

Examples include standard interfaces such as:

    org.freedesktop.DBus.Properties
    org.freedesktop.DBus.Introspectable

and application-specific interfaces such as:

    org.example.Scanner1

At this level, D-Bus becomes an RPC/object protocol:

    Scan(options)
        ↓
    METHOD_CALL
        ↓
    METHOD_RETURN

or:

    JobCompleted(...)
        ↓
      SIGNAL

This layer defines the semantic contract between applications rather than the
mechanics of transporting messages.

---

# Complete Stack

The resulting conceptual stack is:

    +--------------------------------------------------+
    | Application API                                  |
    | service-specific methods/properties/signals      |
    +--------------------------------------------------+
                         L7
    +--------------------------------------------------+
    | Object / RPC                                     |
    | path, interface, member, method/reply semantics  |
    +--------------------------------------------------+
                         L7
    +--------------------------------------------------+
    | Bus                                              |
    | Hello, names, routing, match rules               |
    +--------------------------------------------------+
                      ~ L3-ish
    +--------------------------------------------------+
    | Message Protocol                                 |
    | headers, bodies, types, marshal/unmarshal        |
    +--------------------------------------------------+
                         L6
    +--------------------------------------------------+
    | Connection                                       |
    | authentication, negotiation, capabilities        |
    +--------------------------------------------------+
                         L5
    +--------------------------------------------------+
    | Transport                                        |
    | unix, tcp, nonce-tcp, unixexec, ...              |
    +--------------------------------------------------+
                         L4
    +--------------------------------------------------+
    | Operating System                                 |
    | Unix sockets, TCP sockets, file descriptors      |
    +--------------------------------------------------+

# Implications for a D-Bus Implementation

A clean implementation can consequently separate the modules into roughly:

    Address
       │
       ▼
    Transport
       │
       ▼
    Connection
       │
       ▼
    Message Codec
       │
       ▼
    Bus
       │
       ▼
    Object / RPC
       │
       ▼
    Application API

In particular, **bus semantics should not be required by the connection
layer**.

A generic D-Bus connection should ideally provide operations conceptually
similar to:

    send(Connection, Message)

    receive(Connection) -> Message

This connection can then be used either directly:

    Peer A <──── D-Bus connection ────> Peer B

or through a bus:

    Peer A
       │
       │ connection
       ▼
    Message Bus
       │
       │ connection
       ▼
    Peer B

The higher-level bus abstraction can add:

- `Hello`
- unique names
- well-known names
- `RequestName`
- `ReleaseName`
- match rules
- signal subscriptions
- bus routing semantics

This separation preserves an important property of the D-Bus architecture:

> **D-Bus is the peer-to-peer protocol; a message bus is one application of
> that protocol.**
