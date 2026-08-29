-module(dbus_auth).
-moduledoc """
Defines callbacks for implementing an SASL authentication mechanism.

The authentication state machine is implemented in `m:dbus_connection`.

```erlang
-callback init() ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, term()}.
```

Returns a binary to be sent to the other side.

- `{ok, binary()}`: state-machine waits for `OK` or `REJECT`
- `{continue, binary()}`: state-machine waits for a challenge (`DATA ...`) or `REJECT`
- `{error, term()}`: an error occurred while initializing the mechanism

```erlang
-callback challenge(Chall :: binary(), State :: term()) ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, Reason :: term()}.
```

Called when receiving a challenge from the server. Answers have the same meaning
as for `c:init/0`.

See [D-Bus Specification](https://dbus.freedesktop.org/doc/dbus-specification.html#auth-mechanisms)
and [RFC 4422](https://tools.ietf.org/html/rfc4422) for the complete specification
of the mechanisms.
""".

-callback init() ->
    {ok, Resp :: binary()}
    | {continue, Resp :: binary(), State :: term()}
    | {error, term()}.

-callback challenge(Chall :: binary(), State :: term()) ->
    {ok, Resp :: binary()}
    | {continue, Resp :: binary(), State :: term()}
    | {error, Reason :: term()}.
