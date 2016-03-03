

# Module dbus_auth #
* [Description](#description)

Defines callbacks for implemeting an SASL authentication
mechanism.

Copyright (c) (C) 2014, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

__See also:__ [
```
   -callback init() ->
     {ok, Resp :: binary()} |
     {continue, Resp :: binary(), State :: term()} |
     {error, term()}.
```

Returns a binary to be sent to other side.
* `{ok, binary()}`: state-machine waits for `OK` or `REJECT`
* `{continue, binary()}`: state-machine waits for a challenge (`DATA ...`) or `REJECT`
* `{error, term()}`: an error occurred while initializing the mechanism

```
     -callback challenge(Chall :: binary(), State :: term()) ->
     {ok, Resp :: binary()} |
     {continue, Resp :: binary(), State :: term()} |
     {error, Reason :: term()}.
```

Called when receiving a challenge from the server.
Answers has the same meaning as init @see init/0.

See [D-Bus Specification](https://dbus.freedesktop.org/doc/dbus-specification.md#auth-mechanisms)
and [RFC 4422](https://tools.ietf.org.md/rfc4422).
for complete specification of the mechanisms.
](dbus_peer_conection.md).

<a name="description"></a>

## Description ##
Authentication state machine is implemented in