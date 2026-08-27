-module(dbus_auth_anonymous).
-moduledoc """
Implements ANONYMOUS authentication.

See [RFC 2245](https://tools.ietf.org/html/rfc2245) for the complete specification.
""".

-behaviour(dbus_auth).

%% dbus_auth callbacks
-export([init/0,
         challenge/2]).

-doc "Init ANONYMOUS mechanism.".
-spec init() -> {ok, binary()}.
init() ->
    {ok, <<"ANONYMOUS">>}.


-doc "Not implemented: ANONYMOUS does not require challenge.".
-spec challenge(binary(), any()) -> {error, invalid_challenge}.
challenge(_, _) ->
    {error, invalid_challenge}.
