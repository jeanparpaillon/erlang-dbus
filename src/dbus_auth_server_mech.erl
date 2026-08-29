-module(dbus_auth_server_mech).
-moduledoc """
Defines the behaviour for authentication server mechanisms.
""".
-type state() :: term().

-callback name() -> binary().

-callback init(map()) ->
    {ok, state()} | {error, term()}.

-callback response(binary(), state()) ->
    {continue, binary(), state()}
    | {ok, state()}
    | {rejected, term()}.
