-module(dbus_auth_client_mech).
-moduledoc """
Defines the behaviour for authentication client mechanisms.
""".

-type state() :: term().

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
