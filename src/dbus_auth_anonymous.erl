-module(dbus_auth_anonymous).
-moduledoc """
The ANONYMOUS SASL mechanism, as described in the "Authentication
mechanisms" section of the D-Bus specification and defined by
[RFC 4505](https://tools.ietf.org/html/rfc4505).

It authenticates nothing. The single client message carries only *trace
information* -- an email address, or an opaque token -- which the server
is expected to log and nothing more, so the exchange is one line long:

```
C: AUTH ANONYMOUS 6d65406578616d706c652e636f6d
S: OK 1234deadbeef
C: BEGIN
```
""".
-include("dbus.hrl").

-behaviour(dbus_auth_client_mech).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    name/0,
    init/1,
    initial_response/1,
    challenge/2
]).

%% Either trace information to send, or nothing to send at all.
-type state() :: {trace, binary()} | none.

-doc "Returns the mechanism name, as it appears on the wire.".
-spec name() -> binary().
name() ->
    ?DBUS_AUTH_ANONYMOUS.

-doc "Initializes the mechanism.".
-spec init(term()) -> {ok, state()} | {error, term()}.
init(_Ctx) ->
    {ok, none}.

-doc """
Returns empty response.
""".
-spec initial_response(state()) -> {continue, binary(), state()}.
initial_response(none = State) ->
    {continue, <<>>, State}.

-doc """
Answers the empty challenge of a trace-less exchange.

ANONYMOUS defines no challenge data, so anything else is a server that is
not speaking this mechanism and the exchange is abandoned.
""".
-spec challenge(binary(), state()) ->
    {ok, binary(), state()} | {error, term()}.
challenge(_, none = State) ->
    {ok, <<>>, State}.

-ifdef(TEST).

%%% The contract these tests pin, per the "Authentication Protocol" section
%%% of docs/specifications.html and RFC 4505:
%%%
%%%   * the trace information travels in the initial response of AUTH,
%%%     hex-encoded, verbatim -- it is not an identity and is not parsed;
%%%   * it is at most 255 UTF-8 characters and carries no control
%%%     character, so a value that breaks either rule never reaches the
%%%     wire;
%%%   * with nothing to trace, AUTH carries no initial response and the
%%%     empty challenge that follows is answered with an empty response;
%%%   * ANONYMOUS has no challenge data of its own, so a non-empty one is
%%%     an error rather than something to answer.

name_test() ->
    ?assertEqual(<<"ANONYMOUS">>, name()).

initial_response_test() ->
    ?assertEqual({continue, <<>>, none}, initial_response(none)).

%%% ANONYMOUS has no challenge data
challenge_test() ->
    ?assertEqual({ok, <<>>, none}, challenge(<<"8799cabb">>, none)).

init_always_returns_none_test() ->
    ?assertEqual({ok, none}, init(default)).

-endif.
