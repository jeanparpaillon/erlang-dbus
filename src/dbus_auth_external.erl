-module(dbus_auth_external).
-moduledoc """
The EXTERNAL SASL mechanism, as described in the "Authentication
mechanisms" section of the D-Bus specification
and in [RFC 4422](https://tools.ietf.org/html/rfc4422) appendix A.

The mechanism carries no secret: it asserts an *authorization identity*
and lets the server check it against credentials obtained out of band --
on Unix, the ones that ride along with the credentials-passing nul byte
`dbus_connection' sends before the conversation starts. There is
therefore no challenge-response exchange, only the identity in the initial
response, hex-encoded like every other argument of the protocol:

```
C: AUTH EXTERNAL 31303030
S: OK 1234deadbeef
C: BEGIN
```

`31303030' is `"1000"', the ASCII decimal form of a Unix uid: the
specification asks interoperable Unix clients to send exactly that rather
than a login name. This module builds it from the *effective* identity of
the running VM, not from `$USER', which says who logged in and can be
stale or absent under a systemd unit -- the uid is what the kernel will
report to the server, so anything else would only disagree with it.

Auth context can be:

| Type | Authorization identity sent |
|---|---|
| `integer()` | user ID |
| `binary()` | user ID/name as a binary |
| `none` | no identity |
| `uid` | user ID of the running VM |

If not specified, `uid` is autodetected
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

%% Either an identity to assert, or nothing to assert at all.
-type state() :: {identity, binary()} | none.

-doc "Returns the mechanism name, as it appears on the wire.".
-spec name() -> binary().
name() ->
    ?DBUS_AUTH_EXTERNAL.

-doc "Resolves the authorization identity from the auth context.".
-spec init(term()) -> {ok, state()} | {error, term()}.

init(none) ->
    {ok, none};
init(Uid) when is_integer(Uid) andalso Uid >= 0 ->
    {ok, {identity, integer_to_binary(Uid)}};
init(Uid) when is_integer(Uid) ->
    {error, {invalid_identity, Uid}};
init(Uid) when is_binary(Uid) ->
    {ok, {identity, Uid}};
init(uid) ->
    case dbus_auth:detect_uid() of
        {ok, Uid} -> {ok, {identity, integer_to_binary(Uid)}};
        error -> {ok, none}
    end;
init(undefined) ->
    init(uid);
init(Err) ->
    {error, {invalid_identity, Err}}.

-doc """
Returns the identity, hex-encoded, as the initial response.

With no identity to send, the initial response is omitted and the
mechanism continues: the server answers `DATA' with an empty challenge,
which `challenge/2' completes with an empty response. A server that
instead replies `OK' straight away is handled by the state machine, so
both readings of an argument-less `AUTH EXTERNAL' work.
""".
-spec initial_response(state()) ->
    {ok, binary(), state()} | {continue, binary(), state()}.
initial_response({identity, Identity} = State) ->
    {ok, hex(Identity), State};
initial_response(none = State) ->
    {continue, <<>>, State}.

-doc """
Answers the empty challenge of an identity-less exchange.

EXTERNAL defines no challenge data, so anything else is a server that is
not speaking this mechanism and the exchange is abandoned.
""".
-spec challenge(binary(), state()) ->
    {ok, binary(), state()} | {error, term()}.
challenge(<<>>, none = State) ->
    {ok, <<>>, State};
challenge(_Challenge, _State) ->
    {error, invalid_challenge}.

%%%
%%% Priv
%%%

%% Lower-case hex: the protocol accepts either, the specification writes
%% its examples in lower case, and DBUS_COOKIE_SHA1 forbids upper case --
%% no reason for the two mechanisms to disagree.
hex(Bin) ->
    binary:encode_hex(Bin, lowercase).

-ifdef(TEST).

%%% The contract these tests pin, per the "Authentication Protocol" section
%%% of docs/specifications.html:
%%%
%%%   * the identity travels in the initial response of AUTH, hex-encoded,
%%%     and is the ASCII decimal uid on Unix -- figure 1 of the
%%%     specification is `AUTH EXTERNAL 31303030' for uid 1000;
%%%   * a non-numeric identity, such as the Windows SID of figure 3, is
%%%     sent verbatim;
%%%   * with no identity, AUTH carries no initial response and the empty
%%%     challenge that follows is answered with an empty response;
%%%   * EXTERNAL has no challenge data of its own, so a non-empty one is
%%%     an error rather than something to answer.

%% A test reads as the configured value, then what reaches the wire for it.
first_response(Value) ->
    {ok, State} = init(Value),
    initial_response(State).

name_test() ->
    ?assertEqual(<<"EXTERNAL">>, name()).

%%% Identity from the context

uid_1000_test() ->
    %% figure 1: 31303030 is "1000"
    ?assertEqual({ok, <<"31303030">>, {identity, <<"1000">>}}, first_response(1000)).

root_uid_test() ->
    ?assertEqual({ok, <<"30">>, {identity, <<"0">>}}, first_response(0)).

windows_sid_test() ->
    %% figure 3: 532d312d352d3138 is "S-1-5-18"
    ?_assertEqual(
        {ok, <<"532d312d352d3138">>, {identity, <<"S-1-5-18">>}},
        first_response(<<"S-1-5-18">>)
    ).

invalid_identity_test_() ->
    [
        ?_assertEqual({error, {invalid_identity, -1}}, init(-1)),
        ?_assertEqual({error, {invalid_identity, an_atom}}, init(an_atom)),
        ?_assertMatch({error, {invalid_identity, _}}, init([1000]))
    ].

%%% No identity: AUTH EXTERNAL alone, then an empty DATA

no_identity_test_() ->
    {ok, State} = init(none),
    [
        ?_assertEqual(none, State),
        ?_assertEqual({continue, <<>>, none}, initial_response(State)),
        ?_assertEqual({ok, <<>>, none}, challenge(<<>>, State))
    ].

%%% EXTERNAL has no challenge data

invalid_challenge_test_() ->
    [
        ?_assertEqual({error, invalid_challenge}, challenge(<<"8799cabb">>, none)),
        ?_assertEqual({error, invalid_challenge}, challenge(<<>>, {identity, <<"1000">>})),
        ?_assertEqual(
            {error, invalid_challenge},
            challenge(<<"8799cabb">>, {identity, <<"1000">>})
        )
    ].

%%% Default: the uid of this VM, which must reach the wire as the hex of a
%%% run of ASCII digits

default_is_detected_uid_test() ->
    {ok, State} = init(undefined),
    ?assertMatch({identity, _}, State),
    {identity, Uid} = State,
    ?assert(byte_size(Uid) > 0),
    ?assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Uid))),
    ?assertEqual({ok, binary:encode_hex(Uid, lowercase), State}, initial_response(State)).

%% An unconfigured mechanism is handed `undefined' by
%% dbus_auth_client_mech, and must behave as if `uid' had been asked for.
uid_atom_is_the_default_test() ->
    ?assertEqual(init(undefined), init(uid)).

-endif.
