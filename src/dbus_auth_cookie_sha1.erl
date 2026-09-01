-module(dbus_auth_cookie_sha1).
-moduledoc """
The DBUS_COOKIE_SHA1 mechanism, as described in the "Authentication
mechanisms" section of the D-Bus specification.

It is not a SASL mechanism from a RFC: it is D-Bus' own, and it proves one
thing only -- that the client can *read a private file owned by the user it
claims to be*. The shared secret is a cookie sitting in
`~/.dbus-keyrings/<context>', written there by the server; demonstrating
knowledge of it is the whole authentication. Its security therefore rests
entirely on the home directory being private, which is why this mechanism
is the recommended one exactly where EXTERNAL cannot work -- `tcp:', where
no credentials ride along with the socket -- and pointless elsewhere.

The exchange is three lines, everything hex-encoded:

```
C: AUTH DBUS_COOKIE_SHA1 31303030
S: DATA 6f72675f667265... ("org_freedesktop_general 3 1234abcd")
C: DATA 636166656261... ("cafebabe 47bf1b6b2cd3704fe1aabfa6181ffeb230bd0b67")
S: OK 1234deadbeef
C: BEGIN
```

The server names its *cookie context* (which file), the *cookie ID* (which
line of it) and a random challenge. The client answers with its own random
challenge and the SHA-1 hex digest of

    <server challenge>:<client challenge>:<cookie>

concatenated as text -- the cookie is the hex string as it appears in the
file, not the bytes it encodes, and neither challenge is decoded either.
The server recomputes the digest from its own copy of the cookie and
compares.

The identity comes from the auth context, as it does for
`dbus_auth_external':

| Value | Identity sent |
|---|---|
| absent, or `uid' | the uid of this process, detected (the default) |
| an `integer()' | that uid, in ASCII decimal |
| a `binary()' or `string()' | verbatim, e.g. a login name |
| `#{user => V, keyring_dir => Dir}' | `V' as above, cookies read from `Dir' |

The specification says "the username", and the reference implementation
sends the ASCII decimal uid on Unix -- its server side accepts either,
looking the string up in the user database as a name unless it parses as a
number. The uid is the default here for the same reason as in EXTERNAL:
`$USER' says who logged in, which can be stale or absent under a systemd
unit, while the uid is what actually owns the keyring being read.

`init/1' does no I/O. Everything that can fail because of the *environment*
rather than the configuration -- a missing keyring directory, one with
loose permissions, an unknown cookie ID -- fails in `challenge/2' instead,
where the failure becomes an `ERROR' on the wire and the server's
`REJECTED' lets `dbus_auth_client_mech' fall through to the next mechanism.
Failing in `init/1' would abort the whole conversation.

Only the client side is implemented: serving this mechanism means owning
the cookie file, with the locking and pruning protocol the specification
lays out for writers.
""".
-include("dbus.hrl").
-include_lib("kernel/include/file.hrl").

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

%% Bytes of client challenge, hex-encoded before use. The specification
%% fixes no size; this is what the reference implementation uses.
-define(CHALLENGE_BYTES, 16).

-define(DEFAULT_KEYRING_DIR, ".dbus-keyrings").

-record(state, {
    user :: binary(),
    %% `undefined' when no home directory could be found -- reported from
    %% challenge/2, see the moduledoc.
    keyring_dir :: file:filename_all() | undefined,
    %% The mechanism answers exactly one challenge.
    phase = waiting_challenge :: waiting_challenge | done
}).

-type state() :: #state{}.

-doc "Returns the mechanism name, as it appears on the wire.".
-spec name() -> binary().
name() ->
    ?DBUS_AUTH_DBUS_COOKIE_SHA1.

-doc "Resolves the identity and the keyring directory from the auth context.".
-spec init(term()) -> {ok, state()} | {error, term()}.
init(Ctx) ->
    case user(user_option(Ctx)) of
        {ok, User} ->
            {ok, #state{user = User, keyring_dir = keyring_dir(Ctx)}};
        {error, _Reason} = Err ->
            Err
    end.

-doc """
Sends the identity, hex-encoded, and waits for the server's challenge.

There is no shortcut: the mechanism always continues, since the cookie
cannot be located before the server has named its context and cookie ID.
""".
-spec initial_response(state()) -> {continue, binary(), state()}.
initial_response(#state{user = User} = State) ->
    {continue, hex(User), State}.

-doc """
Answers `<context> <cookie id> <server challenge>' with
`<client challenge> <digest>'.

The context names a file inside the keyring directory, so it is validated
against the character set the specification gives for it -- no slash, no
backslash, no period -- before it is joined to a path: a server that is
lying about its context must not be able to walk out of the directory.
""".
-spec challenge(binary(), state()) ->
    {ok, binary(), state()} | {error, term()}.
challenge(Challenge, #state{phase = waiting_challenge} = State) ->
    case parse_challenge(Challenge) of
        {ok, Context, CookieId, ServerChallenge} ->
            answer(Context, CookieId, ServerChallenge, State);
        {error, _Reason} = Err ->
            Err
    end;
challenge(_Challenge, #state{phase = done}) ->
    {error, unexpected_challenge}.

%%%
%%% Priv
%%%
answer(Context, CookieId, ServerChallenge, State) ->
    case read_cookie(State#state.keyring_dir, Context, CookieId) of
        {ok, Cookie} ->
            ClientChallenge = generate_challenge(),
            Response = response(ServerChallenge, ClientChallenge, Cookie),
            {ok, Response, State#state{phase = done}};
        {error, _Reason} = Err ->
            Err
    end.

%%% The auth context

user_option(#{user := User}) -> User;
user_option(Ctx) when is_map(Ctx) -> uid;
user_option(undefined) -> uid;
user_option(Ctx) -> Ctx.

keyring_dir(#{keyring_dir := Dir}) ->
    Dir;
keyring_dir(_Ctx) ->
    case os:getenv("HOME") of
        false -> undefined;
        Home -> filename:join(Home, ?DEFAULT_KEYRING_DIR)
    end.

user(uid) ->
    case detect_uid() of
        {ok, Uid} -> {ok, Uid};
        error -> {error, no_identity}
    end;
user(Uid) when is_integer(Uid), Uid >= 0 ->
    {ok, integer_to_binary(Uid)};
user(User) when is_binary(User), User =/= <<>> ->
    {ok, User};
user(User) when is_list(User), User =/= [] ->
    try
        {ok, list_to_binary(User)}
    catch
        error:badarg -> {error, {invalid_identity, User}}
    end;
user(User) ->
    {error, {invalid_identity, User}}.

%% `/proc/self' is owned by the uid the process runs as, which is the uid
%% that owns the keyring being read. `id -u' is the portable answer.
detect_uid() ->
    case file:read_file_info("/proc/self") of
        {ok, #file_info{uid = Uid}} when is_integer(Uid) ->
            {ok, integer_to_binary(Uid)};
        _ ->
            uid_from_id_command()
    end.

uid_from_id_command() ->
    case string:trim(os:cmd("id -u 2>/dev/null")) of
        [] ->
            error;
        Out ->
            case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Out) of
                true -> {ok, list_to_binary(Out)};
                false -> error
            end
    end.

%%% The server's challenge

parse_challenge(Hex) ->
    case unhex(Hex) of
        {ok, Data} ->
            parse_challenge_fields(binary:split(Data, <<" ">>, [global]));
        error ->
            {error, invalid_challenge}
    end.

parse_challenge_fields([Context, CookieId, ServerChallenge]) ->
    case {valid_context(Context), valid_cookie_id(CookieId)} of
        {true, true} ->
            {ok, Context, CookieId, ServerChallenge};
        {false, _} ->
            {error, {invalid_cookie_context, Context}};
        {_, false} ->
            {error, {invalid_cookie_id, CookieId}}
    end;
parse_challenge_fields(_Fields) ->
    {error, invalid_challenge}.

%% "Context names must be valid ASCII, nonzero length, and may not contain
%% the characters slash, backslash, space, newline, carriage return, tab, or
%% period." Rejecting the period is what makes ".." unspellable.
valid_context(<<>>) ->
    false;
valid_context(Context) ->
    lists:all(fun context_char/1, binary_to_list(Context)).

context_char(C) when C < 16#20; C > 16#7E -> false;
context_char($/) -> false;
context_char($\\) -> false;
context_char($\s) -> false;
context_char($.) -> false;
context_char(_C) -> true.

%% "The cookie ID number, which must be a non-negative integer".
valid_cookie_id(<<>>) ->
    false;
valid_cookie_id(Id) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Id)).

%%% The cookie file

read_cookie(undefined, _Context, _CookieId) ->
    {error, no_keyring_dir};
read_cookie(Dir, Context, CookieId) ->
    case private_dir(Dir) of
        ok ->
            case file:read_file(filename:join(Dir, Context)) of
                {ok, Contents} -> find_cookie(Contents, CookieId);
                {error, Reason} -> {error, {no_cookie_file, Reason}}
            end;
        {error, _Reason} = Err ->
            Err
    end.

%% "This directory must not be readable or writable by other users. If it
%% is, clients and servers must ignore it." A keyring somebody else can
%% read is a keyring somebody else can authenticate with.
private_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, #file_info{type = directory, mode = Mode}} when Mode band 8#077 =:= 0 ->
            ok;
        {ok, #file_info{type = directory}} ->
            {error, keyring_dir_not_private};
        {ok, _Info} ->
            {error, keyring_dir_not_a_directory};
        {error, Reason} ->
            {error, {no_keyring_dir, Reason}}
    end.

%% One cookie per line, three space-separated fields: ID, creation time,
%% cookie. Clients need not lock the file -- servers rewrite it atomically.
find_cookie(Contents, CookieId) ->
    Lines = binary:split(Contents, [<<"\n">>, <<"\r\n">>], [global, trim_all]),
    find_cookie_line(Lines, CookieId).

find_cookie_line([], _CookieId) ->
    {error, no_cookie};
find_cookie_line([Line | Rest], CookieId) ->
    case binary:split(Line, <<" ">>, [global]) of
        [CookieId, _Time, Cookie] when Cookie =/= <<>> ->
            {ok, Cookie};
        _Other ->
            find_cookie_line(Rest, CookieId)
    end.

%%% The response

%% The challenge only has to be unguessable and free of the space and colon
%% that delimit the digest input; hex of random bytes is both.
generate_challenge() ->
    hex(crypto:strong_rand_bytes(?CHALLENGE_BYTES)).

response(ServerChallenge, ClientChallenge, Cookie) ->
    Composite = <<ServerChallenge/binary, $:, ClientChallenge/binary, $:, Cookie/binary>>,
    Digest = hex(crypto:hash(sha, Composite)),
    hex(<<ClientChallenge/binary, $\s, Digest/binary>>).

%% "hex encoding must output the digits from a to f in lower-case; the
%% digits A to F must not be used in the DBUS_COOKIE_SHA1 mechanism." That
%% binds what we write; what the server writes is decoded either way.
hex(Bin) ->
    binary:encode_hex(Bin, lowercase).

unhex(Hex) ->
    try
        {ok, binary:decode_hex(Hex)}
    catch
        error:badarg -> error
    end.


-ifdef(TEST).

%%% The contract these tests pin, per the "Authentication mechanisms"
%%% section of docs/specifications.html:
%%%
%%%   * the identity travels in the initial response of AUTH, hex-encoded,
%%%     and the mechanism always continues -- the cookie cannot be found
%%%     before the server has named it;
%%%   * the server's DATA decodes to `<context> <id> <challenge>', and the
%%%     answer is the hex of `<client challenge> <sha1 hex digest>' over
%%%     `<server challenge>:<client challenge>:<cookie>' -- all as text,
%%%     nothing decoded, digits a-f in lower case;
%%%   * the cookie comes from the line of `<keyring>/<context>' whose first
%%%     field is the cookie ID, out of three space-separated fields;
%%%   * a keyring directory other users can read or write is ignored, and a
%%%     context name is never allowed to name a file outside the keyring;
%%%   * everything environmental fails as an error the state machine can
%%%     turn into ERROR, so another mechanism gets its turn.

%% The worked example used throughout: uid 1000, context
%% "org_freedesktop_general", cookie 3, server challenge "1234abcd".
-define(CONTEXT, "org_freedesktop_general").

server_data() ->
    hex(<<"org_freedesktop_general 3 1234abcd">>).

cookie_file() ->
    <<
        "1 1234567890 aaaabbbb\n"
        "3 1234567891 deadbeef\n"
        "7 1234567892 ccccdddd\n"
    >>.

ctx(Dir) ->
    #{user => 1000, keyring_dir => Dir}.

answer_challenge(Ctx, Data) ->
    {ok, State} = init(Ctx),
    {continue, _, State1} = initial_response(State),
    challenge(Data, State1).

%% Runs Fun against a keyring directory holding one context file, with the
%% mode the specification requires of it, and removes it afterwards.
with_keyring(Fun) ->
    with_keyring(cookie_file(), 8#700, Fun).

with_keyring(Contents, Mode, Fun) ->
    Name = io_lib:format("dbus-keyring-test-~b", [erlang:unique_integer([positive])]),
    Dir = filename:join(temp_dir(), lists:flatten(Name)),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(filename:join(Dir, ?CONTEXT), Contents),
    ok = file:change_mode(Dir, Mode),
    try
        Fun(Dir)
    after
        %% A directory kept read-only for the test cannot be emptied.
        ok = file:change_mode(Dir, 8#700),
        ok = file:del_dir_r(Dir)
    end.

temp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        Dir -> Dir
    end.

name_test() ->
    ?assertEqual(<<"DBUS_COOKIE_SHA1">>, name()).

%%% The identity in the initial response

initial_response_test_() ->
    Identity = fun(Value) ->
        {ok, State} = init(Value),
        {continue, Response, _} = initial_response(State),
        Response
    end,
    [
        %% "1000", as the reference implementation sends on Unix
        ?_assertEqual(<<"31303030">>, Identity(1000)),
        ?_assertEqual(<<"30">>, Identity(0)),
        %% a login name is equally acceptable to the reference server
        ?_assertEqual(<<"6a65616e">>, Identity(<<"jean">>)),
        ?_assertEqual(<<"6a65616e">>, Identity("jean")),
        ?_assertEqual(<<"31303030">>, Identity(#{user => 1000}))
    ].

invalid_identity_test_() ->
    [
        ?_assertEqual({error, {invalid_identity, -1}}, init(-1)),
        ?_assertEqual({error, {invalid_identity, an_atom}}, init(an_atom)),
        ?_assertEqual({error, {invalid_identity, <<>>}}, init(<<>>)),
        ?_assertMatch({error, {invalid_identity, _}}, init([1000]))
    ].

%% An unconfigured mechanism is handed `undefined' by
%% dbus_auth_client_mech, and must behave as if `uid' had been asked for.
default_is_detected_uid_test_() ->
    {ok, #state{user = Uid} = State} = init(undefined),
    [
        ?_assertEqual(init(uid), init(undefined)),
        ?_assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Uid))),
        ?_assertMatch({continue, _, _}, initial_response(State))
    ].

%% Without an explicit directory the cookies come from the home directory.
default_keyring_dir_test() ->
    {ok, #state{keyring_dir = Dir}} = init(undefined),
    ?assertEqual(filename:join(os:getenv("HOME"), ".dbus-keyrings"), Dir).

%%% The digest, against a hand-computed vector
%%%
%%%   sha1("1234abcd:cafebabe:deadbeef")
%%%     = 47bf1b6b2cd3704fe1aabfa6181ffeb230bd0b67

response_test() ->
    ?assertEqual(
        hex(<<"cafebabe 47bf1b6b2cd3704fe1aabfa6181ffeb230bd0b67">>),
        response(<<"1234abcd">>, <<"cafebabe">>, <<"deadbeef">>)
    ).

%% The cookie is the hex string of the file, used as text: decoding it
%% would give a different digest, and the server does not decode it either.
response_does_not_decode_its_inputs_test() ->
    Digest = hex(crypto:hash(sha, <<"1234abcd:cafebabe:deadbeef">>)),
    {ok, Reply} = unhex(response(<<"1234abcd">>, <<"cafebabe">>, <<"deadbeef">>)),
    ?assertEqual(<<"cafebabe ", Digest/binary>>, Reply).

%%% The full exchange

%% What the digest must be for the cookie of line 3, whatever challenge the
%% mechanism drew for itself.
assert_answers_with_cookie(Response, Cookie) ->
    {ok, Reply} = unhex(Response),
    [ClientChallenge, Digest] = binary:split(Reply, <<" ">>),
    Composite = <<"1234abcd:", ClientChallenge/binary, ":", Cookie/binary>>,
    ?assertEqual(hex(crypto:hash(sha, Composite)), Digest).

successful_challenge_test() ->
    with_keyring(fun(Dir) ->
        {ok, Response, State} = answer_challenge(ctx(Dir), server_data()),
        assert_answers_with_cookie(Response, <<"deadbeef">>),
        ?assertEqual(done, State#state.phase)
    end).

%% The digits a-f in lower case, and nothing else, in what we send.
response_is_lowercase_hex_test() ->
    with_keyring(fun(Dir) ->
        {ok, Response, _} = answer_challenge(ctx(Dir), server_data()),
        ?assert(
            lists:all(
                fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
                binary_to_list(Response)
            )
        )
    end).

%% The challenge is fresh on every exchange, so two runs against the same
%% cookie never produce the same response.
challenge_is_random_test() ->
    with_keyring(fun(Dir) ->
        {ok, First, _} = answer_challenge(ctx(Dir), server_data()),
        {ok, Second, _} = answer_challenge(ctx(Dir), server_data()),
        ?assertNotEqual(First, Second)
    end).

%% The mechanism answers one challenge; the state machine expects OK next.
second_challenge_test() ->
    with_keyring(fun(Dir) ->
        {ok, _, State} = answer_challenge(ctx(Dir), server_data()),
        ?assertEqual({error, unexpected_challenge}, challenge(server_data(), State))
    end).

%% The server may hex-encode in upper case even though it should not: what
%% it sends is decoded either way, only what we send is constrained.
uppercase_challenge_is_accepted_test() ->
    with_keyring(fun(Dir) ->
        Data = string:uppercase(server_data()),
        {ok, Response, _} = answer_challenge(ctx(Dir), Data),
        assert_answers_with_cookie(Response, <<"deadbeef">>)
    end).

%%% Malformed challenges

invalid_challenge_test() ->
    with_keyring(fun(Dir) ->
        Answer = fun(Data) -> answer_challenge(ctx(Dir), Data) end,
        %% not hex at all
        ?assertEqual({error, invalid_challenge}, Answer(<<"zzzz">>)),
        ?assertEqual({error, invalid_challenge}, Answer(<<"abc">>)),
        %% right shape, wrong number of fields
        ?assertEqual(
            {error, invalid_challenge},
            Answer(hex(<<"org_freedesktop_general 3">>))
        ),
        ?assertEqual(
            {error, invalid_challenge},
            Answer(hex(<<"org_freedesktop_general 3 1234abcd extra">>))
        ),
        ?assertEqual({error, invalid_challenge}, Answer(<<>>))
    end).

%% A context name is joined to a path, so the character set the
%% specification gives for it is a security boundary, not a formality.
invalid_context_test() ->
    with_keyring(fun(Dir) ->
        Answer = fun(Context) ->
            answer_challenge(ctx(Dir), hex(<<Context/binary, " 3 1234abcd">>))
        end,
        Rejected = fun(Context) -> {error, {invalid_cookie_context, Context}} end,
        ?assertEqual(Rejected(<<"..">>), Answer(<<"..">>)),
        ?assertEqual(Rejected(<<"../../etc/passwd">>), Answer(<<"../../etc/passwd">>)),
        ?assertEqual(Rejected(<<"/etc/passwd">>), Answer(<<"/etc/passwd">>)),
        ?assertEqual(Rejected(<<"a\\b">>), Answer(<<"a\\b">>)),
        ?assertEqual(Rejected(<<"a\tb">>), Answer(<<"a\tb">>)),
        ?assertEqual(
            Rejected(<<"caf", 16#C3, 16#A9>>),
            Answer(<<"caf", 16#C3, 16#A9>>)
        ),
        %% "nonzero length" -- a doubled space is not an anonymous context
        ?assertEqual(Rejected(<<>>), Answer(<<>>))
    end).

invalid_cookie_id_test() ->
    with_keyring(fun(Dir) ->
        Answer = fun(Id) ->
            Data = hex(<<"org_freedesktop_general ", Id/binary, " 1234abcd">>),
            answer_challenge(ctx(Dir), Data)
        end,
        ?assertEqual({error, {invalid_cookie_id, <<"-1">>}}, Answer(<<"-1">>)),
        ?assertEqual({error, {invalid_cookie_id, <<"3a">>}}, Answer(<<"3a">>))
    end).

%%% Locating the cookie

unknown_cookie_id_test() ->
    with_keyring(fun(Dir) ->
        Data = hex(<<"org_freedesktop_general 4 1234abcd">>),
        ?assertEqual({error, no_cookie}, answer_challenge(ctx(Dir), Data))
    end).

%% The ID is a whole field: "3" must not match the line of cookie 31.
cookie_id_is_not_a_prefix_test() ->
    with_keyring(<<"31 1234567890 aaaabbbb\n">>, 8#700, fun(Dir) ->
        ?assertEqual({error, no_cookie}, answer_challenge(ctx(Dir), server_data())),
        Data = hex(<<"org_freedesktop_general 31 1234abcd">>),
        {ok, Response, _} = answer_challenge(ctx(Dir), Data),
        assert_answers_with_cookie(Response, <<"aaaabbbb">>)
    end).

%% A line that is not three fields is skipped, not fatal: the file is
%% written by another process and may be mid-rewrite.
malformed_lines_are_skipped_test() ->
    Contents = <<"\ngarbage\n1 2\n3 1234567891 deadbeef\n">>,
    with_keyring(Contents, 8#700, fun(Dir) ->
        {ok, Response, _} = answer_challenge(ctx(Dir), server_data()),
        assert_answers_with_cookie(Response, <<"deadbeef">>)
    end).

%% Lines the server wrote with CRLF end at the cookie, not after it.
crlf_cookie_file_test() ->
    with_keyring(<<"3 1234567891 deadbeef\r\n">>, 8#700, fun(Dir) ->
        {ok, Response, _} = answer_challenge(ctx(Dir), server_data()),
        assert_answers_with_cookie(Response, <<"deadbeef">>)
    end).

missing_context_file_test() ->
    with_keyring(fun(Dir) ->
        Data = hex(<<"org_freedesktop_session_bus 3 1234abcd">>),
        ?assertEqual({error, {no_cookie_file, enoent}}, answer_challenge(ctx(Dir), Data))
    end).

missing_keyring_dir_test() ->
    Ctx = #{user => 1000, keyring_dir => "/nonexistent/.dbus-keyrings"},
    ?assertEqual({error, {no_keyring_dir, enoent}}, answer_challenge(Ctx, server_data())).

%% Without a home directory there is nowhere to look, and it is reported
%% from challenge/2 rather than aborting the conversation at init/1.
no_home_test() ->
    {ok, State} = init(#{user => 1000, keyring_dir => undefined}),
    ?assertEqual(undefined, State#state.keyring_dir),
    ?assertEqual({error, no_keyring_dir}, challenge(server_data(), State)).

%%% "This directory must not be readable or writable by other users. If it
%%% is, clients and servers must ignore it."

loose_keyring_dir_test() ->
    Answer = fun(Mode) ->
        with_keyring(cookie_file(), Mode, fun(Dir) ->
            answer_challenge(ctx(Dir), server_data())
        end)
    end,
    ?assertEqual({error, keyring_dir_not_private}, Answer(8#755)),
    ?assertEqual({error, keyring_dir_not_private}, Answer(8#770)),
    ?assertEqual({error, keyring_dir_not_private}, Answer(8#707)),
    %% stricter than required is fine
    ?assertMatch({ok, _, _}, Answer(8#700)),
    ?assertMatch({ok, _, _}, Answer(8#500)).

keyring_dir_is_a_file_test() ->
    with_keyring(fun(Dir) ->
        File = filename:join(Dir, ?CONTEXT),
        ?assertEqual(
            {error, keyring_dir_not_a_directory},
            answer_challenge(#{user => 1000, keyring_dir => File}, server_data())
        )
    end).

-endif.
