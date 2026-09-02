-module(dbus_transport_unix).
-moduledoc """
UNIX domain socket transport.

A scheme adapter, in the sense of `docs/arch.md`: it owns `connect/1` and
nothing else. Once connected the socket is driven through `m:dbus_transport`,
which is the same code the TCP adapter runs under -- `unix:` and `tcp:` differ
only in the `t:socket:sockaddr/0` they build.

Only `path` is connected so far. `abstract` is the same connect with a leading
NUL in the path and is refused here rather than half-done, so that
`dbus_connection` moves on to the next candidate address instead of getting a
socket that talks to nothing.

`dir`, `tmpdir` and `runtime` tell a *server* which socket to create and which
address to publish afterwards; there is nothing for a client to connect to, so
they are `not_connectable` -- the same answer the TCP adapter gives a listen
address, and the one that makes `dbus_connection` try the next alternative.

Unlike `m:dbus_transport_tcp` this module needs no connect timeout: an
`AF_UNIX` connect either finds a listener with room in its backlog or fails on
the spot, so there is no equivalent of a SYN going unanswered.

Passing file descriptors is possible here and only here, so `support_unix_fd/1`
starts out `true`. The flag it returns has to survive `disable_unix_fd/1` and
be readable from the reader process, which is not the process that
authenticated, so it lives in the socket's own `{otp, meta}` slot rather than
in a caller's state or a process dictionary. This module owns that slot for
sockets it opened. Writing it requires being the socket's controlling process,
which the connecting process is -- and it is also the one that runs
authentication, the only caller of `disable_unix_fd/1`.
""".
-behaviour(dbus_transport).

-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    connect/1,
    support_unix_fd/1,
    disable_unix_fd/1
]).

%% The keys that say where the socket is. Exactly one of them may appear, and
%% only the first is connectable.
-define(ENDPOINT_KEYS, [path, abstract, dir, tmpdir, runtime]).

-spec connect(dbus_address()) ->
    {ok, dbus_transport:connection()}
    | {error, term()}.
connect(#dbus_address{scheme = <<"unix">>} = Address) ->
    case endpoint(Address) of
        {ok, Path} -> open(Path);
        {error, _} = E -> E
    end;
connect(#dbus_address{scheme = Scheme}) ->
    {error, {invalid_scheme, Scheme}}.

-spec support_unix_fd(dbus_transport:connection()) -> boolean().
support_unix_fd({_, Sock}) ->
    case socket:getopt(Sock, {otp, meta}) of
        {ok, #{unix_fd := Supported}} -> Supported;
        %% A socket this module did not open, or one already closed: claiming
        %% fd passing we cannot back up is the worse failure of the two.
        _ -> false
    end.

-spec disable_unix_fd(dbus_transport:connection()) -> ok.
disable_unix_fd({_, Sock}) ->
    case socket:setopt(Sock, {otp, meta}, #{unix_fd => false}) of
        ok ->
            ok;
        {error, Reason} ->
            %% The callback has no way to report this, and the caller -- the
            %% auth conversation -- has already decided to carry on without
            %% fd passing.
            ?LOG_WARNING("could not record unix fd refusal: ~p", [Reason]),
            ok
    end.

%%%
%%% Private
%%%

%% "Exactly one of the keys must be provided": two of them is a malformed
%% address rather than one with a fallback, since nothing says which wins.
endpoint(#dbus_address{options = Opts}) ->
    case endpoint_keys(Opts) of
        [{path, Path}] -> {ok, Path};
        [{abstract, _}] -> {error, {unsupported_parameter, abstract}};
        [{Key, _}] -> listen_only(Key);
        [] -> {error, not_connectable};
        Several -> {error, {conflicting_parameters, [Key || {Key, _} <- Several]}}
    end.

endpoint_keys(Opts) ->
    lists:filtermap(
        fun(Key) ->
            case proplists:get_value(Key, Opts) of
                undefined -> false;
                Value -> {true, {Key, Value}}
            end
        end,
        ?ENDPOINT_KEYS
    ).

%% Named rather than folded into the clause above so that the address the user
%% wrote is not silently equated with an empty one.
listen_only(Key) ->
    ?LOG_DEBUG("ignoring listen-only unix address parameter ~ts", [Key]),
    {error, not_connectable}.

open(Path) ->
    case socket:open(local, stream, default) of
        {ok, Sock} -> connect_socket(Sock, Path);
        {error, _} = E -> E
    end.

connect_socket(Sock, Path) ->
    case socket:connect(Sock, #{family => local, path => Path}) of
        ok ->
            ok = socket:setopt(Sock, {otp, meta}, #{unix_fd => true}),
            {ok, Sock};
        {error, Reason} ->
            %% A socket that failed to connect cannot be retried.
            _ = socket:close(Sock),
            {error, Reason}
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

%%% The contract these tests pin:
%%%
%%%   * an address with nothing to connect to -- no endpoint key, or a
%%%     listen-only one -- is refused as `not_connectable' before any socket
%%%     is opened, so the caller can try the next alternative;
%%%   * a `path' address connects to a listening socket and the resulting
%%%     connection carries bytes both ways through `m:dbus_transport',
%%%     including from a process other than the one that connected -- the
%%%     reader in `dbus_connection' is not the connection process;
%%%   * fd passing is supported until the peer refuses it, and the refusal is
%%%     remembered on the connection, not on the caller.

addr(Options) ->
    #dbus_address{scheme = <<"unix">>, options = Options}.

%%%
%%% Address validation -- no server involved
%%%

no_options_test() ->
    ?assertEqual({error, not_connectable}, connect(addr([]))).

%% Listen-only: these tell a server what to create, not a client where to go.
listen_only_test_() ->
    [
        ?_assertEqual({error, not_connectable}, connect(addr([{Key, Value}])))
     || {Key, Value} <- [
            {dir, <<"/some/directory">>},
            {tmpdir, <<"/tmp">>},
            {runtime, <<"yes">>}
        ]
    ].

abstract_is_not_implemented_test() ->
    ?assertEqual(
        {error, {unsupported_parameter, abstract}},
        connect(addr([{abstract, <<"/tmp/dbus-XYZ">>}]))
    ).

conflicting_parameters_test() ->
    ?assertEqual(
        {error, {conflicting_parameters, [path, abstract]}},
        connect(addr([{path, <<"/run/user/1000/bus">>}, {abstract, <<"/tmp/dbus-XYZ">>}]))
    ).

%% Another scheme's options happen to parse; the module still refuses them.
wrong_scheme_test() ->
    Address = #dbus_address{
        scheme = <<"unixexec">>,
        options = [{path, <<"/usr/bin/example">>}]
    },
    ?assertEqual({error, {invalid_scheme, <<"unixexec">>}}, connect(Address)).

%% A well-formed address is still allowed to fail, and says how.
no_such_path_test() ->
    ?assertEqual({error, enoent}, connect(addr([{path, tmp_path()}]))).

%%%
%%% Against a listening socket
%%%

roundtrip_test() ->
    with_listener(fun(Path, Listener) ->
        {ok, Conn} = dbus_transport:connect(addr([{path, Path}])),
        {ok, Peer} = socket:accept(Listener, 1000),

        ok = dbus_transport:send(Conn, <<0, "AUTH\r\n">>),
        ?assertEqual({ok, <<0, "AUTH\r\n">>}, socket:recv(Peer, 7, 1000)),

        ok = socket:send(Peer, <<"OK\r\n">>),
        ?assertEqual({ok, <<"OK\r\n">>}, dbus_transport:recv(Conn, 1000)),

        ?assertEqual(ok, dbus_transport:close(Conn)),
        _ = socket:close(Peer)
    end).

%% `dbus_connection' hands the connection to a reader process it spawns, so a
%% connection that only works in the process that opened it is broken.
recv_from_another_process_test() ->
    with_listener(fun(Path, Listener) ->
        {ok, Conn} = dbus_transport:connect(addr([{path, Path}])),
        {ok, Peer} = socket:accept(Listener, 1000),
        Self = self(),
        _ = spawn_link(fun() -> Self ! {recvd, dbus_transport:recv(Conn, 1000)} end),

        ok = socket:send(Peer, <<"DATA">>),
        receive
            {recvd, Result} -> ?assertEqual({ok, <<"DATA">>}, Result)
        after 2000 -> ?assert(false)
        end,

        ok = dbus_transport:close(Conn),
        _ = socket:close(Peer)
    end).

unix_fd_test() ->
    with_listener(fun(Path, Listener) ->
        {ok, Conn} = dbus_transport:connect(addr([{path, Path}])),
        {ok, Peer} = socket:accept(Listener, 1000),
        ?assert(dbus_transport:support_unix_fd(Conn)),

        %% The refusal lives on the connection, so it is still visible
        %% through a handle rebuilt from it, and to another process.
        ok = dbus_transport:disable_unix_fd(Conn),
        ?assertNot(dbus_transport:support_unix_fd(Conn)),
        Self = self(),
        _ = spawn_link(fun() -> Self ! {fd, dbus_transport:support_unix_fd(Conn)} end),
        receive
            {fd, Supported} -> ?assertNot(Supported)
        after 2000 -> ?assert(false)
        end,

        ok = dbus_transport:close(Conn),
        _ = socket:close(Peer)
    end).

%% A closed connection cannot pass descriptors either.
support_unix_fd_on_closed_socket_test() ->
    with_listener(fun(Path, _Listener) ->
        {ok, Conn} = dbus_transport:connect(addr([{path, Path}])),
        ok = dbus_transport:close(Conn),
        ?assertNot(dbus_transport:support_unix_fd(Conn))
    end).

%%%
%%% Helpers
%%%

with_listener(Fun) ->
    Path = tmp_path(),
    {ok, Listener} = socket:open(local, stream, default),
    ok = socket:bind(Listener, #{family => local, path => Path}),
    ok = socket:listen(Listener),
    try
        Fun(Path, Listener)
    after
        _ = socket:close(Listener),
        %% bind/2 creates the filesystem entry; nothing removes it on close.
        _ = file:delete(Path)
    end.

%% Kept short on purpose: `sun_path' is 108 bytes including the NUL, and a
%% longer one fails as `einval' with nothing saying why.
tmp_path() ->
    Dir = os:getenv("XDG_RUNTIME_DIR", "/tmp"),
    Unique = integer_to_list(erlang:unique_integer([positive])),
    iolist_to_binary(filename:join(Dir, "dbus-unix-test-" ++ Unique)).
-endif.
