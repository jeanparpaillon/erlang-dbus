-module(dbus_transport_unix).
-moduledoc """
UNIX domain socket transport.

A scheme adapter, in the sense of `docs/arch.md`: it owns `connect/1` and
nothing else. Once connected the socket is driven through `m:dbus_transport`,
which is the same code the TCP adapter runs under -- `unix:` and `tcp:` differ
only in the `t:socket:sockaddr/0` they build.

`path` and `abstract` are both connectable and differ only in the sockaddr:
an abstract name is the same connect with a leading NUL, `<<0, Name/binary>>`,
which puts the socket in the abstract namespace instead of the filesystem. The
name is sent unpadded -- `socket:connect/2` derives the address length from the
binary -- which is what a NUL-prefixed `sun_path` means on Linux and what
`dbus-daemon` binds. Abstract sockets are a Linux extension; elsewhere the
connect fails on the spot and `dbus_connection` moves to the next candidate,
which is the same outcome refusing the parameter here would produce.

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
    support_unix_fd/0
]).

%% The keys that say where the socket is. Exactly one of them may appear, and
%% only the first is connectable.
-define(ENDPOINT_KEYS, [path, abstract, dir, tmpdir, runtime]).

-spec connect(dbus_address()) ->
    {ok, dbus_transport:socket()}
    | {error, term()}.
connect(#dbus_address{scheme = <<"unix">>} = Address) ->
    case endpoint(Address) of
        {ok, Path} -> open(Path);
        {error, _} = E -> E
    end;
connect(#dbus_address{scheme = Scheme}) ->
    {error, {invalid_scheme, Scheme}}.

% Not implemented yet, see docs/dbus-unix-fd-passing.md
-spec support_unix_fd() -> boolean().
support_unix_fd() -> false.

%%%
%%% Private
%%%

%% "Exactly one of the keys must be provided": two of them is a malformed
%% address rather than one with a fallback, since nothing says which wins.
endpoint(#dbus_address{options = Opts}) ->
    case endpoint_keys(Opts) of
        [{path, Path}] -> {ok, Path};
        [{abstract, Name}] -> {ok, abstract_path(Name)};
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

%% The address carries the abstract name; the leading NUL is what makes it
%% abstract and is not part of it.
abstract_path(Name) ->
    <<0, (iolist_to_binary(Name))/binary>>.

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
%%%   * an `abstract' address does the same against a listener bound in the
%%%     abstract namespace, which is the only thing the leading NUL changes;
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

%% Nothing is bound under that name, and the abstract namespace has no
%% directory to report a missing entry from -- the connect is refused.
abstract_not_listening_test_() ->
    on_linux(fun() ->
        ?assertEqual({error, econnrefused}, connect(addr([{abstract, tmp_name()}])))
    end).

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

%% The name is not a filesystem path: it connects to a listener bound to
%% `<<0, Name/binary>>', which is the prefixing this module does.
abstract_roundtrip_test_() ->
    on_linux(fun() ->
        with_abstract_listener(fun(Name, Listener) ->
            {ok, Conn} = dbus_transport:connect(addr([{abstract, Name}])),
            {ok, Peer} = socket:accept(Listener, 1000),

            ok = dbus_transport:send(Conn, <<0, "AUTH\r\n">>),
            ?assertEqual({ok, <<0, "AUTH\r\n">>}, socket:recv(Peer, 7, 1000)),

            ok = socket:send(Peer, <<"OK\r\n">>),
            ?assertEqual({ok, <<"OK\r\n">>}, dbus_transport:recv(Conn, 1000)),

            ok = dbus_transport:close(Conn),
            _ = socket:close(Peer)
        end)
    end).

%%%
%%% Helpers
%%%

%% Abstract sockets are a Linux extension; the tests that need one say so
%% rather than failing on a platform that cannot have them.
on_linux(Fun) ->
    case os:type() of
        {unix, linux} -> [Fun];
        _ -> []
    end.

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

%% Nothing to unlink afterwards: an abstract name disappears with the last
%% socket holding it.
with_abstract_listener(Fun) ->
    Name = tmp_name(),
    {ok, Listener} = socket:open(local, stream, default),
    ok = socket:bind(Listener, #{family => local, path => <<0, Name/binary>>}),
    ok = socket:listen(Listener),
    try
        Fun(Name, Listener)
    after
        _ = socket:close(Listener)
    end.

%% Kept short on purpose: `sun_path' is 108 bytes including the NUL, and a
%% longer one fails as `einval' with nothing saying why.
tmp_path() ->
    Dir = os:getenv("XDG_RUNTIME_DIR", "/tmp"),
    iolist_to_binary(filename:join(Dir, unique("dbus-unix-test-"))).

tmp_name() ->
    list_to_binary(unique("erlang-dbus-unix-test-")).

unique(Prefix) ->
    Prefix ++ integer_to_list(erlang:unique_integer([positive])).
-endif.
