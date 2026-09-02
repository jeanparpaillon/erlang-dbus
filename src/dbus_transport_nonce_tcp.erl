-module(dbus_transport_nonce_tcp).
-moduledoc """
Nonce-authenticated TCP transport.

A scheme adapter, in the sense of `docs/arch.md`: it owns `connect/1` and
nothing else. `nonce-tcp:` is `tcp:` plus one write, so the address rules and
the connect itself are `m:dbus_transport_tcp`'s -- `endpoint/1` then
`connect/3` -- and what is left here is the nonce. Re-deriving `host`, `port`
and `family` would be two implementations of the same paragraph of the spec,
free to drift; the only thing this scheme adds is `noncefile`.

The server writes 16 random bytes to a file only the connecting user can read,
and names it in the address; the client proves it can read that file by sending
the bytes back **before anything else on the connection** -- before the NUL
byte that opens authentication. That is the whole mechanism: it substitutes for
the peer credentials an `AF_UNIX` socket would have carried, on a transport
that has none. So the write happens inside `connect/1`, not in the auth
conversation: by the time `dbus_connection` sends its NUL the nonce is already
on the wire, and no caller can get the order wrong.

Exactly 16 bytes are read and sent, and a shorter file is an error rather than
a short nonce -- `libdbus` reads a fixed 16 and fails if the read is short, and
a server comparing against its own 16 would close the connection anyway, but
several bytes later and with nothing saying why. A longer file is read to 16
for the same reason: the extra bytes are not part of the nonce and sending them
would corrupt the first line of the auth conversation.

The file is read *before* the socket is opened. It is the one failure this
scheme can have that says nothing about the peer -- a missing or unreadable
noncefile is a local problem -- so paying for a connect first, and then closing
it unauthenticated, buys nothing. The nonce is per-listening-socket, not
per-connection, so nothing goes stale in between.

A missing `noncefile` is `not_connectable`, the same answer
`m:dbus_transport_tcp` gives a listen address: `nonce-tcp:` without one cannot
be dialled, and reporting it that way lets `dbus_connection` move to the next
alternative instead of failing the whole address list.

The transport underneath is TCP, which cannot carry file descriptors, so
`support_unix_fd/1` is `false` here for the same reason it is there.
""".
-behaviour(dbus_transport).

-include("dbus.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    connect/1,
    support_unix_fd/0
]).

%% "The nonce is a 16 byte random value": not a length the file declares.
-define(NONCE_SIZE, 16).

-spec connect(dbus_address()) ->
    {ok, dbus_transport:socket()}
    | {error, term()}.
connect(#dbus_address{scheme = <<"nonce-tcp">>} = Address) ->
    case dbus_transport_tcp:endpoint(Address) of
        {ok, Endpoint} -> connect_nonce(Endpoint, Address);
        {error, _} = E -> E
    end;
connect(#dbus_address{scheme = Scheme}) ->
    {error, {invalid_scheme, Scheme}}.

-spec support_unix_fd() -> boolean().
support_unix_fd() -> false.

%%%
%%% Private
%%%

connect_nonce(Endpoint, #dbus_address{options = Opts}) ->
    case nonce(proplists:get_value(noncefile, Opts)) of
        {ok, Nonce} -> connect_endpoint(Endpoint, Nonce);
        {error, _} = E -> E
    end.

connect_endpoint({Host, Port, FamilyOpts}, Nonce) ->
    case dbus_transport_tcp:connect(Host, Port, FamilyOpts) of
        {ok, Sock} -> send_nonce(Sock, Nonce);
        {error, _} = E -> E
    end.

%% Straight to the socket rather than through `dbus_transport:send/2': there is
%% no connection handle yet, and building one here would name a module the
%% caller has not been handed the connection under.
send_nonce(Sock, Nonce) ->
    case socket:send(Sock, Nonce) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            %% Nothing can be done with a connection the server is about to
            %% drop for having sent no nonce.
            _ = socket:close(Sock),
            {error, Reason}
    end.

nonce(undefined) ->
    {error, not_connectable};
nonce(Path) ->
    %% `raw' -- the file is not shared with another process and its contents
    %% are bytes, not characters.
    case file:open(Path, [read, raw, binary]) of
        {ok, Fd} ->
            try
                read_nonce(Fd)
            after
                _ = file:close(Fd)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

read_nonce(Fd) ->
    case file:read(Fd, ?NONCE_SIZE) of
        {ok, <<Nonce:?NONCE_SIZE/binary>>} -> {ok, Nonce};
        {ok, Short} -> {error, {short_noncefile, byte_size(Short)}};
        eof -> {error, {short_noncefile, 0}};
        {error, _} = E -> E
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

%%% The contract these tests pin:
%%%
%%%   * the `tcp' address rules apply unchanged -- an address with no host or
%%%     no port is `not_connectable' here too -- and `noncefile' is one more
%%%     of them, since without it there is nothing to send;
%%%   * a noncefile that cannot be read fails *before* a socket is opened, so
%%%     a local misconfiguration never shows up as a connection the server has
%%%     to drop;
%%%   * the nonce is exactly the file's first 16 bytes, and it is the first
%%%     thing on the connection -- ahead of the NUL byte `dbus_connection'
%%%     sends -- since a server reads it before anything else;
%%%   * a file too short to hold a nonce is refused rather than sent as-is;
%%%   * what comes back is an ordinary connection, carrying bytes both ways
%%%     through `m:dbus_transport'.

addr(Options) ->
    #dbus_address{scheme = <<"nonce-tcp">>, options = Options}.

%%%
%%% Address validation -- no server involved
%%%

missing_noncefile_test() ->
    ?assertEqual(
        {error, not_connectable},
        connect(addr([{host, <<"127.0.0.1">>}, {port, <<"12345">>}]))
    ).

%% The `tcp' rules are not re-implemented here, so they still hold.
missing_host_test() ->
    ?assertEqual(
        {error, not_connectable},
        connect(addr([{port, <<"12345">>}, {noncefile, <<"/nonexistent">>}]))
    ).

missing_port_test() ->
    ?assertEqual(
        {error, not_connectable},
        connect(addr([{host, <<"127.0.0.1">>}, {noncefile, <<"/nonexistent">>}]))
    ).

invalid_port_test() ->
    ?assertEqual(
        {error, {invalid_port, <<"garbage">>}},
        connect(
            addr([
                {host, <<"127.0.0.1">>}, {port, <<"garbage">>}, {noncefile, <<"/nonexistent">>}
            ])
        )
    ).

%% Another scheme's options happen to parse; the module still refuses them.
wrong_scheme_test() ->
    Address = #dbus_address{
        scheme = <<"tcp">>,
        options = [{host, <<"127.0.0.1">>}, {port, <<"12345">>}]
    },
    ?assertEqual({error, {invalid_scheme, <<"tcp">>}}, connect(Address)).

%%%
%%% The noncefile itself
%%%

%% A listener is up and the address is good, so a connect would have
%% succeeded: the `enoent' -- and the listener seeing nothing -- is what says
%% the file is read first.
no_such_noncefile_test() ->
    with_listener(fun(Port, Listener) ->
        ?assertEqual({error, enoent}, connect(addr(options(Port, tmp_path())))),
        ?assertEqual({error, timeout}, socket:accept(Listener, 200))
    end).

short_noncefile_test_() ->
    [
        {integer_to_list(byte_size(Content)) ++ " bytes", fun() ->
            with_noncefile(Content, fun(NoncePath) ->
                with_listener(fun(Port, _Listener) ->
                    ?assertEqual(
                        {error, {short_noncefile, byte_size(Content)}},
                        connect(addr(options(Port, NoncePath)))
                    )
                end)
            end)
        end}
     || Content <- [<<>>, <<"short">>, binary:copy(<<"n">>, ?NONCE_SIZE - 1)]
    ].

%% The nonce is 16 bytes wherever the file stops being one: the rest is not
%% part of it and must not reach the server, which reads the next bytes as the
%% start of the auth conversation.
long_noncefile_is_truncated_test() ->
    Nonce = nonce_bytes(),
    with_noncefile(<<Nonce/binary, "trailing junk">>, fun(NoncePath) ->
        with_listener(fun(Port, Listener) ->
            {ok, Conn} = dbus_transport:connect(addr(options(Port, NoncePath))),
            {ok, Peer} = socket:accept(Listener, 1000),
            ?assertEqual({ok, Nonce}, socket:recv(Peer, ?NONCE_SIZE, 1000)),

            %% Nothing followed it on the wire.
            ok = dbus_transport:send(Conn, <<0>>),
            ?assertEqual({ok, <<0>>}, socket:recv(Peer, 1, 1000)),

            ok = dbus_transport:close(Conn),
            _ = socket:close(Peer)
        end)
    end).

%%%
%%% Against a listening socket
%%%

%% The nonce comes first, then the connection behaves like any other.
roundtrip_test() ->
    Nonce = nonce_bytes(),
    with_noncefile(Nonce, fun(NoncePath) ->
        with_listener(fun(Port, Listener) ->
            {ok, Conn} = dbus_transport:connect(addr(options(Port, NoncePath))),
            {ok, Peer} = socket:accept(Listener, 1000),

            %% Ahead of the NUL byte that opens authentication.
            ?assertEqual({ok, Nonce}, socket:recv(Peer, ?NONCE_SIZE, 1000)),

            ok = dbus_transport:send(Conn, <<0, "AUTH\r\n">>),
            ?assertEqual({ok, <<0, "AUTH\r\n">>}, socket:recv(Peer, 7, 1000)),

            ok = socket:send(Peer, <<"OK\r\n">>),
            ?assertEqual({ok, <<"OK\r\n">>}, dbus_transport:recv(Conn, 1000)),

            ?assertEqual(ok, dbus_transport:close(Conn)),
            _ = socket:close(Peer)
        end)
    end).

%% TCP underneath, so no descriptors -- and refusing them is not an error.
unix_fd_test() ->
    with_noncefile(nonce_bytes(), fun(NoncePath) ->
        with_listener(fun(Port, Listener) ->
            {ok, Conn} = dbus_transport:connect(addr(options(Port, NoncePath))),
            {ok, Peer} = socket:accept(Listener, 1000),
            ?assertNot(dbus_transport:support_unix_fd(Conn)),
            ?assertEqual(ok, dbus_transport:disable_unix_fd(Conn)),

            ok = dbus_transport:close(Conn),
            _ = socket:close(Peer)
        end)
    end).

%%%
%%% Helpers
%%%

options(Port, NoncePath) ->
    [
        {host, <<"127.0.0.1">>},
        {port, integer_to_binary(Port)},
        {noncefile, NoncePath}
    ].

nonce_bytes() ->
    crypto:strong_rand_bytes(?NONCE_SIZE).

with_noncefile(Content, Fun) ->
    Path = tmp_path(),
    ok = file:write_file(Path, Content),
    try
        Fun(Path)
    after
        _ = file:delete(Path)
    end.

%% Port 0, then ask the kernel which port it picked: a hardcoded one collides
%% with whatever else runs on the build machine.
with_listener(Fun) ->
    {ok, Listener} = socket:open(inet, stream, tcp),
    ok = socket:bind(Listener, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(Listener),
    {ok, #{port := Port}} = socket:sockname(Listener),
    try
        Fun(Port, Listener)
    after
        _ = socket:close(Listener)
    end.

tmp_path() ->
    Dir = os:getenv("XDG_RUNTIME_DIR", "/tmp"),
    Name = "dbus-nonce-test-" ++ integer_to_list(erlang:unique_integer([positive])),
    iolist_to_binary(filename:join(Dir, Name)).
-endif.
