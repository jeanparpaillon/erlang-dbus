-module(dbus_transport_tcp).
-moduledoc """
TCP transport.

The transport owns no process: `connect/1` returns the socket itself and every
callback is a direct call on it. Reads are passive (`{active, false}`) because
the `dbus_transport` behaviour asks for it -- `recv/2` is synchronous and hands
back no new state -- and because it lets `dbus_connection` run the
authentication handshake inline, only then handing the socket to its reader
process. A single passive socket allows one `recv` at a time, so the reader may
not be started before authentication has finished.

Framing is delegated to the socket rather than buffered here, for the same
reason: `recv/2` has nowhere to keep a partial line. During authentication the
socket runs in `{packet, line}`, so each `recv/2` returns exactly the one
CRLF-terminated command `dbus_sasl:parse/1` accepts; `set_mode(Conn, raw)`
switches to `{packet, raw}` for the message stream. Bytes already buffered by
the driver survive that switch, so a peer that sends its first message in the
same segment as `OK` loses nothing.

TCP cannot carry file descriptors at all, so `support_unix_fd/1` is `false` and
`NEGOTIATE_UNIX_FD` is never sent.
""".
-behaviour(dbus_transport).

-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    connect/1,
    connect/3,
    endpoint/1,
    send/2,
    recv/2,
    close/1,
    support_unix_fd/1,
    disable_unix_fd/1,
    set_mode/2
]).

-export_type([connection/0]).

-type connection() :: gen_tcp:socket().
-type family_opts() :: [inet | inet6].

%% Ceiling on the length of an authentication line, matching libdbus. Bounding
%% it at the socket means a peer that never sends a newline cannot grow the
%% driver's buffer without limit: `{packet, line}' truncates an endless line --
%% in practice at the receive buffer, which is smaller than this -- and
%% delivers it unterminated, which `dbus_sasl:parse/1' then rejects.
-define(MAX_LINE, 16384).

%% `connect/1' takes no timeout and runs inside `dbus_connection:init/1', so
%% without one an unreachable host blocks the caller of `start_link/1' for the
%% kernel's full SYN retry period.
-define(CONNECT_TIMEOUT, 5000).

-define(SOCKET_OPTS, [
    binary,
    {active, false},
    {packet, line},
    {packet_size, ?MAX_LINE},
    {keepalive, true}
]).

-spec connect(dbus_address()) ->
    {ok, connection()}
    | {error, term()}.
connect(#dbus_address{scheme = <<"tcp">>} = Address) ->
    case endpoint(Address) of
        {ok, {Host, Port, FamilyOpts}} -> connect(Host, Port, FamilyOpts);
        {error, _} = E -> E
    end;
connect(#dbus_address{scheme = Scheme}) ->
    {error, {invalid_scheme, Scheme}}.

-doc """
Validate a `tcp' address and return what `connect/3` needs.

Exported so `nonce-tcp` -- the same connect plus a nonce write -- can reuse the
address rules instead of copying them.
""".
-spec endpoint(dbus_address()) ->
    {ok, {inet:hostname(), inet:port_number(), family_opts()}}
    | {error, not_connectable | term()}.
endpoint(#dbus_address{options = Opts}) ->
    ok = log_bind(Opts),
    case host(proplists:get_value(host, Opts)) of
        {ok, Host} -> endpoint_port(Host, Opts);
        {error, _} = E -> E
    end.

-doc """
Connect to an already validated endpoint.
""".
-spec connect(inet:hostname(), inet:port_number(), family_opts()) ->
    {ok, connection()}
    | {error, term()}.
connect(Host, Port, FamilyOpts) ->
    gen_tcp:connect(Host, Port, ?SOCKET_OPTS ++ FamilyOpts, ?CONNECT_TIMEOUT).

-spec send(connection(), iodata()) ->
    ok
    | {error, term()}.
send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

%% Length must be 0 under every packet mode: in `line' mode the socket decides
%% where the packet ends, in `raw' mode 0 means "whatever has arrived".
-spec recv(connection(), timeout()) ->
    {ok, binary()}
    | {error, closed | timeout | term()}.
recv(Sock, Timeout) ->
    gen_tcp:recv(Sock, 0, Timeout).

-spec close(connection()) -> ok.
close(Sock) ->
    gen_tcp:close(Sock).

-spec support_unix_fd(connection()) -> boolean().
support_unix_fd(_Sock) ->
    false.

-spec disable_unix_fd(connection()) -> ok.
disable_unix_fd(_Sock) ->
    ok.

-spec set_mode(connection(), raw | line) -> ok.
set_mode(Sock, Mode) ->
    case inet:setopts(Sock, mode_opts(Mode)) of
        ok ->
            ok;
        {error, Reason} ->
            %% The only way this fails is a socket that is already gone, and
            %% the next `recv/2' reports that properly. Crashing the caller in
            %% the middle of the handshake would say less.
            ?LOG_DEBUG("switching to ~s mode failed: ~p", [Mode, Reason]),
            ok
    end.

%%%
%%% Private
%%%
mode_opts(line) -> [{packet, line}, {packet_size, ?MAX_LINE}];
mode_opts(raw) -> [{packet, raw}].

endpoint_port(Host, Opts) ->
    case port(proplists:get_value(port, Opts)) of
        {ok, Port} -> endpoint_family(Host, Port, Opts);
        {error, _} = E -> E
    end.

endpoint_family(Host, Port, Opts) ->
    case family(proplists:get_value(family, Opts)) of
        {ok, FamilyOpts} -> {ok, {Host, Port, infer_family(Host, FamilyOpts)}};
        {error, _} = E -> E
    end.

%% "Addresses in which host is specified, and port is non-zero, are
%% connectable" -- anything else is a listen address, which is a normal thing
%% to find in a config. Reporting it lets dbus_connection move to the next
%% alternative rather than fail the whole address list.
host(undefined) -> {error, not_connectable};
host(Host) -> {ok, binary_to_list(Host)}.

port(undefined) ->
    {error, not_connectable};
port(Bin) ->
    try binary_to_integer(Bin) of
        0 -> {error, not_connectable};
        Port when Port > 0, Port =< 65535 -> {ok, Port};
        Port -> {error, {invalid_port, Port}}
    catch
        error:badarg -> {error, {invalid_port, Bin}}
    end.

%% "family: ipv4 or ipv6"; unset means unspecified, not "either is fine to
%% invent", so it stays out of the option list.
family(undefined) -> {ok, []};
family(<<"ipv4">>) -> {ok, [inet]};
family(<<"ipv6">>) -> {ok, [inet6]};
family(Family) -> {error, {invalid_family, Family}}.

%% An IPv6 literal is unusable under the default `inet' family: `::1' with no
%% `family=ipv6' fails to resolve rather than connecting. Since the literal
%% says which family it is, use it -- an explicit `family' still wins.
infer_family(Host, []) ->
    case inet:parse_address(Host) of
        {ok, {_, _, _, _}} -> [inet];
        {ok, _} -> [inet6];
        {error, einval} -> []
    end;
infer_family(_Host, FamilyOpts) ->
    FamilyOpts.

%% `bind' names the address a *server* binds to and has no meaning for a
%% connect. Dropping it silently would make a mistyped address look like it
%% worked.
log_bind(Opts) ->
    case proplists:get_value(bind, Opts) of
        undefined ->
            ok;
        Bind ->
            ?LOG_DEBUG("ignoring listen-only parameter bind=~ts on connect", [Bind]),
            ok
    end.

%%%
%%% eunit
%%%
-ifdef(TEST).

%%% The contract these tests pin:
%%%
%%%   * an address that cannot be connected to -- no host, no port, port 0 --
%%%     is refused as `not_connectable' before any socket is opened, so the
%%%     caller can try the next alternative;
%%%   * `family' maps to the `inet'/`inet6' socket option, and an unknown
%%%     value is an error rather than a silent default;
%%%   * in `line' mode each `recv/2' returns exactly one CRLF-terminated
%%%     line -- never two, never half of one -- which is the framing
%%%     `dbus_sasl:parse/1' requires, and a line that never ends is cut off
%%%     rather than buffered without limit;
%%%   * `set_mode(Conn, raw)' switches framing without losing bytes the
%%%     driver already holds;
%%%   * a `recv/2' timeout leaves the connection usable, and the socket is
%%%     readable from a process other than the one that opened it -- the
%%%     reader in `dbus_connection' is not the connection process.

addr(Options) ->
    #dbus_address{scheme = <<"tcp">>, options = Options}.

%%%
%%% Address validation -- no server involved
%%%

missing_host_test() ->
    ?assertEqual({error, not_connectable}, connect(addr([{port, <<"12345">>}]))).

missing_port_test() ->
    ?assertEqual({error, not_connectable}, connect(addr([{host, <<"127.0.0.1">>}]))).

port_zero_test() ->
    ?assertEqual(
        {error, not_connectable},
        connect(addr([{host, <<"127.0.0.1">>}, {port, <<"0">>}]))
    ).

no_options_test() ->
    ?assertEqual({error, not_connectable}, connect(addr([]))).

invalid_port_test_() ->
    [
        ?_assertMatch({error, {invalid_port, _}}, endpoint(addr(with_host([{port, P}]))))
     || P <- [<<"garbage">>, <<"12x">>, <<"-1">>, <<"70000">>, <<>>]
    ].

invalid_family_test() ->
    ?assertEqual(
        {error, {invalid_family, <<"ipx">>}},
        connect(addr(with_host([{port, <<"12345">>}, {family, <<"ipx">>}])))
    ).

%% Another scheme's options happen to parse; the module still refuses them.
wrong_scheme_test() ->
    Address = #dbus_address{
        scheme = <<"nonce-tcp">>,
        options = [{host, <<"127.0.0.1">>}, {port, <<"12345">>}]
    },
    ?assertMatch({error, {invalid_scheme, <<"nonce-tcp">>}}, connect(Address)).

family_test_() ->
    [
        ?_assertEqual(
            {ok, {"example.test", 12345, [inet]}},
            endpoint(addr([{host, <<"example.test">>}, {port, <<"12345">>}, {family, <<"ipv4">>}]))
        ),
        ?_assertEqual(
            {ok, {"example.test", 12345, [inet6]}},
            endpoint(addr([{host, <<"example.test">>}, {port, <<"12345">>}, {family, <<"ipv6">>}]))
        ),
        %% unset stays unset for a name...
        ?_assertEqual(
            {ok, {"example.test", 12345, []}},
            endpoint(addr([{host, <<"example.test">>}, {port, <<"12345">>}]))
        ),
        %% ... but a literal says which family it is. The `:' is escaped in a
        %% real address string; dbus_address hands over the decoded value.
        ?_assertEqual(
            {ok, {"::1", 12345, [inet6]}},
            endpoint(addr([{host, <<"::1">>}, {port, <<"12345">>}]))
        ),
        ?_assertEqual(
            {ok, {"127.0.0.1", 12345, [inet]}},
            endpoint(addr([{host, <<"127.0.0.1">>}, {port, <<"12345">>}]))
        )
    ].

%% Listen-only, accepted and ignored.
bind_is_ignored_test() ->
    ?assertEqual(
        {ok, {"127.0.0.1", 12345, [inet]}},
        endpoint(addr([{host, <<"127.0.0.1">>}, {port, <<"12345">>}, {bind, <<"0.0.0.0">>}]))
    ).

with_host(Options) ->
    [{host, <<"127.0.0.1">>} | Options].

%%%
%%% Against a local server
%%%

%% A connected pair: the transport's socket, and the raw socket of a fake
%% peer to drive it with.
pair() ->
    {ok, LSock} = gen_tcp:listen(0, [
        binary, {active, false}, {packet, raw}, {ip, {127, 0, 0, 1}}, {reuseaddr, true}
    ]),
    {ok, Port} = inet:port(LSock),
    {ok, Client} = connect(addr([{host, <<"127.0.0.1">>}, {port, integer_to_binary(Port)}])),
    {ok, Server} = gen_tcp:accept(LSock, 1000),
    ok = gen_tcp:close(LSock),
    {Client, Server}.

close_pair(Client, Server) ->
    ok = close(Client),
    ok = gen_tcp:close(Server).

one_line_per_recv_test() ->
    {C, S} = pair(),
    %% Two commands in a single segment: still one per recv.
    ok = gen_tcp:send(S, <<"REJECTED EXTERNAL ANONYMOUS\r\nOK 1234\r\n">>),
    ?assertEqual({ok, <<"REJECTED EXTERNAL ANONYMOUS\r\n">>}, recv(C, 1000)),
    ?assertEqual({ok, <<"OK 1234\r\n">>}, recv(C, 1000)),
    close_pair(C, S).

partial_line_is_not_delivered_test() ->
    {C, S} = pair(),
    ok = gen_tcp:send(S, <<"OK 12">>),
    ?assertEqual({error, timeout}, recv(C, 100)),
    %% A timeout leaves the partial line buffered and the socket usable.
    ok = gen_tcp:send(S, <<"34\r\n">>),
    ?assertEqual({ok, <<"OK 1234\r\n">>}, recv(C, 1000)),
    close_pair(C, S).

%% An endless line is truncated, not refused: the driver hands over what it
%% holds, with no terminator. That is the bound that matters -- a peer which
%% never sends a newline cannot make the buffer grow -- and the unterminated
%% line is not a command.
over_long_line_is_truncated_test() ->
    Sent = 4 * ?MAX_LINE,
    {C, S} = pair(),
    ok = gen_tcp:send(S, binary:copy(<<"A">>, Sent)),
    {ok, Line} = recv(C, 1000),
    %% Where exactly it is cut is the driver's business -- it truncates at the
    %% receive buffer, whose default is not ours to pin -- so what is asserted
    %% is that it is cut at all, and that what comes out is not a command.
    ?assert(byte_size(Line) < Sent),
    ?assertEqual(nomatch, binary:match(Line, <<"\r\n">>)),
    ?assertMatch({unknown, _}, dbus_sasl:parse(Line)),
    close_pair(C, S).

switch_to_raw_keeps_buffered_bytes_test() ->
    {C, S} = pair(),
    %% The peer's first message bytes arrive in the same segment as `OK'.
    ok = gen_tcp:send(S, <<"OK 1234\r\n", 1, 2, 3, 4, 5>>),
    ?assertEqual({ok, <<"OK 1234\r\n">>}, recv(C, 1000)),
    ok = set_mode(C, raw),
    ?assertEqual({ok, <<1, 2, 3, 4, 5>>}, recv(C, 1000)),
    close_pair(C, S).

switch_back_to_line_test() ->
    {C, S} = pair(),
    ok = set_mode(C, raw),
    ok = gen_tcp:send(S, <<"OK 1234\r\n">>),
    ok = set_mode(C, line),
    ?assertEqual({ok, <<"OK 1234\r\n">>}, recv(C, 1000)),
    close_pair(C, S).

send_test() ->
    {C, S} = pair(),
    ok = send(C, [<<"AUTH">>, <<" EXTERNAL 31303030\r\n">>]),
    ?assertEqual({ok, <<"AUTH EXTERNAL 31303030\r\n">>}, gen_tcp:recv(S, 0, 1000)),
    close_pair(C, S).

%% The nul byte dbus_connection sends before authenticating is not framing.
send_credentials_byte_test() ->
    {C, S} = pair(),
    ok = send(C, <<0>>),
    ?assertEqual({ok, <<0>>}, gen_tcp:recv(S, 1, 1000)),
    close_pair(C, S).

peer_close_test() ->
    {C, S} = pair(),
    ok = gen_tcp:close(S),
    ?assertEqual({error, closed}, recv(C, 1000)),
    ok = close(C).

%% dbus_connection's reader is a separate process from the one that connected.
recv_from_another_process_test() ->
    {C, S} = pair(),
    Self = self(),
    _ = spawn_link(fun() -> Self ! {received, recv(C, 2000)} end),
    ok = gen_tcp:send(S, <<"OK 1234\r\n">>),
    receive
        {received, Result} -> ?assertEqual({ok, <<"OK 1234\r\n">>}, Result)
    after 3000 -> ?assert(false)
    end,
    close_pair(C, S).

no_unix_fd_test() ->
    {C, S} = pair(),
    ?assertEqual(false, support_unix_fd(C)),
    ?assertEqual(ok, disable_unix_fd(C)),
    close_pair(C, S).

%% Skipped where the host has no IPv6 loopback.
ipv6_loopback_test() ->
    Opts = [binary, {active, false}, inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}, {reuseaddr, true}],
    case gen_tcp:listen(0, Opts) of
        {error, _} ->
            ok;
        {ok, LSock} ->
            {ok, Port} = inet:port(LSock),
            Address = addr([
                {host, <<"::1">>}, {port, integer_to_binary(Port)}, {family, <<"ipv6">>}
            ]),
            {ok, C} = connect(Address),
            {ok, S} = gen_tcp:accept(LSock, 1000),
            ok = gen_tcp:close(LSock),
            ok = gen_tcp:send(S, <<"OK 1234\r\n">>),
            ?assertEqual({ok, <<"OK 1234\r\n">>}, recv(C, 1000)),
            close_pair(C, S)
    end.

connection_refused_test() ->
    %% A port nothing listens on: bind one, learn its number, drop it.
    {ok, LSock} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    ok = gen_tcp:close(LSock),
    ?assertMatch(
        {error, _},
        connect(addr([{host, <<"127.0.0.1">>}, {port, integer_to_binary(Port)}]))
    ).

-endif.
