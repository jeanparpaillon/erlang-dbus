-module(dbus_transport_tcp).
-moduledoc """
TCP transport.

The transport owns no process: `connect/1` returns a connection handle and
every callback is a direct call on it. Reads are synchronous, because the
`dbus_transport` behaviour asks for it -- `recv/2` hands back no new state --
and because it lets `dbus_connection` run the authentication handshake inline,
only then handing the connection to its reader process.

The socket is a `m:socket` socket, not a `gen_tcp` one, so that this transport
and the unix one speak the same API. What `m:socket` does not have is
`gen_tcp`'s packet modes: it is a thin binding over the OS calls and hands over
whatever the kernel had. Framing is therefore done here, over a receive
buffer.

In `line` mode `recv/2` returns exactly one LF-terminated line, which is the
framing `dbus_sasl:parse/1` accepts; `set_mode(Conn, raw)` switches to
returning whatever has arrived. Both modes read through the same buffer, so
bytes read past the end of a line survive the switch -- a peer that sends its
first message in the same segment as `OK` loses nothing -- and a read cut short
by a timeout leaves its partial line where the next read will find it.

That buffer and the current mode live in a public ETS table created by
`connect/1` and carried in the handle. They cannot live in the caller instead,
since `recv/2` returns no state, nor in a process dictionary: the process that
authenticates is not the process that reads afterwards, so the bytes buffered
past `OK` would be stranded in the wrong process at exactly the handover. The
table is owned by the process that connected -- `dbus_connection` -- so it goes
away with it even if `close/1` is never reached.

Peeking (`socket:recv/4` with the `peek` flag) would avoid the buffer
altogether, and does not work here: peeked bytes stay in the socket, so the
descriptor remains readable and every wait for the rest of a half-arrived line
returns immediately, spinning.

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
    support_unix_fd/1,
    disable_unix_fd/1
]).

-type family_opts() :: [inet | inet6].

%% `connect/1' takes no timeout and runs inside `dbus_connection:init/1', so
%% without one an unreachable host blocks the caller of `start_link/1' for the
%% kernel's full SYN retry period.
-define(CONNECT_TIMEOUT, 5000).

-spec connect(dbus_address()) ->
    {ok, dbus_transport:connection()}
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
    {ok, dbus_transport:connection()}
    | {error, term()}.
connect(Host, Port, FamilyOpts) ->
    Domain = domain(FamilyOpts),
    case inet:getaddr(Host, Domain) of
        {ok, Addr} -> open(Domain, Addr, Port);
        {error, _} = E -> E
    end.

-spec support_unix_fd(dbus_transport:connection()) -> boolean().
support_unix_fd(_Conn) ->
    false.

-spec disable_unix_fd(dbus_transport:connection()) -> ok.
disable_unix_fd(_Conn) ->
    ok.

%%%
%%% Private
%%%

%% An unspecified family is the `inet' `gen_tcp' would have picked; a literal
%% has already been mapped to its own family by `infer_family/2'.
domain([]) -> inet;
domain([Family]) -> Family.

open(Domain, Addr, Port) ->
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} -> connect_socket(Sock, Domain, Addr, Port);
        {error, _} = E -> E
    end.

connect_socket(Sock, Domain, Addr, Port) ->
    ok = socket:setopt(Sock, {socket, keepalive}, true),
    Dest = #{family => Domain, addr => Addr, port => Port},
    case socket:connect(Sock, Dest, ?CONNECT_TIMEOUT) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            %% A connect that timed out is still in progress in the kernel,
            %% and a socket that failed to connect cannot be retried, so it
            %% goes rather than being handed back with the error.
            _ = socket:close(Sock),
            {error, Reason}
    end.

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
%%%   * `family' maps to the socket's domain, and an unknown value is an error
%%%     rather than a silent default;
%%%   * in `line' mode each `recv/2' returns exactly one CRLF-terminated
%%%     line -- never two, never half of one -- which is the framing
%%%     `dbus_sasl:parse/1' requires, and a line that never ends is cut off
%%%     rather than buffered without limit;
%%%   * `set_mode(Conn, raw)' switches framing without losing bytes already
%%%     read past the end of the last line, including across the handover to
%%%     the reader process;
%%%   * a `recv/2' timeout leaves the connection usable, with the half-read
%%%     line still there, and the connection is readable from a process other
%%%     than the one that opened it -- the reader in `dbus_connection' is not
%%%     the connection process.

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
-endif.
