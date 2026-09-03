-module(dbus_unix_fd_SUITE).
-moduledoc """
End-to-end Unix file-descriptor passing, against a real `dbus-daemon`.

A `socketpair` proves the `sendmsg'/`recvmsg' code round-trips a number; it
cannot prove the flip of `dbus_transport_unix:support_unix_fd/0` was safe. That
takes a peer that negotiates: the descriptor has to survive `NEGOTIATE_UNIX_FD`,
the `UNIX_FDS` header the daemon rewrites as it forwards, and the daemon's own
`SCM_RIGHTS` hop -- and it still has to name the same open file at the far end.

Two groups, one suite:

  * `unix' runs against the ambient session bus and is the positive case;
  * `tcp' runs against a private `dbus-daemon' listening on `127.0.0.1'.
    `dbus-daemon' refuses `NEGOTIATE_UNIX_FD' there, and the transport never
    offers it, so the case is that the connection authenticates normally with
    `agree_unix_fd = false'.

Both groups skip rather than fail when what they need is absent: no session bus,
no `dbus-daemon' on `PATH', no `/proc' to identify a descriptor through.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

-export([
    unix_fd_is_negotiated/1,
    descriptor_survives_the_bus/1,
    descriptors_are_not_leaked/1,
    unix_fd_is_not_negotiated/1,
    descriptors_are_refused/1
]).

%% Long enough that a busy machine is not a failure, short enough that a
%% message which will never arrive does not eat the timetrap.
-define(RECV_TIMEOUT, 5000).

-define(ECHO_PATH, <<"/org/erlang/dbus/FdEcho">>).
-define(ECHO_IFACE, <<"org.erlang.dbus.FdEcho">>).
-define(ECHO_MEMBER, <<"Echo">>).

-define(BUS_NAME, <<"org.freedesktop.DBus">>).
-define(BUS_PATH, <<"/org/freedesktop/DBus">>).

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, unix}, {group, tcp}].

groups() ->
    [
        {unix, [sequence], [
            unix_fd_is_negotiated,
            descriptor_survives_the_bus,
            descriptors_are_not_leaked
        ]},
        {tcp, [sequence], [
            unix_fd_is_not_negotiated,
            descriptors_are_refused
        ]}
    ].

%% Both the identity check and the leak check read `/proc/self/fd', and the
%% descriptors are closed through a NIF: without either there is nothing to
%% assert, which is a skip rather than a failure.
init_per_suite(Config) ->
    case {fd_identity_works(), nif_loaded()} of
        {true, true} -> Config;
        {false, _} -> {skip, "no /proc/self/fd: cannot identify a descriptor"};
        {_, false} -> {skip, "dbus_fd NIF not loaded"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(unix, Config) ->
    case session_bus_address() of
        {ok, Address} -> [{address, Address} | Config];
        error -> {skip, "no unix: address in DBUS_SESSION_BUS_ADDRESS"}
    end;
init_per_group(tcp, Config) ->
    case os:find_executable("dbus-daemon") of
        false -> {skip, "dbus-daemon not found in PATH"};
        Exe -> start_tcp_daemon(Exe, Config)
    end.

end_per_group(tcp, Config) ->
    stop_tcp_daemon(?config(daemon, Config));
end_per_group(_Group, _Config) ->
    ok.

%%%
%%% unix
%%%

%% The whole point of the flip, read off the wire rather than off the record:
%% the client offers `NEGOTIATE_UNIX_FD' and `dbus-daemon' answers
%% `AGREE_UNIX_FD'.
unix_fd_is_negotiated(Config) ->
    {Result, Sent, Received} = traced_auth(?config(address, Config)),
    ?assertMatch({ok, #dbus_auth{agree_unix_fd = true}}, Result),
    ?assert(contains(Sent, <<"NEGOTIATE_UNIX_FD">>)),
    ?assert(contains(Received, <<"AGREE_UNIX_FD">>)).

%% A descriptor across the bus and back. What is compared is the *file*, not
%% the number: `SCM_RIGHTS' installs a new descriptor in the receiving process
%% every hop, so the number is expected to differ while `st_dev'/`st_ino' are
%% expected not to.
descriptor_survives_the_bus(Config) ->
    Address = ?config(address, Config),
    with_descriptor(fun(Fd) ->
        Identity = fd_identity(Fd),
        Echoed = echo_through_bus(Address, Fd),
        try
            ?assertNotEqual(Fd, Echoed),
            ?assertEqual(Identity, fd_identity(Echoed))
        after
            ok = dbus_fd:close(Echoed)
        end
    end).

%% Everything the round trip opens is closed again: the descriptors the two
%% connections received, the sockets they run on, and the descriptor that
%% travelled. Run twice, because a leak of one per round trip is the shape a
%% single run cannot tell from a baseline that was never stable.
descriptors_are_not_leaked(Config) ->
    Address = ?config(address, Config),
    Before = open_fd_count(),
    lists:foreach(
        fun(_) ->
            with_descriptor(fun(Fd) ->
                Echoed = echo_through_bus(Address, Fd),
                ok = dbus_fd:close(Echoed)
            end)
        end,
        lists:seq(1, 2)
    ),
    ?assertEqual(Before, settled_fd_count(Before)).

%%%
%%% tcp
%%%

%% `dbus_transport_tcp:support_unix_fd/0' is `false', so the command is never
%% written -- the daemon, which refuses it on TCP, is never asked. What the
%% case pins is that this costs nothing: authentication completes and the
%% connection is usable.
unix_fd_is_not_negotiated(Config) ->
    {Result, Sent, Received} = traced_auth(?config(address, Config)),
    ?assertMatch({ok, #dbus_auth{agree_unix_fd = false}}, Result),
    {ok, #dbus_auth{guid = Guid}} = Result,
    ?assert(is_binary(Guid)),
    ?assertNot(contains(Sent, <<"NEGOTIATE_UNIX_FD">>)),
    ?assertNot(contains(Received, <<"AGREE_UNIX_FD">>)).

%% The other half of the same connection: it works normally, and a message
%% carrying a descriptor is refused before anything is written.
descriptors_are_refused(Config) ->
    Address = ?config(address, Config),
    {ok, Conn} = dbus_connection:start_link([Address]),
    try
        Name = hello(Conn),
        ?assert(is_binary(Name)),
        with_descriptor(fun(Fd) ->
            Call = fd_call(Name, 0),
            ?assertEqual(
                {error, unix_fd_not_negotiated},
                dbus_connection:send(Conn, Call#dbus_message{fds = [Fd]})
            ),
            %% Refused, not spent: the descriptor is still ours and still open.
            ?assertMatch({_, _}, fd_identity(Fd))
        end)
    after
        dbus_connection:stop(Conn)
    end.

%%%
%%% The round trip
%%%

%% Two connections to the same bus, both owned by this process. `A' sends a
%% method call whose single `h' points at `Fd'; `B' receives it and answers
%% with the descriptor it was handed, which `A' gets back as a third number.
%% `B' closes its own copy -- the reply gave the daemon a duplicate, not the
%% descriptor.
echo_through_bus(Address, Fd) ->
    {ok, A} = dbus_connection:start_link([Address]),
    try
        AName = hello(A),
        {ok, B} = dbus_connection:start_link([Address]),
        try
            BName = hello(B),
            echo_through_bus(A, AName, B, BName, Fd)
        after
            dbus_connection:stop(B)
        end
    after
        dbus_connection:stop(A)
    end.

echo_through_bus(A, AName, B, BName, Fd) ->
    Call = fd_call(BName, 0),
    {ok, Serial} = dbus_connection:send(A, Call#dbus_message{fds = [Fd]}),

    Received = expect_call(?ECHO_MEMBER),
    ?assertEqual(0, Received#dbus_message.body),
    {ok, Relayed} = dbus_message:fd(0, Received),
    ?assertNotEqual(Fd, Relayed),

    try
        Return = fd_return(AName, dbus_message:get_serial(Received)),
        {ok, _} = dbus_connection:send(B, Return#dbus_message{fds = [Relayed]})
    after
        %% Sent or not, this copy is ours: `sendmsg(2)' gave the daemon a
        %% duplicate of the open file description, not this number.
        ok = dbus_fd:close(Relayed)
    end,

    Echo = expect_return(Serial),
    ?assertEqual(0, Echo#dbus_message.body),
    {ok, Echoed} = dbus_message:fd(0, Echo),
    Echoed.

hello(Conn) ->
    Call = method_call(?BUS_NAME, ?BUS_PATH, ?BUS_NAME, <<"Hello">>),
    {ok, Serial} = dbus_connection:send(Conn, Call),
    Msg = expect_return(Serial),
    Msg#dbus_message.body.

%%%
%%% Messages
%%%

method_call(Destination, Path, Interface, Member) ->
    #dbus_message{
        header = #dbus_header{
            type = ?TYPE_METHOD_CALL,
            fields = [
                {?FIELD_PATH, #dbus_variant{type = object_path, value = Path}},
                {?FIELD_INTERFACE, #dbus_variant{type = string, value = Interface}},
                {?FIELD_MEMBER, #dbus_variant{type = string, value = Member}},
                {?FIELD_DESTINATION, #dbus_variant{type = string, value = Destination}}
            ]
        }
    }.

%% A body of one `h'. The value is an index into `#dbus_message.fds', never a
%% descriptor, so it is 0 whatever number the caller is passing.
fd_call(Destination, Index) ->
    Call = method_call(Destination, ?ECHO_PATH, ?ECHO_IFACE, ?ECHO_MEMBER),
    Call#dbus_message{body = {[unix_fd], [Index]}}.

fd_return(Destination, ReplySerial) ->
    #dbus_message{
        header = #dbus_header{
            type = ?TYPE_METHOD_RETURN,
            fields = [
                {?FIELD_REPLY_SERIAL, #dbus_variant{type = uint32, value = ReplySerial}},
                {?FIELD_DESTINATION, #dbus_variant{type = string, value = Destination}}
            ]
        },
        body = {[unix_fd], [0]}
    }.

%%%
%%% Receiving
%%%

%% `NameAcquired' and anything else the bus volunteers is skipped: what is
%% waited for is named by the predicate.
recv_dbus(Pred) ->
    receive
        {dbus, Msg} ->
            case Pred(Msg) of
                true -> Msg;
                false -> recv_dbus(Pred)
            end
    after ?RECV_TIMEOUT ->
        ct:fail(no_matching_dbus_message)
    end.

expect_return(Serial) ->
    Msg = recv_dbus(fun(M) -> is_reply_to(Serial, M) end),
    case Msg of
        #dbus_message{header = #dbus_header{type = ?TYPE_ERROR}} ->
            ct:fail({dbus_error, dbus_message:find_field(?FIELD_ERROR_NAME, Msg)});
        _ ->
            Msg
    end.

expect_call(Member) ->
    recv_dbus(fun(M) -> is_call_of(Member, M) end).

is_reply_to(Serial, #dbus_message{header = #dbus_header{type = Type}} = Msg) when
    Type =:= ?TYPE_METHOD_RETURN; Type =:= ?TYPE_ERROR
->
    dbus_message:find_field(?FIELD_REPLY_SERIAL, Msg) =:= Serial;
is_reply_to(_Serial, _Msg) ->
    false.

is_call_of(Member, #dbus_message{header = #dbus_header{type = ?TYPE_METHOD_CALL}} = Msg) ->
    dbus_message:find_field(?FIELD_MEMBER, Msg) =:= Member;
is_call_of(_Member, _Msg) ->
    false.

%%%
%%% The auth conversation, traced
%%%

%% Authentication runs in the calling process, so tracing it catches the whole
%% conversation and nothing else. The tracer has to be a *different* process:
%% a process is excluded from its own call trace, so tracing `self()' to
%% `self()' delivers nothing at all -- and an empty trace makes every one of
%% these assertions pass or fail for the wrong reason.
traced_auth(Address) ->
    {ok, Conn} = dbus_transport:connect(Address),
    try
        ok = dbus_transport:send(Conn, <<0>>),
        Tracer = trace_on(),
        Result = dbus_auth_client_mech:try_auth(#{}, Conn),
        {Sent, Received} = trace_off(Tracer),
        {Result, Sent, Received}
    after
        _ = dbus_transport:close(Conn)
    end.

trace_on() ->
    Tracer = spawn_link(fun() -> collect_trace([], []) end),
    _ = erlang:trace_pattern({dbus_transport, send, 2}, true, [local]),
    _ = erlang:trace_pattern({dbus_transport, recv, 2}, [{'_', [], [{return_trace}]}], [local]),
    _ = erlang:trace(self(), true, [call, {tracer, Tracer}]),
    Tracer.

trace_off(Tracer) ->
    _ = erlang:trace(self(), false, [call]),
    _ = erlang:trace_pattern({dbus_transport, send, 2}, false, [local]),
    _ = erlang:trace_pattern({dbus_transport, recv, 2}, false, [local]),
    %% Trace messages are ordinary messages sent by the traced process to the
    %% tracer, so this request cannot overtake the ones already sent.
    Tracer ! {self(), flush},
    receive
        {Tracer, Sent, Received} -> {Sent, Received}
    after ?RECV_TIMEOUT ->
        ct:fail(trace_not_collected)
    end.

collect_trace(Sent, Received) ->
    receive
        {trace, _Pid, call, {dbus_transport, send, [_Conn, Data]}} ->
            collect_trace([iolist_to_binary(Data) | Sent], Received);
        {trace, _Pid, return_from, {dbus_transport, recv, 2}, {ok, Data, _Fds}} ->
            collect_trace(Sent, [Data | Received]);
        {From, flush} ->
            From ! {self(), join(Sent), join(Received)};
        _Other ->
            collect_trace(Sent, Received)
    end.

join(Reversed) ->
    iolist_to_binary(lists:reverse(Reversed)).

contains(Haystack, Needle) ->
    binary:match(Haystack, Needle) =/= nomatch.

%%%
%%% Descriptors
%%%

%% `socket:getopt/2' is the one way to obtain a descriptor from Erlang, so the
%% file that travels is a socket. The number stays owned by `Sock' -- only the
%% copies the bus hands back are closed here.
with_descriptor(Fun) ->
    {ok, Sock} = socket:open(local, stream, default),
    try
        {ok, Fd} = socket:getopt(Sock, {otp, fd}),
        Fun(Fd)
    after
        _ = socket:close(Sock)
    end.

%% `stat(2)' through `/proc/self/fd/<n>' follows the magic link to the open
%% file itself, so this is `st_dev'/`st_ino' of what the descriptor names --
%% the only thing that is the same on both sides of a `SCM_RIGHTS' hop.
fd_identity(Fd) ->
    Path = "/proc/self/fd/" ++ integer_to_list(Fd),
    case file:read_file_info(Path) of
        {ok, #file_info{major_device = Dev, inode = Inode}} -> {Dev, Inode};
        {error, Reason} -> ct:fail({fd_identity, Fd, Reason})
    end.

fd_identity_works() ->
    {ok, Sock} = socket:open(local, stream, default),
    try
        {ok, Fd} = socket:getopt(Sock, {otp, fd}),
        Path = "/proc/self/fd/" ++ integer_to_list(Fd),
        case file:read_file_info(Path) of
            {ok, #file_info{}} -> true;
            {error, _} -> false
        end
    after
        _ = socket:close(Sock)
    end.

nif_loaded() ->
    try dbus_fd:close(16#7FFFFFFE) of
        _ -> true
    catch
        error:nif_library_not_loaded -> false
    end.

open_fd_count() ->
    {ok, Names} = file:list_dir("/proc/self/fd"),
    length(Names).

%% A socket the test closed is not always gone from `/proc/self/fd' the
%% instant the closing call returns -- the daemon side of the connection and
%% OTP's own teardown both run elsewhere. A leak does not settle, so waiting
%% for the baseline is not the same as accepting one.
settled_fd_count(Baseline) ->
    settled_fd_count(Baseline, 50).

settled_fd_count(_Baseline, 0) ->
    open_fd_count();
settled_fd_count(Baseline, Retries) ->
    case open_fd_count() of
        Baseline ->
            Baseline;
        _Other ->
            timer:sleep(100),
            settled_fd_count(Baseline, Retries - 1)
    end.

%%%
%%% Buses
%%%

session_bus_address() ->
    case os:getenv("DBUS_SESSION_BUS_ADDRESS") of
        false -> error;
        Value -> address_of_scheme(<<"unix">>, iolist_to_binary(Value))
    end.

address_of_scheme(Scheme, Binary) ->
    try dbus_address:parse(Binary) of
        {ok, Addresses} ->
            case [A || #dbus_address{scheme = S} = A <- Addresses, S =:= Scheme] of
                [Address | _] -> {ok, Address};
                [] -> error
            end;
        {error, _} ->
            error
    catch
        _:_ -> error
    end.

%% A bus of our own: nothing on this machine listens on TCP by default, and
%% `EXTERNAL' cannot work there -- no credentials ride along with the socket --
%% so the daemon is told to offer `DBUS_COOKIE_SHA1' and nothing else.
%%
%% AppArmor mediation is turned off with it. A `dbus-daemon' built against
%% libapparmor asks the kernel for the peer's confinement as it accepts, which
%% only works on a `AF_UNIX' socket; over TCP the query fails with `EPROTONOSUPPORT'
%% and the daemon drops the connection before a byte of SASL is read
%% ("Unable to set up new connection: Failed to get AppArmor confinement
%% information of socket peer"), which reaches the client as `econnreset'.
start_tcp_daemon(Exe, Config) ->
    ConfigFile = filename:join(?config(priv_dir, Config), "tcp-session.conf"),
    ok = file:write_file(ConfigFile, daemon_config()),
    Port = open_port(
        {spawn_executable, Exe},
        [
            {args, ["--config-file=" ++ ConfigFile, "--print-address", "--nofork"]},
            binary,
            exit_status,
            stderr_to_stdout,
            {line, 4096}
        ]
    ),
    case daemon_address(Port) of
        {ok, Address} ->
            [{address, Address}, {daemon, Port} | Config];
        {error, Reason} ->
            stop_tcp_daemon(Port),
            {skip, lists:flatten(io_lib:format("no tcp dbus-daemon: ~p", [Reason]))}
    end.

daemon_address(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            case address_of_scheme(<<"tcp">>, Line) of
                {ok, Address} -> {ok, Address};
                error -> daemon_address(Port)
            end;
        {Port, {data, _Partial}} ->
            daemon_address(Port);
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}}
    after 10000 ->
        {error, timeout}
    end.

stop_tcp_daemon(undefined) ->
    ok;
stop_tcp_daemon(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} -> _ = os:cmd("kill " ++ integer_to_list(OsPid));
        undefined -> ok
    end,
    try
        erlang:port_close(Port)
    catch
        error:badarg -> ok
    end,
    ok.

daemon_config() ->
    <<
        "<!DOCTYPE busconfig PUBLIC \"-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN\"\n"
        " \"http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd\">\n"
        "<busconfig>\n"
        "  <type>session</type>\n"
        "  <listen>tcp:host=127.0.0.1,bind=127.0.0.1,port=0,family=ipv4</listen>\n"
        "  <apparmor mode=\"disabled\"/>\n"
        "  <auth>DBUS_COOKIE_SHA1</auth>\n"
        "  <policy context=\"default\">\n"
        "    <allow send_destination=\"*\" eavesdrop=\"true\"/>\n"
        "    <allow eavesdrop=\"true\"/>\n"
        "    <allow own=\"*\"/>\n"
        "  </policy>\n"
        "</busconfig>\n"
    >>.
