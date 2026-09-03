-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".
-include("dbus.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% What a transport module's `connect/1' hands back: the socket alone. It is
%% `connect/1' here that pairs it with the module it came from, and only the
%% pair is a connection() -- the other two callbacks take that pair, since
%% dispatching to a module needs its name.
-type socket() :: socket:socket().

-record(transport, {
    mod :: module(),
    sock :: socket:socket(),
    support_unix_fd :: boolean()
}).

-opaque connection() :: #transport{}.
-export_type([socket/0, connection/0]).

%% `cmsghdr' is a `size_t' and two `int's, padded to the platform's alignment:
%% 16 bytes wherever this runs. Room for two messages' worth of descriptors is
%% deliberate slack -- one `recvmsg' may return several `rights' messages, and
%% a control buffer that is too small does not shorten the read: the kernel
%% drops the descriptors that do not fit, closes them, and says so only
%% through `ctrunc'.
-define(CMSG_HEADER_SIZE, 16).
-define(CTRL_SIZE, 2 * (?CMSG_HEADER_SIZE + ?MAX_UNIX_FDS * 4)).

-callback connect(Address :: dbus_address()) ->
    {ok, socket()}
    | {error, Reason :: term()}.

-callback support_unix_fd() -> boolean().

-export([
    connect/1,
    send/2,
    send/3,
    recv/2,
    close/1,
    support_unix_fd/1
]).

-spec connect(dbus_address()) ->
    {ok, connection()}
    | {error, Reason :: term()}.
connect(Address) ->
    case resolve(Address) of
        {ok, Transport} ->
            case Transport:connect(Address) of
                {ok, Conn} ->
                    T = #transport{
                        mod = Transport,
                        sock = Conn,
                        support_unix_fd = Transport:support_unix_fd()
                    },
                    {ok, T};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, undefined} ->
            {error, {invalid_transport, Address}}
    end.

-spec send(connection(), iodata()) ->
    ok
    | {error, Reason :: term()}.
send(Conn, Data) ->
    send(Conn, Data, []).

%% Descriptors travel with the bytes or not at all: `?MAX_UNIX_FDS' of them at
%% most, and none over a transport that cannot carry them. What the *peer*
%% agreed to is not known here -- `support_unix_fd' says the socket can carry a
%% descriptor, `AGREE_UNIX_FD' says the peer allows it, and the second belongs
%% to `m:dbus_connection', which is also the layer that writes `UNIX_FDS'.
-spec send(connection(), iodata(), [dbus_fd:fd()]) ->
    ok
    | {error, Reason :: term()}.
send(#transport{sock = S}, Data, []) ->
    socket:send(S, Data);
send(#transport{support_unix_fd = false}, _Data, [_ | _]) ->
    {error, unix_fd_not_supported};
send(#transport{}, _Data, Fds) when length(Fds) > ?MAX_UNIX_FDS ->
    {error, {too_many_fds, length(Fds)}};
send(#transport{sock = S}, Data, Fds) ->
    sendmsg(S, iolist_to_binary(Data), Fds).

-spec recv(connection(), timeout()) ->
    {ok, Data :: binary(), Fds :: [dbus_fd:fd()]}
    | {error, closed | timeout | term()}.
recv(#transport{sock = S, support_unix_fd = false}, Timeout) ->
    case socket:recv(S, 0, [], Timeout) of
        {ok, Data} -> {ok, Data, []};
        {error, _} = E -> E
    end;
recv(#transport{sock = S}, Timeout) ->
    case socket:recvmsg(S, 0, ?CTRL_SIZE, [], Timeout) of
        {ok, #{iov := Iov, ctrl := Ctrl, flags := Flags}} ->
            received(iolist_to_binary(Iov), Ctrl, Flags);
        {error, _} = E ->
            E
    end.

-spec close(connection()) ->
    ok
    | {error, Reason :: term()}.
close(#transport{sock = S}) ->
    socket:close(S).

-spec support_unix_fd(connection()) -> boolean().
support_unix_fd(#transport{support_unix_fd = Support}) ->
    Support.

%%%
%%% Priv
%%%
-spec resolve(dbus_address() | module()) ->
    {ok, module()} | {error, undefined}.
resolve(Mod) when is_atom(Mod) ->
    {ok, Mod};
resolve(#dbus_address{} = Address) ->
    case dbus_address:scheme(Address) of
        <<"tcp">> -> {ok, dbus_transport_tcp};
        <<"nonce-tcp">> -> {ok, dbus_transport_nonce_tcp};
        <<"unix">> -> {ok, dbus_transport_unix};
        _ -> {error, undefined}
    end.

%% The descriptors ride on the *first byte* of the payload and the rest of it
%% follows as ordinary bytes, because a `sendmsg' that writes only part of its
%% `iov' delivers the `rights' again with every continuation. OTP does that
%% continuation itself under an `infinity' timeout -- the `{ok, RestData}' the
%% documentation describes is what a caller sees only with a deadline -- and
%% the control message is carried along with it: a 4 MiB payload sent in one
%% call arrives with a copy of every descriptor per segment. One byte is
%% written whole or not at all, so there is no remainder for a continuation to
%% duplicate.
%%
%% `SCM_RIGHTS' with an empty `iov' is not delivered on Linux at all, which is
%% why descriptors cannot be flushed on their own. A D-Bus message is at least
%% sixteen bytes, so there is always a byte to carry them.
%%
%% Attaching them to the first byte rather than the last is what the receiving
%% side already assumes: the kernel delivers ancillary data no later than the
%% end of the segment it came with, so descriptors arrive at or before the
%% message that declares them, never after.
sendmsg(_S, <<>>, _Fds) ->
    {error, no_payload_for_fds};
sendmsg(S, <<First:1/binary, Rest/binary>>, Fds) ->
    Msg = #{
        iov => [First],
        ctrl => [
            #{
                level => socket,
                type => rights,
                data => encode_fds(Fds)
            }
        ]
    },
    case socket:sendmsg(S, Msg, [], infinity) of
        ok ->
            send_rest(S, Rest);
        %% A remainder of a one-byte `iov' would mean nothing was written and
        %% the descriptors went nowhere: an error, not something to retry.
        {ok, _} ->
            {error, fd_send_incomplete};
        {error, _} = E ->
            E
    end.

send_rest(_S, <<>>) -> ok;
send_rest(S, Rest) -> socket:send(S, Rest).

%% `ctrunc' says the control buffer was too small and the kernel closed the
%% descriptors that did not fit. They are gone, so this is a protocol error
%% rather than a short read -- and the ones that did arrive are ours to close
%% before the connection is dropped.
received(Data, Ctrl, Flags) ->
    case rights(Ctrl) of
        {ok, Fds} ->
            case lists:member(ctrunc, Flags) of
                true ->
                    _ = [dbus_fd:close(Fd) || Fd <- Fds],
                    {error, fd_truncated};
                false ->
                    {ok, Data, Fds}
            end;
        {error, _} = E ->
            E
    end.

%% One `recvmsg' may return several `rights' messages; their descriptors
%% concatenate, in order. Anything else in `ctrl' is not ours.
rights(Ctrl) ->
    rights([Bin || #{level := socket, type := rights, data := Bin} <- Ctrl], []).

rights([], Acc) ->
    {ok, lists:append(lists:reverse(Acc))};
rights([Bin | Rest], Acc) ->
    case decode_fds(Bin) of
        {ok, Fds} -> rights(Rest, [Fds | Acc]);
        {error, _} = E -> E
    end.

%% OTP hands `rights' over undecoded -- `t:socket:cmsg_recv/0' types it
%% `data := binary()' with no `value' beside it, unlike `timestamp' -- so the
%% payload is an array of native C `int's. Native order, native width, signed:
%% this is memory the kernel wrote, not a network-format integer array. A
%% payload that is not a whole number of them is malformed and must not be
%% parsed into descriptors that were never sent.
encode_fds(Fds) ->
    <<<<Fd:32/native-signed>> || Fd <- Fds>>.

decode_fds(Bin) when byte_size(Bin) rem 4 =:= 0 ->
    {ok, [Fd || <<Fd:32/native-signed>> <= Bin]};
decode_fds(Bin) ->
    {error, {bad_rights, byte_size(Bin)}}.

%%%
%%% eunit
%%%
-ifdef(TEST).

%%% The contract these tests pin:
%%%
%%%   * `rights' is the kernel's own array of native `int's, and a payload that
%%%     is not a whole number of them is an error rather than a shorter list;
%%%   * a transport that cannot carry descriptors refuses a non-empty list
%%%     before writing anything, and `recv/2' still answers with a list --
%%%     an empty one;
%%%   * over an fd-capable connection a descriptor arrives as a *different*
%%%     number naming the same open file, exactly once, whatever the size of
%%%     the payload it travelled with.

%%%
%%% The codec -- no socket involved
%%%

codec_roundtrip_test() ->
    Fds = [0, 3, 16#7FFFFFFF],
    ?assertEqual({ok, Fds}, decode_fds(encode_fds(Fds))).

no_fds_is_an_empty_payload_test() ->
    ?assertEqual(<<>>, encode_fds([])),
    ?assertEqual({ok, []}, decode_fds(<<>>)).

%% Four bytes to an `int': three of them are not one descriptor.
decode_fds_refuses_a_partial_int_test() ->
    ?assertEqual({error, {bad_rights, 3}}, decode_fds(<<0, 0, 0>>)).

%%%
%%% Over a connected pair
%%%

%% The number crossing the socket is not the number that was sent -- both are
%% open in this OS process -- so what it names is checked by using it: the
%% adopted socket reaches the peer the original was connected to.
%%
%% The bytes come back in more than one `recv/2': the descriptors ride on the
%% first of them, and the kernel ends a read at the ancillary boundary. That is
%% the same byte stream a transport has always been -- one send is not one
%% receive -- and it is why `dbus_connection' accumulates.
fd_roundtrip_test() ->
    with_fd_pair(fun(A, B) ->
        socket_pair(fun(Near, Far) ->
            {ok, Fd} = socket:getopt(Near, {otp, fd}),

            ok = send(A, <<"FD">>, [Fd]),
            {Data, [Received]} = drain(B, 2),
            ?assertEqual(<<"FD">>, Data),
            ?assertNotEqual(Fd, Received),

            {ok, Adopted} = socket:open(Received),
            ok = socket:send(Adopted, <<"ping">>),
            ?assertEqual({ok, <<"ping">>}, socket:recv(Far, 4, 1000)),
            %% Closes the descriptor that arrived; `Near' keeps its own.
            ok = socket:close(Adopted)
        end)
    end).

%% Bytes without descriptors take the same path as `send/2'.
no_fds_roundtrip_test() ->
    with_fd_pair(fun(A, B) ->
        ok = send(A, <<0, "AUTH\r\n">>),
        ?assertEqual({ok, <<0, "AUTH\r\n">>, []}, recv(B, 1000))
    end).

%% Refused before the `sendmsg', so the numbers need not name anything: what
%% is asserted is that nothing reached the peer.
too_many_fds_test() ->
    with_fd_pair(fun(A, B) ->
        Fds = lists:duplicate(?MAX_UNIX_FDS + 1, 0),
        ?assertEqual({error, {too_many_fds, ?MAX_UNIX_FDS + 1}}, send(A, <<"FD">>, Fds)),
        ?assertEqual({error, timeout}, recv(B, 100))
    end).

%% `SCM_RIGHTS' with an empty `iov' is not delivered, so there is no byte to
%% carry the descriptors and nothing is written.
fds_need_a_payload_test() ->
    with_fd_pair(fun(A, B) ->
        ?assertEqual({error, no_payload_for_fds}, send(A, <<>>, [0])),
        ?assertEqual({error, timeout}, recv(B, 100))
    end).

%% More than the socket buffer takes, so the send completes in more than one
%% write -- and the descriptor must not be attached to the second one.
partial_write_test_() ->
    {timeout, 60, fun() -> with_fd_pair(fun partial_write/2) end}.

%% Named rather than written inline: the pair, the socket whose descriptor
%% travels, and the reader are three nested funs deep otherwise.
partial_write(A, B) ->
    socket_pair(fun(Near, _Far) -> partial_write(A, B, Near) end).

partial_write(A, B, Near) ->
    {ok, Fd} = socket:getopt(Near, {otp, fd}),
    Data = binary:copy(<<"0123456789abcdef">>, 262144),
    Size = byte_size(Data),
    Self = self(),
    %% Draining as it is written: the payload is larger than the socket
    %% buffer, so a `send/3' nobody reads never returns.
    _ = spawn_link(fun() -> Self ! {drained, drain(B, Size)} end),
    ok = send(A, Data, [Fd]),
    await_drained(Size).

await_drained(Size) ->
    receive
        {drained, {Received, Fds}} ->
            ?assertEqual(Size, byte_size(Received)),
            ?assertEqual(1, length(Fds)),
            _ = [dbus_fd:close(F) || F <- Fds],
            ok
    after 30000 ->
        ?assert(false)
    end.

%%%
%%% Helpers
%%%

%% Reads until `Size' bytes have arrived, keeping the descriptors that came
%% with them, in order.
drain(Conn, Size) ->
    drain(Conn, Size, <<>>, []).

drain(_Conn, Size, Received, Fds) when byte_size(Received) >= Size ->
    {Received, Fds};
drain(Conn, Size, Received, Fds) ->
    {ok, Data, More} = recv(Conn, 5000),
    drain(Conn, Size, <<Received/binary, Data/binary>>, Fds ++ More).

%% `support_unix_fd' is set here rather than taken from
%% `dbus_transport_unix:support_unix_fd/0': what is under test is the
%% fd-capable path, not the flag, and these sockets never went through
%% `connect/1'.
with_fd_pair(Fun) ->
    socket_pair(fun(Client, Server) ->
        Fun(fd_conn(Client), fd_conn(Server))
    end).

fd_conn(Sock) ->
    #transport{mod = dbus_transport_unix, sock = Sock, support_unix_fd = true}.

%% A connected `AF_UNIX' stream pair. OTP has no `socketpair/2', so it is a
%% listener, a connect and an accept; the pair is what matters, not how it was
%% made.
socket_pair(Fun) ->
    Path = tmp_path(),
    {ok, Listener} = socket:open(local, stream, default),
    {ok, Client} = socket:open(local, stream, default),
    try
        ok = socket:bind(Listener, #{family => local, path => Path}),
        ok = socket:listen(Listener),
        ok = socket:connect(Client, #{family => local, path => Path}),
        {ok, Server} = socket:accept(Listener, 1000),
        with_socket_pair(Fun, Client, Server)
    after
        _ = socket:close(Client),
        _ = socket:close(Listener),
        %% bind/2 creates the filesystem entry; nothing removes it on close.
        _ = file:delete(Path)
    end.

with_socket_pair(Fun, Client, Server) ->
    try
        Fun(Client, Server)
    after
        _ = socket:close(Server)
    end.

%% Kept short on purpose: `sun_path' is 108 bytes including the NUL, and a
%% longer one fails as `einval' with nothing saying why.
tmp_path() ->
    Dir = os:getenv("XDG_RUNTIME_DIR", "/tmp"),
    Name = "dbus-transport-test-" ++ integer_to_list(erlang:unique_integer([positive])),
    iolist_to_binary(filename:join(Dir, Name)).
-endif.
