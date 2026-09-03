-module(dbus_connection).
-moduledoc """
Handles connection to a D-Bus peer.

## File descriptors

A message received with `UNIX_FDS` descriptors carries them on
`#dbus_message.fds`, and they are handed to the owner process with the message:
they are open in this OS process, they count against `RLIMIT_NOFILE`, and this
module will not close them. Consuming one -- `socket:open/2`, a port program, a
NIF -- or `dbus_fd:close/1` is the owner's job, and so is `dbus_fd:dup/1` for a
descriptor that has to outlive the message it arrived with.

The descriptors nobody takes delivery of are this module's: a message that
fails to parse, descriptors that arrive when `AGREE_UNIX_FD` was never
exchanged, and the queue left over when the connection dies. Those are closed
with `dbus_fd:close/1` and logged with a count and a reason -- a discard nobody
can see is indistinguishable from a leak.

Descriptors *sent* stay the sender's. `sendmsg(2)` gives the peer a copy of the
open file description, not the number, so the descriptors in a message passed
to `send/2` are still open here when it returns.
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([
    start_link/1,
    start_link/2,
    stop/1,
    get_guid/1,
    set_owner/2,
    send/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export_type([
    connection/0
]).

-record(state, {
    owner :: pid(),
    auth_ctx :: term() | undefined,
    transport :: dbus_transport:connection() | undefined,
    guid = <<>> :: binary(),
    agree_unix_fd = false :: boolean(),
    serial = 1 :: dbus_serial(),
    reader :: pid() | undefined,
    acc = <<>> :: binary(),
    fds = [] :: [dbus_fd:fd()]
}).

-type connection() :: pid().
-type option() ::
    {name, atom()}
    %% Per-mechanism context, key is module name
    | {auth_ctx, map()}.

-define(SERVER_OPTS_KEYS, [name]).

-spec start_link(dbus_address() | [dbus_address()]) -> gen_server:start_ret().
start_link(Addresses) when is_list(Addresses) ->
    start_link(Addresses, []);
start_link(Address) ->
    start_link([Address], []).

-spec start_link(dbus_address() | [dbus_address()], [option()]) -> gen_server:start_ret().
start_link(Addresses, Options) when is_list(Addresses) ->
    ServerOpts = server_opts(Options),
    Owner = self(),
    StartArgs = #{
        owner => Owner,
        addresses => Addresses,
        auth_ctx => proplists:get_value(auth_ctx, Options, #{})
    },
    gen_server:start_link(?MODULE, StartArgs, ServerOpts).

-spec stop(connection()) ->
    ok.
stop(Connection) ->
    gen_server:stop(Connection).

-doc """
Returns peer GUID
""".
-spec get_guid(connection()) ->
    {ok, binary()}.
get_guid(Connection) ->
    gen_server:call(Connection, get_guid).

-doc """
Change connection owner. Must be called by actual owner, or returns
`{error, forbidden}`
""".
-spec set_owner(connection(), pid()) ->
    ok
    | {error, forbidden}.
set_owner(Connection, Owner) ->
    gen_server:call(Connection, {set_owner, Owner}).

-doc """
Send given message to the D-Bus peer.

Returns message serial, allocated by connection.

`#dbus_message.fds` travel with it. On a connection that did not negotiate
`AGREE_UNIX_FD` a message carrying descriptors is `{error, unix_fd_not_negotiated}`
and nothing is written; the descriptors stay open and stay the caller's.
""".
-spec send(connection(), dbus_message()) ->
    {ok, dbus_serial()} | {error, term()}.
send(Connection, Message) ->
    gen_server:call(Connection, {send, Message}).

%%%
%%% Callbacks implementation
%%%
init(
    #{
        owner := Owner,
        addresses := Addresses,
        auth_ctx := AuthCtx
    } = _StartArgs
) ->
    State = #state{owner = Owner, auth_ctx = AuthCtx},
    try_connect(Addresses, State).

handle_call(get_guid, _From, #state{guid = Guid} = State) ->
    {reply, {ok, Guid}, State};
handle_call({set_owner, Owner}, {Owner, _Tag}, #state{owner = Owner} = State) ->
    {reply, ok, State#state{owner = Owner}};
handle_call({set_owner, _Owner}, _From, State) ->
    {reply, {error, forbidden}, State};
%% Refused before a serial is allocated: nothing was written, so nothing was
%% spent. The transport answers `unix_fd_not_supported' for a socket that
%% cannot carry a descriptor at all; this is the other half -- one it could
%% carry, that the peer never agreed to.
handle_call(
    {send, #dbus_message{fds = [_ | _]}},
    _From,
    #state{agree_unix_fd = false} = State
) ->
    {reply, {error, unix_fd_not_negotiated}, State};
handle_call(
    {send, Message},
    _From,
    #state{
        transport = Conn,
        serial = Serial
    } = State
) ->
    Message1 = dbus_message:set_serial(Serial, Message),
    State1 = incr_serial(State),

    Data = dbus_marshaller:marshal_message(Message1),
    case dbus_transport:send(Conn, Data, Message1#dbus_message.fds) of
        ok ->
            {reply, {ok, Serial}, State1};
        {error, Reason} ->
            {reply, {error, Reason}, State1}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% Descriptors on a connection that never negotiated them: the peer is not
%% speaking the protocol it agreed to, so the connection stops rather than
%% carrying on with a queue it has no rule for.
handle_info(
    {data, _Data, [_ | _] = Fds, From},
    #state{reader = From, agree_unix_fd = false} = State
) ->
    discard_fds(Fds, unix_fd_not_negotiated),
    {stop, {unexpected_fds, length(Fds)}, State};
handle_info(
    {data, Data, Fds, From},
    #state{reader = From, acc = Acc, fds = Queue} = State
) ->
    Bin = <<Acc/binary, Data/binary>>,
    Queue1 = Queue ++ Fds,
    case dbus_marshaller:unmarshal_data(Bin, Queue1) of
        {ok, Messages, Rest, Queue2} ->
            lists:foreach(
                fun(Msg) ->
                    State#state.owner ! {dbus, Msg}
                end,
                Messages
            ),
            {noreply, State#state{acc = Rest, fds = Queue2}};
        %% Either the bytes are short or the descriptors are: both are in
        %% flight and the next `recv' completes the message.
        more ->
            {noreply, State#state{acc = Bin, fds = Queue1}};
        {error, Reason} ->
            %% Nothing was framed, so nothing claimed a descriptor: the whole
            %% queue is undeliverable.
            discard_fds(Queue1, {unmarshal_error, Reason}),
            {stop, {unmarshal_error, Reason}, State#state{fds = []}}
    end;
handle_info(
    {'DOWN', _Ref, process, Owner, _Reason},
    #state{owner = Owner} = State
) ->
    {stop, owner_down, State};
handle_info({'EXIT', Reader, Reason}, #state{reader = Reader} = State) ->
    {stop, {reader_exit, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{fds = Fds}) ->
    discard_fds(Fds, {connection_terminated, Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Private
%%%
try_connect([], _State) ->
    {stop, no_addresses};
try_connect([Address | Addresses], State) ->
    case dbus_transport:connect(Address) of
        {ok, Conn} ->
            ?LOG_INFO("Successfully connected to ~p", [Address]),
            handle_auth(State#state{transport = Conn});
        {error, Reason} ->
            ?LOG_ERROR("Failed to connect to ~p: ~p", [Address, Reason]),
            try_connect(Addresses, State)
    end.

handle_auth(#state{transport = Conn} = State) ->
    % As of spec, client must send nul byte right after connecting and before
    % authentication
    dbus_transport:send(Conn, <<0>>),

    case dbus_auth_client_mech:try_auth(State#state.auth_ctx, Conn) of
        {ok, Auth} ->
            ?LOG_INFO("Authentication successful, server guid: ~p", [Auth#dbus_auth.guid]),
            handle_begin(Auth, State);
        {error, Reason} ->
            ?LOG_ERROR("Authentication failed: ~p", [Reason]),
            {stop, {auth_error, Reason}}
    end.

handle_begin(
    #dbus_auth{guid = Guid, agree_unix_fd = AgreeUnixFd},
    #state{transport = Conn} = State
) ->
    Reader = start_reader(Conn),
    State1 = State#state{
        reader = Reader,
        guid = Guid,
        agree_unix_fd = AgreeUnixFd
    },
    {ok, State1}.

%% What the owner never received, closed here. `m:dbus_fd' is a NIF because
%% OTP has no `close(2)': `socket:close/1' closes a socket, and what D-Bus
%% carries is usually a file, a pipe or a `memfd'.
discard_fds([], _Reason) ->
    ok;
discard_fds(Fds, Reason) ->
    ?LOG_WARNING("Discarding ~b unclaimed file descriptor(s): ~p", [length(Fds), Reason]),
    lists:foreach(fun close_fd/1, Fds).

close_fd(Fd) ->
    case dbus_fd:close(Fd) of
        ok ->
            ok;
        {error, Posix} ->
            ?LOG_WARNING("Failed to close file descriptor ~b: ~p", [Fd, Posix]),
            ok
    end.

incr_serial(#state{serial = Serial} = State) ->
    State#state{serial = next_serial(Serial)}.

next_serial(16#FFFFFFFF) -> 1;
next_serial(N) -> N + 1.

start_reader(Conn) ->
    Self = self(),
    spawn_link(fun() ->
        reader_loop(Conn, Self)
    end).

reader_loop(Conn, Parent) ->
    case dbus_transport:recv(Conn, infinity) of
        %% Bytes and descriptors both go on untouched: taking the descriptors
        %% a message declares off the front of the queue needs the framing,
        %% and the framing is the connection's.
        {ok, Data, Fds} ->
            Parent ! {data, Data, Fds, self()},
            reader_loop(Conn, Parent);
        {error, Reason} ->
            %% Handle error
            exit({recv_error, Reason})
    end.

server_opts(Props) ->
    lists:filter(
        fun({Key, _Value}) ->
            lists:member(Key, ?SERVER_OPTS_KEYS)
        end,
        Props
    ).

%%%
%%% Tests
%%%
-ifdef(TEST).

%%% What these pin, which is the framing contract rather than the socket:
%%%
%%%   * a `recv' is not a message in either direction -- bytes accumulate in
%%%     `#state.acc' and descriptors in `#state.fds', and a message is framed
%%%     when both are complete;
%%%   * the queue is in arrival order and a message takes what it declares off
%%%     the front, so several messages in one `recv' each get their own;
%%%   * every path where nobody takes delivery closes the descriptors.

%% A message whose body is a single `h' pointing at the first descriptor. Its
%% `UNIX_FDS' field is synthesised from `fds' while marshalling.
fd_message(Fds) ->
    #dbus_message{
        header = #dbus_header{type = ?TYPE_METHOD_CALL, serial = 1},
        body = {[unix_fd], [0]},
        fds = Fds
    }.

%% The owner and the reader are both the test process: what the connection
%% sends the owner lands in the test's own mailbox.
negotiated_state() ->
    #state{owner = self(), reader = self(), agree_unix_fd = true}.

data(Data, Fds) ->
    {data, Data, Fds, self()}.

descriptors_reach_the_owner_test() ->
    Bin = dbus_marshaller:marshal_message(fd_message([3, 4])),
    {noreply, State} = handle_info(data(Bin, [7, 8]), negotiated_state()),
    ?assertMatch([#dbus_message{fds = [7, 8]}], delivered()),
    ?assertEqual({<<>>, []}, {State#state.acc, State#state.fds}).

%% The descriptors are attached to the segment they arrived on, which is not
%% the segment the message ends on.
bytes_split_across_two_recvs_test() ->
    Bin = dbus_marshaller:marshal_message(fd_message([3])),
    <<Head:16/binary, Tail/binary>> = Bin,
    {noreply, State1} = handle_info(data(Head, [7]), negotiated_state()),
    ?assertEqual([], delivered()),
    ?assertEqual({Head, [7]}, {State1#state.acc, State1#state.fds}),
    {noreply, State2} = handle_info(data(Tail, []), State1),
    ?assertMatch([#dbus_message{fds = [7]}], delivered()),
    ?assertEqual({<<>>, []}, {State2#state.acc, State2#state.fds}).

%% All the bytes and not all the descriptors: `unmarshal_data/2' answers
%% `more' and the message waits, rather than being framed with a short list.
descriptors_still_in_flight_test() ->
    Bin = dbus_marshaller:marshal_message(fd_message([3, 4])),
    {noreply, State1} = handle_info(data(Bin, [7]), negotiated_state()),
    ?assertEqual([], delivered()),
    ?assertEqual({Bin, [7]}, {State1#state.acc, State1#state.fds}),
    {noreply, State2} = handle_info(data(<<>>, [8]), State1),
    ?assertMatch([#dbus_message{fds = [7, 8]}], delivered()),
    ?assertEqual({<<>>, []}, {State2#state.acc, State2#state.fds}).

%% Two messages in one `sendmsg': the descriptors belong to the one that
%% declares them, whatever order the peer packed them in.
two_messages_one_recv_test() ->
    First = dbus_marshaller:marshal_message(fd_message([])),
    Second = dbus_marshaller:marshal_message(fd_message([3, 4])),
    Recv = data(<<First/binary, Second/binary>>, [7, 8]),
    {noreply, State} = handle_info(Recv, negotiated_state()),
    ?assertMatch(
        [#dbus_message{fds = []}, #dbus_message{fds = [7, 8]}],
        delivered()
    ),
    ?assertEqual({<<>>, []}, {State#state.acc, State#state.fds}).

%% A leftover descriptor is a peer's, not a bug here: it belongs to a message
%% whose bytes have not all arrived.
unclaimed_descriptors_stay_queued_test() ->
    Bin = dbus_marshaller:marshal_message(fd_message([3])),
    {noreply, State} = handle_info(data(Bin, [7, 8]), negotiated_state()),
    ?assertMatch([#dbus_message{fds = [7]}], delivered()),
    ?assertEqual([8], State#state.fds).

unnegotiated_descriptors_stop_the_connection_test() ->
    with_fds(2, fun unnegotiated_descriptors_stop_the_connection/1).

unnegotiated_descriptors_stop_the_connection(Fds) ->
    State = (negotiated_state())#state{agree_unix_fd = false},
    ?assertMatch(
        {stop, {unexpected_fds, 2}, _},
        handle_info(data(<<"anything">>, Fds), State)
    ),
    assert_closed(Fds).

%% Nothing was framed, so no message claimed a descriptor and the whole queue
%% is undeliverable.
unmarshal_failure_closes_the_queue_test() ->
    with_fds(1, fun unmarshal_failure_closes_the_queue/1).

unmarshal_failure_closes_the_queue(Fds) ->
    %% Sixteen bytes so the header is read rather than awaited, and a protocol
    %% version no header has.
    Bin = <<$l, ?TYPE_METHOD_CALL, 0, 99, 0:32, 1:32, 0:32>>,
    {stop, {unmarshal_error, bad_header}, State} = handle_info(
        data(Bin, Fds), negotiated_state()
    ),
    ?assertEqual([], State#state.fds),
    assert_closed(Fds).

terminate_closes_the_queue_test() ->
    with_fds(2, fun terminate_closes_the_queue/1).

terminate_closes_the_queue(Fds) ->
    ?assertEqual(ok, terminate(normal, (negotiated_state())#state{fds = Fds})),
    assert_closed(Fds).

%% Refused before the transport is reached -- which `transport = undefined'
%% is what asserts: a send that got as far as `dbus_transport:send/3' would
%% fail on the record.
sending_descriptors_unnegotiated_test() ->
    State = #state{owner = self(), agree_unix_fd = false, serial = 1},
    ?assertEqual(
        {reply, {error, unix_fd_not_negotiated}, State},
        handle_call({send, fd_message([3])}, {self(), tag}, State)
    ).

%%%
%%% Helpers
%%%
delivered() ->
    receive
        {dbus, Msg} -> [Msg | delivered()]
    after 0 -> []
    end.

%% A socket is the one way to obtain a descriptor from Erlang; the copies
%% `dbus_fd:dup/1' makes are owned by the test and may be given to code that
%% closes them.
with_fds(N, Fun) ->
    {ok, Sock} = socket:open(inet, stream, tcp),
    try
        {ok, Fd} = socket:getopt(Sock, {otp, fd}),
        Fun([dup(Fd) || _ <- lists:seq(1, N)])
    after
        _ = socket:close(Sock)
    end.

dup(Fd) ->
    {ok, Copy} = dbus_fd:dup(Fd),
    Copy.

%% Nothing here opens a descriptor between the close under test and this
%% check, so the number cannot have been handed out again in between.
assert_closed(Fds) ->
    [?assertEqual({error, ebadf}, dbus_fd:close(Fd)) || Fd <- Fds].

-endif.
