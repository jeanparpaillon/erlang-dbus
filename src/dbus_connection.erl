-module(dbus_connection).
-moduledoc """
Handles connection to a D-Bus peer.
""".
-include("dbus.hrl").

-behaviour(gen_server).

-export([
    start_link/1,
    start_link/2,
    stop/1,
    get_uuid/1,
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
    address :: dbus_address(),
    transport :: module(),
    transport_state = undefined :: dbus_transport:connection() | undefined,
    uuid = <<>> :: binary(),
    serial = 1 :: dbus_serial(),
    reader = undefined :: pid() | undefined,
    acc = <<>> :: binary()
}).

-type connection() :: pid().
-type option() ::
    %% Overrides detected transport from address, mainly for testing
    {transport, atom()}
    %% Per-mechanism context, key is module name
    | {auth_ctx, map()}.

-spec start_link(dbus_address()) -> gen_server:start_ret().
start_link(Address) ->
    start_link(Address, []).

-spec start_link(dbus_address(), [option()]) -> gen_server:start_ret().
start_link(Address, Options) ->
    Ret =
        case proplists:get_value(transport, Options, undefined) of
            undefined ->
                case dbus_transport:resolve(Address) of
                    {ok, Transport} ->
                        {ok, Transport};
                    {error, undefined} ->
                        {error, {invalid_transport, Address}}
                end;
            Transport ->
                {ok, Transport}
        end,

    case Ret of
        {ok, Transport1} ->
            Owner = self(),
            StartArgs = #{
                owner => Owner,
                address => Address,
                transport => Transport1,
                auth_ctx => proplists:get_value(auth_ctx, Options, #{})
            },
            gen_server:start_link(?MODULE, StartArgs, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(connection()) ->
    ok.
stop(Connection) ->
    gen_server:stop(Connection).

-doc """
Returns peer GUID
""".
-spec get_uuid(connection()) ->
    {ok, binary()}.
get_uuid(Connection) ->
    gen_server:call(Connection, get_uuid).

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
        transport := Transport,
        owner := Owner,
        address := Address,
        auth_ctx := AuthCtx
    } = _StartArgs
) ->
    case dbus_transport:connect(Transport, Address) of
        {ok, Conn} ->
            State = #state{
                owner = Owner,
                address = Address,
                uuid = <<>>,
                transport_state = Conn
            },

            handle_auth(State, AuthCtx);
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_uuid, _From, #state{uuid = UUID} = State) ->
    {reply, {ok, UUID}, State};
handle_call({set_owner, Owner}, {Owner, _Tag}, #state{owner = Owner} = State) ->
    {reply, ok, State#state{owner = Owner}};
handle_call({set_owner, _Owner}, _From, State) ->
    {reply, {error, forbidden}, State};
handle_call(
    {send, Message},
    _From,
    #state{
        transport_state = TransportState,
        transport = Transport,
        serial = Serial
    } = State
) ->
    Message1 = dbus_message:set_serial(Serial, Message),
    State1 = incr_serial(State),

    Data = dbus_marshaller:marshal_message(Message1),
    case dbus_transport:send(Transport, TransportState, Data) of
        ok ->
            {reply, {ok, State1#state.serial}, State1};
        {error, Reason} ->
            {reply, {error, Reason}, State1}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(
    {data, Data, From},
    #state{reader = From, acc = Acc} = State
) ->
    Bin = <<Acc/binary, Data/binary>>,
    case dbus_marshaller:unmarshal_data(Bin) of
        {ok, Messages, Rest} ->
            lists:foreach(
                fun(Msg) ->
                    State#state.owner ! {dbus, Msg}
                end,
                Messages
            ),
            {noreply, State#state{acc = Rest}};
        {error, Reason} ->
            %% Handle unmarshalling error
            {stop, {unmarshal_error, Reason}, State}
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Private
%%%
handle_auth(State, AuthCtx) ->
    T = State#state.transport,
    TS = State#state.transport_state,

    ok = dbus_transport:set_mode(T, TS, line),

    % As of spec, client must send nul byte right after connecting and before
    % authentication
    dbus_transport:send(T, TS, <<0>>),

    case dbus_auth_client_mech:try_auth(AuthCtx, T, TS) of
        {ok, Resp, TS1} ->
            handle_begin(State#state{
                uuid = Resp,
                transport_state = TS1
            });
        {error, Reason} ->
            {stop, {auth_error, Reason}}
    end.

handle_begin(State) ->
    T = State#state.transport,
    TS = State#state.transport_state,

    ok = dbus_transport:set_mode(T, TS, raw),
    Reader = start_reader(T, TS),

    {ok, State#state{reader = Reader}}.

incr_serial(#state{serial = Serial} = State) ->
    State#state{serial = next_serial(Serial)}.

next_serial(16#FFFFFFFF) -> 1;
next_serial(N) -> N + 1.

start_reader(Transport, TransportState) ->
    Conn = self(),
    spawn_link(fun() ->
        reader_loop(Transport, TransportState, Conn)
    end).

reader_loop(Transport, TransportState, Conn) ->
    case dbus_transport:recv(Transport, TransportState, infinity) of
        {ok, Data} ->
            Conn ! {data, Data, self()},
            reader_loop(Transport, TransportState, Conn);
        {error, Reason} ->
            %% Handle error
            exit({recv_error, Reason})
    end.
