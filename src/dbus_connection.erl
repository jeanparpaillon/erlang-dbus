-module(dbus_connection).
-moduledoc """
Handles connection to a D-Bus peer.
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

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
    auth_ctx :: term() | undefined,
    transport :: dbus_transport:connection() | undefined,
    uuid = <<>> :: binary(),
    serial = 1 :: dbus_serial(),
    reader :: pid() | undefined,
    acc = <<>> :: binary()
}).

-type connection() :: pid().
-type option() ::
    %% Overrides detected transport from address, mainly for testing
    {transport, atom()}
    %% Per-mechanism context, key is module name
    | {auth_ctx, map()}.

-spec start_link(dbus_address() | [dbus_address()]) -> gen_server:start_ret().
start_link(Addresses) when is_list(Addresses) ->
    start_link(Addresses, []);
start_link(Address) ->
    start_link([Address], []).

-spec start_link(dbus_address() | [dbus_address()], [option()]) -> gen_server:start_ret().
start_link(Addresses, Options) when is_list(Addresses) ->
    Owner = self(),
    StartArgs = #{
        owner => Owner,
        addresses => Addresses,
        auth_ctx => proplists:get_value(auth_ctx, Options, #{})
    },
    gen_server:start_link(?MODULE, StartArgs, []).

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
        owner := Owner,
        addresses := Addresses,
        auth_ctx := AuthCtx
    } = _StartArgs
) ->
    State = #state{owner = Owner, auth_ctx = AuthCtx},
    try_connect(Addresses, State).

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
        transport = Conn,
        serial = Serial
    } = State
) ->
    Message1 = dbus_message:set_serial(Serial, Message),
    State1 = incr_serial(State),

    Data = dbus_marshaller:marshal_message(Message1),
    case dbus_transport:send(Conn, Data) of
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
        {ok, Resp} ->
            ?LOG_INFO("Authentication successful, server guid: ~p", [Resp]),
            handle_begin(State#state{uuid = Resp});
        {error, Reason} ->
            ?LOG_ERROR("Authentication failed: ~p", [Reason]),
            {stop, {auth_error, Reason}}
    end.

handle_begin(#state{transport = Conn} = State) ->
    Reader = start_reader(Conn),
    {ok, State#state{reader = Reader}}.

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
        {ok, Data} ->
            Parent ! {data, Data, self()},
            reader_loop(Conn, Parent);
        {error, Reason} ->
            %% Handle error
            exit({recv_error, Reason})
    end.
