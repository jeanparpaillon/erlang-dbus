-module(dbus_bus).
-moduledoc """
dbus_bus is the first class application : proxies bus interfaces
""".
-include_lib("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    start_link/2,
    request_name/2,
    request_name/3
]).

-export([
    init/2,
    handle_dbus/2,
    handle_call/3
]).

-define(PATH, <<"/org/freedesktop/DBus">>).
-define(INTERFACE, <<"org.freedesktop.DBus">>).
-define(SERVICE_DBUS, <<"org.freedesktop.DBus">>).
-define(MEMBER_HELLO, <<"Hello">>).
-define(MEMBER_NAME_ACQUIRED, <<"NameAcquired">>).
-define(MEMBER_REQUEST_NAME, <<"RequestName">>).

-type request_name_opt() ::
    allow_replacement
    | replace_existing
    | do_not_queue.

-type request_name_ret() ::
    primary_owner
    | in_queue
    | exists
    | already_owner.

-record(state, {
    conn :: dbus_connection:connection(),
    unique_name :: binary() | undefined,
    acquired = [] :: [binary()]
}).

-spec start_link(dbus_connection:connection() | atom(), gen_server:server_ref()) ->
    gen_server:start_ret().
start_link(Conn, Ref) ->
    Opts = [{server_ref, Ref}],
    dbus_proxy:start_link(?MODULE, [], Conn, Opts).

-spec request_name(dbus_proxy:proxy(), binary()) ->
    {ok, request_name_ret()}
    | {error, term()}.
request_name(Proxy, Name) ->
    request_name(Proxy, Name, []).

-spec request_name(dbus_proxy:proxy(), binary(), [request_name_opt()]) ->
    {ok, request_name_ret()}
    | {error, term()}.
request_name(Proxy, Name, Opts) ->
    dbus_proxy:call(Proxy, {request_name, Name, Opts}).

%%%
%%% Callbacks
%%% Callbacks for handling D-Bus messages and initialization
init(Conn, _Args) ->
    Hello = dbus_method_call:build(
        ?MEMBER_HELLO,
        ?PATH,
        [],
        [
            {interface, ?INTERFACE},
            {destination, ?SERVICE_DBUS}
        ]
    ),
    case dbus_rpc:call(Conn, Hello) of
        {ok, Name} when is_binary(Name) ->
            ?LOG_INFO("Acquired bus name ~p", [Name]),
            {ok, #state{conn = Conn, unique_name = Name}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to acquire bus name: ~p", [Reason]),
            {error, Reason}
    end.

handle_dbus(Message, State) ->
    Interface = dbus_message:find_field(?FIELD_INTERFACE, Message),
    Member = dbus_message:find_field(?FIELD_MEMBER, Message),
    do_handle(Interface, Member, Message, State).

handle_call({request_name, Name, _Opts}, _From, State) ->
    Args = {[string, uint32], [Name, 0]},
    Request = dbus_method_call:build(
        ?MEMBER_REQUEST_NAME,
        ?PATH,
        Args,
        [
            {interface, ?INTERFACE},
            {destination, ?SERVICE_DBUS}
        ]
    ),

    Ret = dbus_rpc:call(State#state.conn, Request),
    {reply, Ret, State}.

do_handle(?INTERFACE, ?MEMBER_NAME_ACQUIRED, Message, State) ->
    case dbus_message:get_body(Message) of
        <<":", _Rest/binary>> ->
            % Unique name, already stored as `unique_name`
            {noreply, State};
        Name ->
            State1 = State#state{acquired = [Name | State#state.acquired]},
            {noreply, State1}
    end;
do_handle(_Interface, _Member, Message, State) ->
    ?LOG_INFO("Received message ~p", [Message]),
    {noreply, State}.
