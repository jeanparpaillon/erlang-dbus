-module(dbus_auth_client_mech).
-moduledoc """
Defines the behaviour for authentication client mechanisms.
""".
-include("dbus.hrl").

-type state() :: term().

-callback name() -> binary().

%% Called with this mechanism's own entry of the auth context map, or
%% `undefined' when the context has none for it.
-callback init(term()) ->
    {ok, state()} | {error, term()}.

-callback initial_response(state()) ->
    {continue, binary(), state()}
    | {ok, binary(), state()}
    | {none, state()}
    | {error, term()}.

-callback challenge(binary(), state()) ->
    {continue, binary(), state()}
    | {ok, binary(), state()}
    | {error, term()}.

-record(state, {
    ctx :: map(),
    transport :: dbus_transport:connection(),
    %% Bytes read from the transport but not yet consumed as a line.
    buf = <<>> :: binary(),
    supported_mechs = undefined :: [binary()] | undefined,
    mech :: module() | undefined,
    mech_state :: term() | undefined,
    guid :: binary() | undefined,
    agree_unix_fd = false :: boolean()
}).

-define(RECV_TIMEOUT, 1000).

%% The specification caps an authentication line at 16384 bytes, CRLF
%% included; a longer one is not a line we are still waiting to complete.
-define(MAX_LINE, 16384).

-export([try_auth/2]).

-spec try_auth(map(), dbus_transport:connection()) ->
    {ok, dbus_auth()} | {error, term()}.
try_auth(Ctx, Conn) ->
    State = #state{
        ctx = Ctx,
        transport = Conn
    },

    ok = do_send(dbus_sasl:command_auth(), State),
    do_auth(waiting_for_reject, do_recv(State)).

%%%
%%% Priv
%%%
do_auth(waiting_for_ok, {{ok, Response}, State}) ->
    handle_ok(Response, State);
do_auth(waiting_for_ok, {{rejected, _}, State}) ->
    handle_rejected(State);
do_auth(waiting_for_ok, {{data, _}, State}) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State));
do_auth(waiting_for_ok, {{error, _Msg}, State}) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State));
do_auth(waiting_for_data, {{data, Challenge}, State}) ->
    Mech = State#state.mech,
    MechState = State#state.mech_state,
    case Mech:challenge(Challenge, MechState) of
        {ok, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_data(Response), State1),
            do_auth(waiting_for_ok, do_recv(State1));
        {continue, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_data(Response), State1),
            do_auth(waiting_for_data, do_recv(State1));
        {error, Reason} ->
            ok = do_send(dbus_sasl:command_error(Reason), State),
            do_auth(waiting_for_ok, do_recv(State))
    end;
do_auth(waiting_for_data, {{rejected, _}, State}) ->
    handle_rejected(State);
do_auth(waiting_for_data, {{error, _Msg}, State}) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State));
do_auth(waiting_for_data, {{ok, Response}, State}) ->
    handle_ok(Response, State);
do_auth(
    waiting_for_reject,
    {{rejected, Mechanisms}, State = #state{supported_mechs = undefined}}
) ->
    handle_rejected(State#state{supported_mechs = Mechanisms});
do_auth(waiting_for_reject, {{rejected, _Mechanisms}, State}) ->
    % Server is supposed sending always same supported mechanisms
    handle_rejected(State);
do_auth(waiting_for_reject, {_Ret, _State}) ->
    {error, {unexpected_response, waiting_for_reject}};
do_auth(waiting_for_unix_fd, {agree_unix_fd, State}) ->
    handle_begin(State#state{agree_unix_fd = true});
do_auth(waiting_for_unix_fd, {{error, _}, State}) ->
    handle_begin(State#state{agree_unix_fd = false});
do_auth(_, {{transport_error, Reason}, _State}) ->
    {error, {transport_error, Reason}};
%% Not an unknown command to be answered with ERROR: the stream is no longer
%% framable, so there is nothing to resynchronise on.
do_auth(_, {{protocol_error, Reason}, _State}) ->
    {error, {protocol_error, Reason}};
% catch all clause
do_auth(_, {_Response, State}) ->
    ok = do_send(dbus_sasl:command_error(), State),
    do_auth(waiting_for_ok, do_recv(State)).

handle_rejected(State) ->
    Mechanisms = State#state.supported_mechs,
    case next_mechanism(Mechanisms) of
        not_found ->
            {error, {no_valid_auth_mechanism, Mechanisms}};
        {Mech, OtherMechs} ->
            case Mech:init(maps:get(Mech, State#state.ctx, undefined)) of
                {ok, MechState} ->
                    State1 = State#state{
                        mech = Mech, mech_state = MechState, supported_mechs = OtherMechs
                    },
                    handle_auth_init(State1);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

handle_auth_init(State) ->
    Mech = State#state.mech,
    MechState = State#state.mech_state,
    MechName = Mech:name(),
    case Mech:initial_response(MechState) of
        {ok, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_auth(MechName, Response), State1),
            do_auth(waiting_for_ok, do_recv(State1));
        {continue, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_auth(MechName, Response), State1),
            do_auth(waiting_for_data, do_recv(State1));
        {none, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_auth(MechName), State1),
            do_auth(waiting_for_ok, do_recv(State1));
        {error, Reason} ->
            {error, Reason}
    end.

handle_ok(<<>>, State) ->
    Conn = State#state.transport,

    case dbus_transport:support_unix_fd(Conn) of
        true ->
            ok = do_send(dbus_sasl:command_negotiate_unix_fd(), State),
            do_auth(waiting_for_unix_fd, do_recv(State));
        false ->
            handle_begin(State)
    end;
handle_ok(Response, State) ->
    handle_ok(<<>>, State#state{guid = Response}).

handle_begin(State) ->
    ok = do_send(dbus_sasl:command_begin(), State),
    {ok, #dbus_auth{
        guid = State#state.guid,
        agree_unix_fd = State#state.agree_unix_fd
    }}.

next_mechanism([]) ->
    not_found;
next_mechanism([MechName | OtherMechs]) ->
    case lookup_mechanism(MechName) of
        undefined ->
            next_mechanism(OtherMechs);
        Mech ->
            {Mech, OtherMechs}
    end.

lookup_mechanism(<<"EXTERNAL">>) -> dbus_auth_external;
lookup_mechanism(<<"DBUS_COOKIE_SHA1">>) -> dbus_auth_cookie_sha1;
lookup_mechanism(<<"ANONYMOUS">>) -> dbus_auth_anonymous;
lookup_mechanism(_) -> undefined.

do_send(Data, State) ->
    dbus_transport:send(State#state.transport, Data).

%% `dbus_transport:recv/2' does no framing: it hands back whatever bytes have
%% arrived, which may be half a line, exactly one, or several at once. The
%% authentication protocol is line-based -- every line ends with CRLF -- so
%% the framing is ours, and what follows the line we take stays in the buffer
%% for the next call rather than being dropped.
do_recv(#state{buf = Buf} = State) ->
    case take_line(Buf) of
        {ok, Line, Rest} ->
            {dbus_sasl:parse(Line), State#state{buf = Rest}};
        more when byte_size(Buf) >= ?MAX_LINE ->
            {{protocol_error, line_too_long}, State};
        more ->
            case dbus_transport:recv(State#state.transport, ?RECV_TIMEOUT) of
                %% No descriptors here: authentication is a line protocol, and
                %% the server has not even been asked to allow them yet.
                {ok, Data, _Fds} ->
                    do_recv(State#state{buf = <<Buf/binary, Data/binary>>});
                {error, Reason} ->
                    {{transport_error, Reason}, State}
            end
    end.

%% Splits off the first line, CRLF included -- `dbus_sasl:parse/1' takes a
%% whole line and rejects anything trailing it.
take_line(Buf) ->
    case binary:match(Buf, <<"\r\n">>) of
        nomatch ->
            more;
        {Pos, 2} ->
            Len = Pos + 2,
            <<Line:Len/binary, Rest/binary>> = Buf,
            {ok, Line, Rest}
    end.
