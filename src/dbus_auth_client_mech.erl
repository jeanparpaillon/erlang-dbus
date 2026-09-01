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
    transport :: module(),
    transport_state :: term(),
    supported_mechs = undefined :: [binary()] | undefined,
    mech :: module() | undefined,
    mech_state :: term() | undefined
}).

-export([try_auth/3]).

-spec try_auth(map(), module(), term()) ->
    {ok, binary(), term()} | {error, term()}.
try_auth(Ctx, Transport, TransportState) ->
    State = #state{
        ctx = Ctx,
        transport = Transport,
        transport_state = TransportState
    },

    ok = do_send(dbus_sasl:command_auth(), State),
    do_auth(waiting_for_reject, do_recv(State), State).

%%%
%%% Priv
%%%
do_auth(waiting_for_ok, {ok, Response}, State) ->
    handle_ok(Response, State);
do_auth(waiting_for_ok, {rejected, _}, State) ->
    handle_rejected(State);
do_auth(waiting_for_ok, {data, _}, State) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State), State);
do_auth(waiting_for_ok, {error, _Msg}, State) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State), State);
do_auth(waiting_for_data, {data, Challenge}, State) ->
    Mech = State#state.mech,
    MechState = State#state.mech_state,
    case Mech:challenge(Challenge, MechState) of
        {ok, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_data(Response), State1),
            do_auth(waiting_for_ok, do_recv(State1), State1);
        {continue, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_data(Response), State1),
            do_auth(waiting_for_data, do_recv(State1), State1);
        {error, Reason} ->
            ok = do_send(dbus_sasl:command_error(Reason), State),
            do_auth(waiting_for_ok, do_recv(State), State)
    end;
do_auth(waiting_for_data, {rejected, _}, State) ->
    handle_rejected(State);
do_auth(waiting_for_data, {error, _Msg}, State) ->
    ok = do_send(dbus_sasl:command_cancel(), State),
    do_auth(waiting_for_reject, do_recv(State), State);
do_auth(waiting_for_data, {ok, Response}, State) ->
    handle_ok(Response, State);
do_auth(
    waiting_for_reject,
    {rejected, Mechanisms},
    #state{supported_mechs = undefined} = State
) ->
    handle_rejected(State#state{supported_mechs = Mechanisms});
do_auth(waiting_for_reject, {rejected, _Mechanisms}, State) ->
    % Server is supposed sending always same supported mechanisms
    handle_rejected(State);
do_auth(waiting_for_reject, _, _State) ->
    {error, {unexpected_response, waiting_for_reject}};
do_auth(waiting_for_unix_fd, agree_unix_fd, State) ->
    handle_begin(State, <<>>);
do_auth(waiting_for_unix_fd, {error, _}, State) ->
    Transport = State#state.transport,
    TransportState = State#state.transport_state,
    ok = dbus_transport:disable_unix_fd(Transport, TransportState),
    handle_begin(State, <<>>);
do_auth(_, {transport_error, Reason}, _State) ->
    {error, {transport_error, Reason}};
% catch all clause
do_auth(_, _, State) ->
    ok = do_send(dbus_sasl:command_error(), State),
    do_auth(waiting_for_ok, do_recv(State), State).

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
            do_auth(waiting_for_ok, do_recv(State1), State1);
        {continue, Response, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_auth(MechName, Response), State1),
            do_auth(waiting_for_data, do_recv(State1), State1);
        {none, NewMechState} ->
            State1 = State#state{mech_state = NewMechState},
            ok = do_send(dbus_sasl:command_auth(MechName), State1),
            do_auth(waiting_for_ok, do_recv(State1), State1);
        {error, Reason} ->
            {error, Reason}
    end.

handle_ok(Response, State) ->
    Transport = State#state.transport,
    TransportState = State#state.transport_state,

    case dbus_transport:support_unix_fd(Transport, TransportState) of
        true ->
            ok = do_send(dbus_sasl:command_negotiate_unix_fd(), State),
            do_auth(waiting_for_unix_fd, do_recv(State), State);
        false ->
            handle_begin(State, Response)
    end.

handle_begin(State, Response) ->
    ok = do_send(dbus_sasl:command_begin(), State),
    {ok, Response, State#state.transport_state}.

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
    dbus_transport:send(State#state.transport, State#state.transport_state, Data).

do_recv(State) ->
    case dbus_transport:recv(State#state.transport, State#state.transport_state, 1000) of
        {ok, Data} ->
            dbus_sasl:parse(Data);
        {error, Reason} ->
            {transport_error, Reason}
    end.
