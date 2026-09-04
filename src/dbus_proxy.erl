-module(dbus_proxy).
-moduledoc """
This module defines a proxy to a D-Bus object
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-callback init(dbus_connection:connection(), term()) -> {ok, term()} | {error, term()}.
-callback handle_dbus(dbus_message(), State) ->
    {reply, dbus_message(), State}
    | {noreply, State}.

-export([
    start_link/4,
    stop/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type server_option() :: {server_name, gen_server:server_name()}.

-record(state, {
    conn :: dbus_connection:connection(),
    cb_mod = undefined :: module() | undefined,
    cb_state = undefined :: term() | undefined
}).

-type proxy() :: pid().
-export_type([proxy/0]).

-doc """
Start proxy
""".
-spec start_link(module(), term(), dbus_connection:connection(), [server_option()]) ->
    gen_server:start_ret().
start_link(CbMod, CbArgs, Conn, Opts) ->
    Args = [CbMod, CbArgs, Conn],
    case server_name(Opts) of
        undefined ->
            gen_server:start_link(?MODULE, Args, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Args, [])
    end.

-spec stop(proxy()) -> ok.
stop(Proxy) ->
    gen_server:stop(Proxy).

%%%
%%% gen_server callbacks
%%%
init([CbMod, CbArgs, Conn]) ->
    ok = dbus_connection:subscribe(Conn),
    case CbMod:init(Conn, CbArgs) of
        {ok, CbState} ->
            {ok, #state{conn = Conn, cb_mod = CbMod, cb_state = CbState}};
        {error, Reason} ->
            {stop, {callback_init, Reason}}
    end.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({dbus, Conn, Type, Message}, #state{conn = Conn} = State) ->
    handle_callback(Type, Message, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Priv
%%%
server_name(Props) ->
    proplists:get_value(server_name, Props, undefined).

handle_callback(signal, Signal, State) ->
    CbMod = State#state.cb_mod,
    CbState = State#state.cb_state,
    {noreply, CbState1} = CbMod:handle_dbus(Signal, CbState),
    {noreply, State#state{cb_state = CbState1}};
handle_callback(method_call, Call, State) ->
    CbMod = State#state.cb_mod,
    CbState = State#state.cb_state,
    case CbMod:handle_dbus(Call, CbState) of
        {noreply, CbState1} ->
            {noreply, State#state{cb_state = CbState1}};
        {reply, Message, CbState1} ->
            ok = dbus_connection:send(State#state.conn, Message),
            {noreply, State#state{cb_state = CbState1}};
        {error, Reason} ->
            ?LOG_ERROR("Error handling method call ~p: ~p", [Call, Reason]),
            {noreply, State}
    end;
handle_callback(method_return, Return, State) ->
    CbMod = State#state.cb_mod,
    CbState = State#state.cb_state,
    {noreply, CbState1} = CbMod:handle_dbus(Return, CbState),
    {noreply, State#state{cb_state = CbState1}};
handle_callback(error, Error, State) ->
    CbMod = State#state.cb_mod,
    CbState = State#state.cb_state,
    {noreply, CbState1} = CbMod:handle_dbus(Error, CbState),
    {noreply, State#state{cb_state = CbState1}}.
