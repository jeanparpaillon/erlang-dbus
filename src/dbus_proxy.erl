-module(dbus_proxy).
-moduledoc """
This module defines a proxy to a D-Bus object
""".
-include("dbus.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-callback init(term()) -> {ok, term()} | {error, term()}.
-callback handle_dbus_signal(dbus_signal:t(), State) -> {noreply, State}.
-callback handle_dbus_call(dbus_method_call:t(), State) ->
    {reply, dbus_method_return:t(), State}
    | {noreply, State}.
-callback handle_dbus_return(dbus_method_return:t(), State) ->
    {noreply, State}.
-callback handle_dbus_error(dbus_error:t(), State) ->
    {noreply, State}.

-export([
    start_link/2,
    start_link/3,
    method_call/2,
    method_call/6,
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
    callers :: ets:table(),
    % Maybe replace with pubsub mechanism ? (`pg`)
    owner = undefined :: pid() | undefined,
    callback_mod = undefined :: module() | undefined,
    callback_state = undefined :: term() | undefined
}).

-type proxy() :: pid().
-export_type([proxy/0]).

-doc """
Start proxy, linked mode.

On receiving an unexpected message, it is delivered to the process owner (the
one that started the proxy), as : `{dbus}`
""".
-spec start_link(dbus_connection:connection(), [server_option()]) -> gen_server:start_ret().
start_link(Conn, Opts) ->
    Args = #{
        conn => Conn,
        owner => self()
    },
    do_start_link(Args, server_name(Opts)).

-doc """
Start proxy, callback mode

On receiving an unexpected message, `handle_message` callback is called.
""".
-spec start_link(module(), dbus_connection:connection(), [server_option()]) ->
    gen_server:start_ret().
start_link(CallbackMod, Conn, Opts) ->
    Args = #{
        conn => Conn,
        callback_mod => CallbackMod,
        callback_opts => Opts
    },
    do_start_link(Args, server_name(Opts)).

-doc """
Implements a D-Bus method call

Returns
  * `{ok, Result}` on success, where `Result` is the parsed reply.
  * `{error, Reason}` on failure, where `Reason` is the parsed error.

In case of connection error, the function throws an error.
""".
-spec method_call(
    Proxy :: proxy(),
    MethodName :: binary(),
    ObjectPath :: binary(),
    Signature :: dbus_signature(),
    InArgs :: term(),
    Options :: [dbus_message:method_opt()]
) ->
    {ok, term()}
    % No reply is expected from called object
    | ok
    | {error, term()}.
method_call(Proxy, MethodName, ObjectPath, Signature, InArgs, Options) ->
    {ok, Call} = dbus_method_call:build(MethodName, ObjectPath, Signature, InArgs, Options),
    method_call(Proxy, Call).

-spec method_call(proxy(), dbus_method_call:t()) ->
    {ok, term()}
    | ok
    | {error, term()}.
method_call(Proxy, Call) ->
    gen_server:call(Proxy, {method_call, Call}).

-spec stop(proxy()) -> ok.
stop(Proxy) ->
    gen_server:stop(Proxy).

%%%
%%% gen_server callbacks
%%%
init(#{conn := Conn} = Args) ->
    Owner = maps:get(owner, Args),
    CallbackMod = maps:get(callback_mod, Args),
    Callers = ets:new(callers, [set, protected, named_table]),
    ok = dbus_connection:subscribe(Conn),

    State = #state{
        conn = Conn,
        callers = Callers,
        owner = Owner,
        callback_mod = CallbackMod,
        callback_state = undefined
    },
    case CallbackMod of
        undefined ->
            {ok, State};
        _ ->
            case CallbackMod:init() of
                {ok, State1} ->
                    {ok, State1};
                {error, Reason} ->
                    {stop, {callback_init, Reason}}
            end
    end.

handle_call({method_call, Call}, From, State) ->
    Message = dbus_method_call:to_message(Call),
    case dbus_connection:send(State#state.conn, Message) of
        {ok, Serial} ->
            case dbus_method_call:no_reply_expected(Call) of
                true ->
                    {reply, ok, State};
                false ->
                    true = ets:insert(State#state.callers, {Serial, From}),
                    {noreply, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({dbus, Conn, Message}, #state{conn = Conn} = State) ->
    handle_dbus_message(dbus_message:get_type(Message), Message, State);
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

do_start_link(Args, undefined) ->
    gen_server:start_link(?MODULE, Args, []);
do_start_link(Args, Name) ->
    gen_server:start_link(Name, ?MODULE, Args, []).

handle_dbus_message(Type, Message, State) when
    Type =:= method_return orelse Type =:= error
->
    Serial = dbus_message:get_serial(Message),
    case ets:lookup(State#state.callers, Serial) of
        [{Serial, From}] ->
            true = ets:delete(State#state.callers, Serial),
            Ret = dbus_message:cast(Message),
            handle_call_reply(Type, From, Ret);
        [] ->
            handle_unexpected_message(Type, Message, State)
    end,
    {noreply, State};
%% A signal, or a call addressed to an object we export, is never a reply to
%% something we sent: it goes straight to the owner or to the callback module,
%% which is what `handle_dbus_call/2' is for.
handle_dbus_message(Type, Message, State) when
    Type =:= signal orelse Type =:= method_call
->
    handle_unexpected_message(Type, Message, State).

handle_unexpected_message(Type, Message, #state{owner = Owner} = State) when is_pid(Owner) ->
    Ret = dbus_message:cast(Message),
    ?LOG_DEBUG("Received message ~p", [Ret]),
    Owner ! {dbus, Type, Ret},
    {noreply, State};
handle_unexpected_message(Type, Message, State) ->
    Ret = dbus_message:cast(Message),
    Mod = State#state.callback_mod,
    do_handle_callback(Mod, Type, Ret, State).

handle_call_reply(method_return, From, Ret) ->
    gen_server:reply(From, {ok, Ret});
handle_call_reply(error, From, Ret) ->
    gen_server:reply(From, {error, Ret}).

do_handle_callback(Mod, signal, Signal, State) ->
    {noreply, NewCallbackState} = Mod:handle_dbus_signal(Signal, State#state.callback_state),
    {noreply, State#state{callback_state = NewCallbackState}};
do_handle_callback(Mod, method_call, Call, State) ->
    case Mod:handle_dbus_call(Call, State#state.callback_state) of
        {noreply, NewCallbackState} ->
            {noreply, State#state{callback_state = NewCallbackState}};
        {reply, Return, NewCallbackState} ->
            Message = dbus_method_return:to_message(Return),
            {ok, _} = dbus_connection:send(State#state.conn, Message),
            {noreply, State#state{callback_state = NewCallbackState}};
        {error, Reason} ->
            ?LOG_ERROR("Error handling method call ~p: ~p", [Call, Reason]),
            {noreply, State}
    end;
do_handle_callback(Mod, method_return, Return, State) ->
    {noreply, NewCallbackState} = Mod:handle_dbus_return(Return, State#state.callback_state),
    {noreply, State#state{callback_state = NewCallbackState}};
do_handle_callback(Mod, error, Error, State) ->
    {noreply, NewCallbackState} = Mod:handle_dbus_error(Error, State#state.callback_state),
    {noreply, State#state{callback_state = NewCallbackState}}.
