-module(dbus_bus).
-moduledoc """
Holds a D-Bus bus object state and functions to Bus interface
""".
-include_lib("kernel/include/logger.hrl").

-export([start_link/2]).

-export([
    init/1,
    handle_dbus_signal/2,
    handle_dbus_call/2,
    handle_dbus_return/2,
    handle_dbus_error/2
]).

-spec start_link(dbus_connection:connection(), atom()) -> gen_server:start_ret().
start_link(Conn, Name) ->
    Opts = [{server_name, {local, Name}}],
    dbus_proxy:start_link(?MODULE, Conn, Opts).

-spec init(term()) -> {ok, term()} | {error, term()}.
init(_Args) ->
    {ok, #{}}.

-spec handle_dbus_signal(dbus_signal:t(), term()) -> {noreply, term()}.
handle_dbus_signal(Signal, State) ->
    ?LOG_INFO("Received signal ~p", [Signal]),
    {noreply, State}.

-spec handle_dbus_call(dbus_method_call:t(), term()) -> {noreply, term()}.
handle_dbus_call(Call, State) ->
    ?LOG_INFO("Received call ~p", [Call]),
    {noreply, State}.

-spec handle_dbus_return(dbus_method_return:t(), term()) -> {noreply, term()}.
handle_dbus_return(Return, State) ->
    ?LOG_INFO("Received return ~p", [Return]),
    {noreply, State}.

-spec handle_dbus_error(dbus_error:t(), term()) -> {noreply, term()}.
handle_dbus_error(Error, State) ->
    ?LOG_INFO("Received error ~p", [Error]),
    {noreply, State}.
