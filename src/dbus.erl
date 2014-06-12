%%%
%%% @doc       D-BUS application module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(dbus).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% helpers
-export([get_system_bus/0, get_session_bus/0]).

%% application callbacks
start(normal, []) ->
    error_logger:logfile({open, "dbus.log"}),
    dbus_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec start() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start top supervisor of the Yate application
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(crypto),
    application:start(sasl),
    application:start(xmerl),
    application:start(dbus).

get_system_bus() ->
    dbus_bus_reg:get_bus(dbus_bus:get_bus_id(system)).

get_session_bus() ->
    dbus_bus_reg:get_bus(dbus_bus:get_bus_id(session)).
