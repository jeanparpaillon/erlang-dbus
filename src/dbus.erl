%%%
%%% @doc       D-BUS application module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @author    Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright 2006 Mikael Magnusson
%%% @copyright 2014 Jean Parpaillon
%%%
-module(dbus).
-compile({parse_transform, lager_transform}).

-include("dbus_client.hrl").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% helpers
-export([connect/1]).

%% application callbacks
start(normal, []) ->
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
    application:ensure_all_started(dbus).

-spec connect(dbus_bus_name()) -> {ok, dbus_bus_conn()} | {error, term()}.
connect(BusName) ->
    dbus_bus_connection:connect(BusName).
