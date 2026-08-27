-module(dbus).
-moduledoc "D-BUS application module.".

-include("dbus_client.hrl").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).

start(normal, []) ->
    dbus_sup:start_link().

stop(_State) ->
    ok.

-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    application:ensure_all_started(dbus).
