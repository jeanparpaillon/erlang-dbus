%%%
%%% @copyright 2006 Mikael Magnusson, 2014-2016 Jean Parpaillon
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @author    Jean Parpaillon <jean.parpaillon@free.fr>
%%% @doc       D-BUS application module
%%%
%%% @end
-module(dbus).

-include("dbus_client.hrl").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).

%% @doc
%% @end
start(normal, []) ->
    dbus_sup:start_link().

stop(_State) ->
    ok.

%% @doc
%% @end
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    application:ensure_all_started(dbus).
