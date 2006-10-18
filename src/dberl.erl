%%%
%%% @doc       D-BUS applictation module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(dberl).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0]).

%% application callbacks
start(normal, []) ->
    error_logger:logfile({open, "dberl.log"}),
    dberl.sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec start() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start top supervisor of the Yate application
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(dberl).
