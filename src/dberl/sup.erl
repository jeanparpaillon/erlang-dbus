%%%
%%% @doc       Top supervisor
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(dberl.sup).

-import(dbus).
-import(error_logger).
-import(supervisor).

-behaviour(supervisor).

%% api
-export([
	 start_link/0
	]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).


%%--------------------------------------------------------------------
%% @spec start_link() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Dbus = {dberl,{dbus,start_link,[]}, permanent, 10000, worker, [dbus]},
    BusReg = {bus_reg,{dberl.bus_reg,start_link,[]}, permanent, 10000, worker, [bus_reg]},
    ServiceReg = {service_reg,{dberl.service_reg,start_link,[]}, permanent, 10000, worker, [service_reg]},
    
    {ok, {{one_for_one, 1, 60}, [BusReg, ServiceReg, Dbus]}}.
