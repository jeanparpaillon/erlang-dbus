%%
%% @copyright 2006 Mikael Magnusson
%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc       Top supervisor
%%
-module(dbus_sup).

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
%%     Dbus = {dberl,{dbus,start_link,[]}, permanent, 10000, worker, [dbus]},
    BusReg = {bus_reg,{dberl.bus_reg,start_link,[]}, permanent, 10000, worker, [bus_reg]},
    ServiceReg = {service_reg,{dberl.service_reg,start_link,[]}, permanent, 10000, worker, [service_reg]},
%%     Hello = {hello, {hello, start_link, ['org.za.hem.DBus', '/Root']}, permanent, 10000, worker, [hello]},
    {ok, {{one_for_one, 10, 60}, [BusReg, ServiceReg]}}.
