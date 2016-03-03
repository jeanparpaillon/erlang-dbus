%%
%% @copyright 2006 Mikael Magnusson
%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc       Top supervisor for dbus application
%% @end
-module(dbus_sup).

-behaviour(supervisor).

%% api
-export([
	 start_link/0
	]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).


%% @doc Start supervisor
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    %%Dbus = {dbus,{dbus,start_link,[]}, permanent, 10000, worker, [dbus]},
    BusReg = {dbus_bus_reg,{dbus_bus_reg,start_link,[]}, permanent, 10000, worker, [dbus_bus_reg]},
    ServiceReg = {dbus_service_reg,{dbus_service_reg,start_link,[]}, permanent, 10000, worker, [dbus_service_reg]},
    {ok, {{one_for_one, 10, 60}, [BusReg, ServiceReg]}}.
