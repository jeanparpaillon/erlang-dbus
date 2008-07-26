%%%-------------------------------------------------------------------
%%% File    : dbus_demo_sup.erl
%%% Author  : Mikael Magnusson <mikael@skinner.hem.za.org>
%%% Description : 
%%%
%%% Created : 29 Oct 2006 by Mikael Magnusson <mikael@skinner.hem.za.org>
%%%-------------------------------------------------------------------
-module(dbus_demo_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link([]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Hello = {dbus_demo, {dbus_demo, start_link, ['org.za.hem.DBus', '/Root']},
	     permanent, 10000, worker, [dbus_demo]},
    Dbus = {dbus,{dbus,start_link,[]}, permanent, 10000, worker, [dbus]},
    {ok,{{one_for_one,10,60}, [Hello, Dbus]}}.

%%====================================================================
%% Internal functions
%%====================================================================
