%%%-------------------------------------------------------------------
%%% File    : dbus_demo_sup.erl
%%% Author  : Mikael Magnusson <mikma@users.sourceforge.net>
%%% Description : 
%%%
%%% Created : 29 Oct 2006 by Mikael Magnusson <mikma@users.sourceforge.net>
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
    Hello = {dbus_demo_hello, {dbus_demo_hello, start_link,
			       ["com.example.SampleService",
                                '/SomeObject']},
	     permanent, 10000, worker, [dbus_demo_hello]},
    Dbus = {dbus_demo,{dbus_demo,start_link,[]},
	    permanent, 10000, worker, [dbus_demo]},
    {ok,{{one_for_one,10,60}, [Hello, Dbus]}}.

%%====================================================================
%% Internal functions
%%====================================================================
