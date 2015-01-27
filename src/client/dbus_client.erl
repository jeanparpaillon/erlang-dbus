%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_client).

-behaviour(gen_server).

-include("dbus_client.hrl").

-export([start_link/4,
	 state/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {bus,
		handler,
		manager,
		objects}).

-callback init(Bus :: dbus_bus_conn(), Env :: [dbus_client_env()]) ->
    {ok, State :: any()} | {error, term()}.

-callback signal(Path :: dbus_name(), 
		 IfaceName :: dbus_name(), 
		 Signal :: dbus_name(), 
		 Args :: [dbus_arg()],
		 State :: any()) ->
    {ok, State2 :: any()} | {error, term()}.

% @doc Start dbus client
%
% @end
%
-spec start_link(BusName :: dbus_bus_name(), Handler :: dbus_client_handler(), Opts :: [dbus_client_opt()], Env :: any()) -> 
			{ok, pid()} | ignore | {error, term()}.
start_link(BusName, Handler, Opts, Env) when is_atom(Handler) ->
    gen_server:start_link(?MODULE, [BusName, {Handler, undefined}, Opts, Env], []).

state(Ref) ->
    gen_server:call(Ref, state).

%%%
%%% gen_server callbacks
%%%
init([BusName, Handler, Opts, Env]) ->
    {ok, Bus} = dbus:connect(BusName),
    case init_manager(Bus, proplists:get_value(manager, Opts)) of
	{ok, Mgr, InitState} ->
	    case do_call(Handler, init, [Bus, [{env, Env} | InitState]]) of
		{ok, Handler2} ->
		    {ok, #state{bus=Bus, manager=Mgr, handler=Handler2}};
		{error, Err} ->
		    {stop, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(state, _From, #state{handler={_Mod, HandlerState}}=State) ->
    {reply, HandlerState, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({dbus_signal, #dbus_message{body=Args}=Msg}, #state{handler=Handler}=State) ->
    IfaceName = dbus_message:find_field(?FIELD_INTERFACE, Msg),
    Signal = dbus_message:find_field(?FIELD_MEMBER, Msg),
    case IfaceName of
	'org.freedesktop.DBus' ->
	    handle_dbus_signal(Signal, Args, State);
	IfaceName ->
	    Path = dbus_message:find_field(?FIELD_PATH, Msg),
	    case may_call(Handler, signal, [Path, IfaceName, Signal, Args]) of
		{ok, Handler2} ->
		    {noreply, State#state{handler=Handler2}};
		ignore ->
		    {noreply, State};
		{error, Err} ->
		    {stop, Err, State}
	    end
    end;

handle_info(_Info, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%
%%% Priv
%%%
do_call({Mod, _}, init, Args) ->
    try erlang:apply(Mod, init, Args) of
	{ok, State} ->  {ok, {Mod, State}};
	{error, Err} -> {error, Err}
    catch 
	error:undef ->  error(undef);
	_:Err ->        {error, Err}
    end;    
do_call({Mod, State}, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args ++ [State]) of
	{ok, State2} ->  {ok, {Mod, State2}};
	{error, Err} ->  {error, Err}
    catch 
	error:undef ->   error(undef);
	_:Err ->         {error, Err}
    end.


may_call({Handler, State}, Fun, Args) ->
    try do_call({Handler, State}, Fun, Args ++ [State])
    catch error:undef -> ignore
    end.


handle_dbus_signal('NameAcquired', Name, State) ->
    ?debug("Name acquired: ~p~n", [Name]),
    {noreply, State};

handle_dbus_signal(Signal, Args, State) ->
    ?debug("DBus signal: ~p(~p)~n", [Signal, Args]),
    {noreply, State}.


init_manager(_, undefined) ->
    {ok, undefined, []};
init_manager(Bus, Service) ->
    case dbus_bus_connection:get_objects_manager(Bus, Service) of
	{ok, Manager} ->
	    {ok, Objects} = dbus_proxy:get_managed_objects(Manager),
	    {ok, Manager, [{objects, Objects}]};
	{error, Err} -> 
	    {error, Err}
    end.
