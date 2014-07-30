%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_client).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include_lib("dbus/include/dbus_client.hrl").

-export([start_link/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {bus,
		handler,
		manager,
		objects}).

-callback init(Bus :: dbus_bus_conn(), Env :: any()) ->
    {ok, State :: any()} | {error, term()}.

% @doc Start dbus client
%
% @end
%
-spec start_link(BusName :: dbus_bus_name(), Handler :: atom(), Opts :: [dbus_client_opt()], Env :: any()) -> 
			{ok, pid()} | ignore | {error, term()}.
start_link(BusName, Handler, Opts, Env) ->
    gen_server:start_link(?MODULE, [BusName, Handler, Opts, Env], []).

%%%
%%% gen_server callbacks
%%%
init([BusName, Handler, Opts, Env]) ->
    {ok, Bus} = dbus:connect(BusName),
    case do_call(Handler, init, [Bus, Env]) of
	{ok, S} ->
	    case proplists:get_value(manager, Opts) of
		undefined ->
		    {ok, #state{bus=Bus, handler={Handler, S}}};
		Service ->
		    case dbus_bus_connection:get_objects_manager(Bus, Service) of
			{ok, Manager} ->
			    {ok, Objects} = dbus_proxy:get_managed_objects(Manager),
			    case may_call(Handler, add_objects, [Objects, S]) of
				{ok, S2} ->
				    {ok, #state{bus=Bus, manager=Manager, handler={Handler, S2}}};
				ignore ->
				    {ok, #state{bus=Bus, manager=Manager, handler={Handler, S}}};
				{error, Err} ->
				    {stop, Err}
			    end;
			{error, Err} ->
			    {stop, Err}
		    end
	    end;
	{error, Err} ->
	    {stop, Err}
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
handle_info({dbus_signal, #dbus_message{body=Args}=Msg}, #state{handler={Mod, S}}=State) ->
    IfaceName = dbus_message:find_field(?FIELD_INTERFACE, Msg),
    Signal = dbus_message:find_field(?FIELD_MEMBER, Msg),
    case IfaceName of
	'org.freedesktop.DBus' ->
	    handle_dbus_signal(Signal, Args, State);
	IfaceName ->
	    Path = dbus_message:find_field(?FIELD_PATH, Msg),
	    case may_call(Mod, signal, [Path, IfaceName, Signal, Args, S]) of
		{ok, S2} ->
		    {noreply, State#state{handler={Mod, S2}}};
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
do_call(Mod, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args)
    catch
	_:Err ->
	    {error, Err}
    end.


may_call(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
	true ->
	    try erlang:apply(Mod, Fun, Args)
	    catch
		_:Err ->
		    {error, Err}
	    end;
	false ->
	    ignore
    end.


handle_dbus_signal('NameAcquired', Name, State) ->
    lager:debug("Name acquired: ~p~n", Name),
    {noreply, State};
handle_dbus_signal(Signal, Args, State) ->
    lager:debug("DBus signal: ~p(~p)~n", [Signal, Args]),
    {noreply, State}.
