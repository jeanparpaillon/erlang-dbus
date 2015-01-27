%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_client).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("dbus_client.hrl").

-export([start_link/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
-spec start_link(BusName :: dbus_bus_name(), Handler :: dbus_client_handler(), Opts :: [dbus_client_opt()], Env :: any()) -> 
			{ok, pid()} | ignore | {error, term()}.
start_link(BusName, Handler, Opts, Env) when is_atom(Handler) ->
    gen_server:start_link(?MODULE, [BusName, {mod, Handler, undefined}, Opts, Env], []);

start_link(BusName, {gen_server, Handler}, Opts, Env) when is_atom(Handler) ->
    gen_server:start_link(?MODULE, [BusName, {gen_server, Handler}, Opts, Env], []);

start_link(BusName, Handler, Opts, Env) when is_pid(Handler) ->
    gen_server:start_link(?MODULE, [BusName, Handler, Opts, Env], []).

%%%
%%% gen_server callbacks
%%%
init([BusName, Handler, Opts, Env]) ->
    {ok, Bus} = dbus:connect(BusName),
    case do_call(Handler, init, [Bus, Env]) of
	{ok, Handler2} ->
	    case proplists:get_value(manager, Opts) of
		undefined ->
		    {ok, #state{bus=Bus, handler=Handler2}};
		Service ->
		    case dbus_bus_connection:get_objects_manager(Bus, Service) of
			{ok, Manager} ->
			    {ok, Objects} = dbus_proxy:get_managed_objects(Manager),
			    case may_call(Handler2, add_objects, [Objects]) of
				{ok, Handler3} ->
				    {ok, #state{bus=Bus, manager=Manager, handler=Handler3}};
				ignore ->
				    {ok, #state{bus=Bus, manager=Manager, handler=Handler2}};
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
do_call({mod, Handler, State}, Fun, Args) ->
    do_call_fun({mod, Handler, State}, Fun, Args);

do_call({gen_server, Ref}, Fun, Args) ->
    do_call_srv({gen_server, Ref}, Fun, Args);

do_call(Pid, Fun, Args) when is_pid(Pid) ->
    do_call_pid(Pid, Fun, Args).


do_call_fun({mod, Mod, undefined}, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args) of
	{ok, State} ->
	    {ok, {mod, Mod, State}};
	{error, Err} ->
	    {error, Err}
    catch 
	error:undef ->
	    error(undef);
	_:Err ->
	    {error, Err}
    end;


do_call_fun({mod, Mod, State}, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args ++ [State]) of
	{ok, State2} ->
	    {ok, {mod, Mod, State2}};
	{error, Err} ->
	    {error, Err}
    catch _:Err ->
	    {error, Err}
    end.


do_call_srv({gen_server, Mod}, init, Args) ->
    case gen_server:start_link(Mod, Args, []) of
	{ok, Pid} -> {ok, {gen_server, Pid}};
	ignore -> error(undef);
	{error, Err} -> {error, Err}
    end;
do_call_srv(Handler, Fun, Args) ->
    case may_call_srv(Handler, Fun, Args) of
	ignore ->
	    error(undef);
	Ret -> Ret
    end.


do_call_pid(Pid, Fun, Args) ->
    may_call_pid(Pid, Fun, Args).


may_call({mod, Handler, State}, Fun, Args) when is_atom(Handler) ->
    may_call_fun({mod, Handler, State}, Fun, Args);

may_call({gen_server, Ref}, Fun, Args) ->
    may_call_srv({gen_server, Ref}, Fun, Args).

may_call_fun({mod, Mod, State}, Fun, Args) ->
    try do_call_fun({mod, Mod, State}, Fun, Args)
    catch 
	error:undef ->
	    ignore
    end.


may_call_srv({gen_server, Ref}, Fun, Args) ->
    case gen_server:call(Ref, {Fun, Args}) of
	ok ->
	    {ok, {gen_server, Ref}};
	ignore ->
	    ignore;
	{error, Err} ->
	    {error, Err}
    end.


may_call_pid(Pid, Fun, Args) when is_pid(Pid) ->
    Pid ! list_to_tuple(lists:flatten([Fun, Args])).


handle_dbus_signal('NameAcquired', Name, State) ->
    lager:debug("Name acquired: ~p~n", Name),
    {noreply, State};

handle_dbus_signal(Signal, Args, State) ->
    lager:debug("DBus signal: ~p(~p)~n", [Signal, Args]),
    {noreply, State}.
