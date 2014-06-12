%%
%% @copyright 2014 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc DBUS transport implementation on UNIX socket
%%
%% Messages
%% {received, Pid, Data}
%% {closed, Pid}

-module(dbus_transport_unix).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% api
-export([connect/2]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  sock,
	  owner
	 }).

-define(IS_SERVER, 1).
-define(IS_ABSTRACT, 2).
-define(IS_NULLTERM, 4).

connect(BusOptions, Options) ->
    
    {_Flags, Path} =
	case lists:keysearch(path, 1, BusOptions) of
	    {value, {_, Path1}} ->
		{?IS_NULLTERM, Path1};
	    _ ->
		case lists:keysearch(abstract, 1, BusOptions) of
		    {value, {_, Path2}} ->
			{?IS_ABSTRACT bor ?IS_NULLTERM, Path2};
		    _ ->
			throw(no_path)
		end
	end,
    gen_server:start_link(?MODULE, [Path, Options, self()],[]).

%%
%% gen_server callbacks
%%
init([Path, _Options, Owner]) when is_pid(Owner) ->
    true = link(Owner),
    uds_server:start_link(),
    try uds:connect(Path) of
	{ok, Sock} ->
	    uds:set_active(Sock, once),
	    {ok, #state{sock=Sock, owner=Owner}};
	{error, Err} ->
	    lager:error("Error opening socket: ~p~n", [Err]),
	    {error, Err}
    catch 
	exit:Err -> 
	    lager:error("Error opening socket: ~p~n", [Err]),
	    {error, Err}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setopts, _Options}, _From, State) ->
    % TODO ?
    {reply, ok, State};

handle_call({change_owner, OldPid, NewPid}, _From, #state{owner=OldPid}=State) when is_pid(NewPid) ->
    true = link(NewPid),
    true = unlink(OldPid),
    {reply, ok, State#state{owner=NewPid}};

handle_call({change_owner, _OldPid, _NewPid}, _From, State) ->
    {reply, error, State};

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({send, Data}, #state{sock=Sock}=State) ->
    uds:send(Sock, Data),
    {noreply, State};

handle_cast(close, #state{sock=Sock}=State) ->
    uds:close(Sock),
    {stop, normal, State#state{sock=undefined}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    lager:error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({unix, Sock, Data}, #state{sock=Sock, owner=Owner}=State) ->
    Owner ! {received, self(), Data},
    ok = uds:set_active(Sock, once),
    {noreply, State};

handle_info({unix_closed, Sock}, #state{sock=Sock, owner=Owner}=State) ->
    Owner ! {closed, self()},
    {stop, normal, State#state{sock=undefined}};

handle_info(Info, State) ->
    lager:error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Sock = State#state.sock,
    case Sock of
	undefined -> ignore;
	_ -> uds:close(Sock)
    end,
    terminated.
