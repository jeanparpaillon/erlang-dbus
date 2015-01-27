%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc tcp connection gen_server
%%
%% Messages
%% {received, Pid, Data}
%% {closed, Pid}

-module(dbus_transport_tcp).

-include("dbus.hrl").

-behaviour(gen_server).


%% api
-export([connect/3]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {sock, owner}).

connect(Host, Port, Options) ->
    gen_server:start_link(?MODULE, [Host, Port, Options, self()], []).

%%
%% gen_server callbacks
%%
init([Host, Port, Options, Owner]) ->
    true = link(Owner),
    case gen_tcp:connect(Host, Port, Options) of
	{ok, Sock} ->
	    ok = inet:setopts(Sock, [{keepalive, true},
				     {active, once}, 
				     binary]),
	    {ok, #state{sock=Sock, owner=Owner}};
	{error, Err} ->
	    ?error("Error opening socket: ~p~n", [Err]),
	    {error, Err}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({set_raw, true}, _From, #state{sock=Sock}=State) ->
    ok = inet:setopts(Sock, [{packet, raw}]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({send, Data}, #state{sock=Sock}=State) ->
    gen_tcp:send(Sock, Data),
    {noreply, State};

handle_cast(close, State) ->
    ok = gen_tcp:close(State#state.sock),
    {stop, normal, State#state{sock=undefined}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({tcp, Sock, Data}, #state{sock=Sock, owner=Owner}=State) ->
    Owner ! {received, Data},
    ok = inet:setopts(Sock, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Sock}, #state{sock=Sock, owner=Owner}=State) ->
    Owner ! closed,
    {stop, normal, State};

handle_info(Info, State) ->
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, #state{sock=Sock}) ->
    case Sock of
	undefined -> ignore;
	_ -> gen_tcp:close(Sock)
    end,
    terminated.
