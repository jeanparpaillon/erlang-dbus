%%
%% @copyright 2014 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc DBUS transport implementation on UNIX socket
%%
%% Support UNIX fd passing.
%% @todo use OTP mechanism when available, see <a href="https://github.com/erlang/otp/pull/612" >https://github.com/erlang/otp/pull/612</a>
%% @end

-module(dbus_transport_unix).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([connect/2]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% Internal
-export([do_read/2]).

-record(state, {sock, 
		owner,
		loop     :: pid(),
		raw      :: boolean()}).

-define(DEFAULT_PATH, "/tmp/erlang-dbus").

connect(BusOptions, _Options) ->
    Path = case proplists:get_value(path, BusOptions) of
	       undefined ->
                   throw(no_path);
	       V ->
		   list_to_binary(V)
	   end,
    gen_server:start_link(?MODULE, [Path, self()],[]).

%%
%% gen_server callbacks
%%
init([Path, Owner]) when is_pid(Owner), is_binary(Path) ->
    true = link(Owner),
    ?debug("Connecting to UNIX socket: ~p~n", [Path]),
    {ok, S} = prim_inet:open(tcp, local, stream),
    {ok, Fd} = prim_inet:getfd(S),
    case gen_tcp:connect({local, Path}, 0, [{fd, Fd}]) of
	{ok, Sock} ->
            Loop = spawn_link(?MODULE, do_read, [Sock, self()]),
            gen_tcp:controlling_process(Sock, Loop),
            {ok, #state{sock=Sock, owner=Owner, loop=Loop}};
	{error, Err} ->
	    ?error("Error creating socket: ~p~n", [Err]),
	    {error, Err}
    end;
init(_) ->
    ?error("Invalid argument in UNIX transport init~n", []),
    {error, invalid_argument}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(support_unix_fd, _From, State) ->
    {reply, true, State};

handle_call({set_raw, Raw}, _From, State) ->
    {reply, ok, State#state{raw=Raw}};

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({send, Data}, State) when is_list(Data) ->
    handle_cast({send, iolist_to_binary(Data)}, State);

handle_cast({send, Data}, #state{sock=Sock}=State) when is_binary(Data) ->
    ?debug("unix send(~p)~n", [Data]),
    gen_tcp:send(Sock, Data),
    {noreply, State};

handle_cast(close, State) ->
    {stop, normal, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({unix, Data}, #state{owner=Owner}=State) ->
    ?debug("unix received(~p)~n", [Data]),
    Owner ! {received, Data},
    {noreply, State};

handle_info({'EXIT', _From, _Reason}, #state{owner=Owner}=State) ->
    ?error("Listener has died, who will listen ?~n", []),
    Owner ! closed,
    {stop, normal, State#state{sock=undefined}};

handle_info(Info, State) ->
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{sock=Sock, loop=Loop}) ->
    case Sock of
	undefined -> ignore;
	_ -> 
	    exit(Loop, kill),
	    %% Avoid do_read loop polling on closed fd
	    timer:sleep(100),
            gen_tcp:close(Sock)
    end,
    ok.

%%%
%%% Priv
%%%
do_read(Sock, Pid) ->
    receive
        {tcp, Sock, Buf} ->
            Pid ! {unix, list_to_binary(Buf)},
            do_read(Sock, Pid);
        Unhandled ->
            ?debug("Unhandled receive: ~p", [Unhandled]),
            do_read(Sock, Pid)
    end.
