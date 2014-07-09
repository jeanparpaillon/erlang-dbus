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

% Needed for spawn_link
-export([do_read/3]).

-record(state, {sock, 
		owner,
		raw      :: boolean()}).

-define(AF_UNIX, 1).
-define(SOCK_STREAM, 1).
-define(BACKLOG, 5).

connect(BusOptions, _Options) ->
    Path = case proplists:get_value(path, BusOptions) of
	       undefined ->
		   case proplists:get_value(abstract, BusOptions) of
		       undefined ->
			   throw(no_path);
		       V ->
			   lager:debug("DBUS socket: {abstract, ~p}~n", [V]),
			   list_to_binary([0 | V])
		   end;
	       V ->
		   lager:debug("DBUS socket: {path, ~p}~n", [V]),
		   list_to_binary(V)
	   end,
    gen_server:start_link(?MODULE, [Path, self()],[]).

%%
%% gen_server callbacks
%%
init([Path, Owner]) when is_pid(Owner), is_binary(Path) ->
    true = link(Owner),
    lager:debug("Connecting to UNIX socket: ~p~n", [Path]),
    case procket:socket(?AF_UNIX, ?SOCK_STREAM, 0) of
	{ok, Sock} ->
	    SockUn = <<?AF_UNIX:16/native,
		       Path/binary,
		       0:((procket:unix_path_max()-byte_size(Path))*8)>>,
	    case procket:connect(Sock, SockUn) of
		ok ->
		    process_flag(trap_exit, true),
		    PollID = inert:start(),
		    spawn_link(?MODULE, do_read, [PollID, Sock, self()]),
		    {ok, #state{sock=Sock, owner=Owner}};
		{error, Err} ->
		    lager:error("Error connecting socket: ~p~n", [Err]),
		    {error, Err}
	    end;
	{error, Err} ->
	    lager:error("Error creating socket: ~p~n", [Err]),
	    {error, Err}
    end;
init(_) ->
    lager:error("Invalid argument in UNIX transport init~n", []),
    {error, invalid_argument}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(support_unix_fd, _From, State) ->
    {reply, true, State};

handle_call({set_raw, Raw}, _From, State) ->
    {reply, ok, State#state{raw=Raw}};

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({send, Data}, State) when is_list(Data) ->
    handle_cast({send, iolist_to_binary(Data)}, State);

handle_cast({send, Data}, #state{sock=Sock}=State) when is_binary(Data) ->
    lager:debug("### Sending on DBUS: ~p~n", [Data]),
    procket:sendto(Sock, Data, 0, <<>>),
    {noreply, State};

handle_cast(close, #state{sock=Sock}=State) ->
    procket:close(Sock),
    {stop, normal, State#state{sock=undefined}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    lager:error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({unix, Data}, #state{owner=Owner}=State) ->
    Owner ! {received, Data},
    {noreply, State};

handle_info({'EXIT', _From, _Reason}, #state{owner=Owner}=State) ->
    lager:error("Listener has died, who will listen ?~n", []),
    Owner ! closed,
    {stop, normal, State#state{sock=undefined}};

handle_info(Info, State) ->
    lager:error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, #state{sock=Sock}) ->
    case Sock of
	    undefined -> ignore;
	    _ -> procket:close(Sock)
    end,
        terminated.

%%%
%%% Priv
%%%
do_read(PollID, Sock, Pid) ->
    case procket:recvfrom(Sock, 16#FFFF) of
	{error, eagain} ->
	    ok = inert:poll(PollID, Sock),
	    do_read(PollID, Sock, Pid);
	{error, _Err} ->
	    ok;
	% EOF
	{ok, <<>>} ->
	    ok;
	{ok, Buf} ->
	    Pid ! {unix, Buf},
	    do_read(PollID, Sock, Pid)
    end.
