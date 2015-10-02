%%
%% @copyright 2014 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc DBUS transport implementation on UNIX socket
%%
%% Messages
%% {received, Pid, Data}
%% {closed, Pid}

-module(dbus_transport_unix).

-behaviour(gen_server).

-include("dbus.hrl").
-include_lib("procket/include/procket.hrl").

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
		loop     :: pid(),
		raw      :: boolean()}).

connect(BusOptions, _Options) ->
    Path = case proplists:get_value(path, BusOptions) of
	       undefined ->
		   case proplists:get_value(abstract, BusOptions) of
		       undefined ->
			   throw(no_path);
		       V ->
			   {abstract, list_to_binary(V)}
		   end;
	       V ->
		   {path, list_to_binary(V)}
	   end,
    gen_server:start_link(?MODULE, [Path, self()],[]).

%%
%% gen_server callbacks
%%
init([{Mode, Path}, Owner]) when is_pid(Owner), is_binary(Path) ->
    true = link(Owner),
    ?debug("Connecting to UNIX socket: ~p~n", [Path]),
    case procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0) of
	{ok, Sock} ->
	    SockUn = get_sock_un(Mode, Path),
	    case procket:connect(Sock, SockUn) of
		ok ->
		    process_flag(trap_exit, true),
		    PollID = inert:start(),
		    Loop = spawn_link(?MODULE, do_read, [PollID, Sock, self()]),
		    {ok, #state{sock=Sock, owner=Owner, loop=Loop}};
		{error, Err} ->
		    ?error("Error connecting socket: ~p~n", [Err]),
		    {error, Err}
	    end;
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
    procket:sendto(Sock, Data, 0, <<>>),
    {noreply, State};

handle_cast(close, State) ->
    {stop, normal, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({unix, Data}, #state{owner=Owner}=State) ->
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
	    % Avoid do_read loop polling on closed fd
	    timer:sleep(100),
	    procket:close(Sock)
    end,
    ok.

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

get_sock_un(abstract, Path) when byte_size(Path) < ?UNIX_PATH_MAX-1 ->
    Len = byte_size(Path),
    <<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
      0, Path/binary>>;
get_sock_un(path, Path) when byte_size(Path) < ?UNIX_PATH_MAX ->
    Len = byte_size(Path),
    <<(procket:sockaddr_common(?PF_LOCAL, Len))/binary,
      Path/binary, 
      0:((procket:unix_path_max()-Len)*8)>>.
