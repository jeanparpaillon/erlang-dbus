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
-export([do_read/2]).

-record(state, {sock, owner}).

-define(PF_LOCAL, 1).
-define(UNIX_PATH_MAX, 108).
-define(BACKLOG, 5).

connect(BusOptions, _Options) ->
    Path = case proplists:get_value(path, BusOptions) of
	       undefined ->
		   case proplists:get_value(abstract, BusOptions) of
		       undefined ->
			   throw(no_path);
		       V ->
			   lager:debug("DBUS socket: {abstract, ~p}~n", [V]),
			   {abstract, list_to_binary(V)}
		   end;
	       V ->
		   lager:debug("DBUS socket: {path, ~p}~n", [V]),
		   {path, list_to_binary(V)}
	   end,
    gen_server:start_link(?MODULE, [Path, self()],[]).

%%
%% gen_server callbacks
%%
init([{Mode, Path}, Owner]) when is_pid(Owner), is_binary(Path) ->
    true = link(Owner),
    case procket:socket(unix, stream, 0) of
	{ok, Sock} ->
	    SockUn = case Mode of
			 path ->
			     <<?PF_LOCAL:16/native,
			       Path/binary,
			       0:((?UNIX_PATH_MAX-byte_size(Path))*8)>>;
			 abstract ->
			     <<?PF_LOCAL:16/native,
			       0:8, Path/binary,
			       0:((?UNIX_PATH_MAX-(1+byte_size(Path)))*8)>>
		     end,
	    case procket:connect(Sock, SockUn) of
		ok ->
		    process_flag(trap_exit, true),
	 	    lager:info("transport_unix:init connect"),
		    Res = spawn_link(?MODULE, do_read, [Sock, self()]),
                    lager:info("transport_unix:init Res= ~p",[Res]),
		    {ok, #state{sock=Sock, owner=Owner}};
		{error, Err} ->
		    lager:error("Error connecting socket: ~p~n", [Err]),
		    {error, Err}
	    end;
	{error, Err} ->
	    lager:error("Error creating socket: ~p~n", [Err]),
	    {error, Err}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setopts, _Options}, _From, State) ->
    % TODO ?
    {reply, ok, State};

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
    lager:info("Transport unix:handle_info {received,Data}"),
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
do_read(Sock, Pid) ->
    case procket:recvfrom(Sock, 16#FFFF) of
	{error, eagain} ->
	    timer:sleep(10),
	    do_read(Sock, Pid);
	{error, Err} ->
	    lager:debug("UNIX socket listener died: ~p~n", [Err]),
	    ok;
	% EOF
	{ok, <<>>} ->
	    %do_read(Sock, Pid);
	    ok;
	{ok, Buf} ->
	    lager:info("transport_unix:do_read ok Buf = ~p ",[Buf]),
	    Pid ! {unix, Buf},
	    do_read(Sock, Pid)
    end.
