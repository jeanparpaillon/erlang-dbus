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

-record(state, {
	  sock,
	  owner
	 }).

-define(IS_SERVER, 1).
-define(IS_ABSTRACT, 2).
-define(IS_NULLTERM, 4).

-define(PF_LOCAL, 1).
-define(UNIX_PATH_MAX, 108).
-define(BACKLOG, 5).

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
init([Path, _Options, Owner]) when is_pid(Owner), is_list(Path) ->
    true = link(Owner),
    case procket:socket(unix, stream, 0) of
	{ok, Sock} ->
	    BinPath = list_to_binary(Path),
	    SockUn = <<?PF_LOCAL:16/native,
		       BinPath/binary,
		       0:((?UNIX_PATH_MAX-byte_size(BinPath))*8)>>,
	    case procket:connect(Sock, SockUn) of
		ok ->
		    %process_flag(trap_exit, true),
		    spawn_link(?MODULE, do_read, [Sock, self()]),
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

handle_call({change_owner, OldPid, NewPid}, _From, #state{owner=OldPid}=State) when is_pid(NewPid) ->
    true = link(NewPid),
    true = unlink(OldPid),
    {reply, ok, State#state{owner=NewPid}};

handle_call({change_owner, _OldPid, _NewPid}, _From, State) ->
    {reply, error, State};

handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({send, Data}, #state{sock=Sock}=State) when is_list(Data) ->
    handle_cast({send, iolist_to_binary(Data)}, #state{sock=Sock}=State);
handle_cast({send, Data}, #state{sock=Sock}=State) when is_binary(Data) ->
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
    Owner ! {received, self(), Data},
    {noreply, State};

handle_info({unix_closed, Sock}, #state{sock=Sock, owner=Owner}=State) ->
    Owner ! {closed, self()},
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
	% EOF
	{ok, <<>>} -> ok;
	{ok, Buf} ->
	    Pid ! {unix, Buf},
	    do_read(Sock, Pid)
    end.
