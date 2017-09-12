%%
%% @copyright 2014 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @author Tony Wallace <tony@tony.gen.nz> - added comments to code
%% @doc DBUS transport implementation on UNIX socket
%% This is done by starting a gen_server in response to a connect/2.
%% Currently only the first parameter of connect/2 is used.
%%
%% Once started this server accepts the following calls:
%%   gen_server:call(ServerRef,support_unix_fd) -> true
%%   gen_server:call(ServerRef,{set_raw,true}) -> ok
%%
%% The following casts are supported:
%%   gen_server:cast(ServerRef,{send,Data}) -> ok
%%   gen_server:cast(ServerRef,close) -> ok
%%   gen_server:cast(ServerRef,stop) -> ok
%%
%% In order to handle messages comming in from the tcp connection
%% messages are sent directly to the process that called connect.
%% The following messages are sent:
%%     {received,Data} - Data has been received
%%     closed - The tcp socket has been closed
%%
%% As an implementation note, this module spawns a separate listener process (do_read)
%% to receive communications from the socket.  The reason for doing this is unclear,
%% as it does not seem necessary in dbus_transport_tcp which operates similarly.
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

%% @doc
%% connect/2 starts a gen_server to manage a unix socket
%% @param BusOptions
%% A property list containing either a path key value pair
%% or an abstract key value pair.
-spec connect(
	[{path,string()|binary()}|{abstract,string()}],
	Unused_Parameter::any()) ->
		     {ok,pid()} | ignore | {error, term()}.
connect(BusOptions, _Options) ->
    Path = case proplists:get_value(path, BusOptions) of
	       undefined ->
                   case proplists:get_value(abstract, BusOptions) of
                       undefined ->
                           throw(no_path);
                       V ->
                           <<0, (list_to_binary(V))/binary>>
                   end;
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
    case gen_tcp:connect({local, Path}, 0, [local, {recbuf, 65535}]) of
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
    %%?debug("unix send(~p)~n", [Data]),
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
    %%?debug("unix received(~p)~n", [Data]),
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
