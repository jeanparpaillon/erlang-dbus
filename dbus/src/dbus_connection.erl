%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%% Glue module to tcp_conn transport module
%%
%% Messages imlemented by transport modules
%%
%% {received, Conn, Data}
%% {auth_ok, Auth, Sock}

-module(dbus_connection).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/1,
	 start_link/2,
	 start_link/3,
	 close/1,
	 call/2,
	 call/3,
	 cast/2,
	 reply/2
	]).

%% gen_server callbacks
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-record(state, {
	  serial=0,
	  sock,
	  auth,
	  buf= <<>>,
	  owner,
	  state,
	  pending=[]
	 }).


start_link(BusId) ->
    start_link(BusId, []).

start_link(BusId, Options) ->
    start_link(BusId, Options, self()).

start_link(BusId, Options, Owner) when is_record(BusId, bus_id),
				       is_list(Options),
				       is_pid(Owner) ->
    gen_server:start_link(?MODULE, [BusId, Options, Owner], []).

close(Conn) ->
    gen_server:cast(Conn, close).

call(Conn, Header) ->
    gen_server:cast(Conn, {call, Header, self()}).

call(Conn, Header, From) ->
    gen_server:cast(Conn, {call, Header, From, self()}).

cast(Conn, Header) ->
    gen_server:cast(Conn, {cast, Header}).

reply(Conn, Header) ->
    gen_server:cast(Conn, {dbus_reply, Header}).

%%
%% gen_server callbacks
%%
init([#bus_id{scheme=tcp,options=BusOptions}, Options, Owner]) ->
    true = link(Owner),
    {Host, Port} = case {lists:keysearch(host, 1, BusOptions),
			 lists:keysearch(port, 1, BusOptions)} of
		       {{value, {host, Host1}}, {value, {port,Port1}}} ->
			   {Host1, Port1};
		       _ ->
			   throw(no_host_or_port)
		   end,

    {ok, Sock} = tcp_conn:connect(Host, Port, Options),
%%     {ok, Auth} = auth:start_link(DbusHost, DbusPort),
    {ok, Auth} = auth:start_link(Sock),
    {ok, #state{sock=Sock,
		auth=Auth,
		owner=Owner}};

init([#bus_id{scheme=unix, options=BusOptions}, Options, Owner]) ->
    true = link(Owner),
    Path = case lists:keysearch(path, 1, BusOptions) of
	       {value, {_, Path1}} ->
		   Path1;
	       _ ->
		   throw(no_path)
	   end,

    {ok, Sock} = unix_conn:connect(Path, Options),
%%     {ok, Auth} = auth:start_link(DbusHost, DbusPort),
    {ok, Auth} = auth:start_link(Sock),
    {ok, #state{sock=Sock,
		auth=Auth,
		owner=Owner}};
init(Args) ->
    throw({bad_init_args}).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({call, Header, Pid}, State) ->
    {ok, State1} = handle_method_call(Header, none, Pid, State),
    {noreply, State1};

handle_cast({call, Header, From, Pid}, State) ->
    {ok, State1} = handle_method_call(Header, From, Pid, State),
    {noreply, State1};

handle_cast({cast, Header}, State) ->
    Serial = State#state.serial+1,
    Header1 = Header#header{serial=Serial},
    {ok, Data} = marshaller:marshal_message(Header1),
    ok = transport:send(State#state.sock, Data),
    {noreply, State#state{serial=Serial}};

handle_cast({dbus_reply, Header}, State) ->
    Serial = State#state.serial+1,
    Header1 = Header#header{serial=Serial},
    {ok, Data} = marshaller:marshal_message(Header1),
    ok = transport:send(State#state.sock, Data),
    {noreply, State#state{serial=Serial}};

handle_cast(close, State) ->
    ok = transport:close(State#state.sock),
    {stop, normal, State#state{sock=undefined}};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({received, Sock, Data}, #state{sock=Sock}=State) ->
    Buf = State#state.buf,
    {ok, State1} = handle_data(<<Buf/binary, Data/binary>>, State),
    {noreply, State1};

handle_info({auth_ok, Auth, Sock}, #state{auth=Auth}=State) ->
    ok = transport:change_owner(Sock, Auth, self()),

    Owner = State#state.owner,
    Owner ! {auth_ok, self()},

    {noreply, State#state{sock=Sock,
			  state=up,
			  auth=terminated
			 }};

%% handle_info({auth_rejected, Auth}, #state{auth=Auth}=State) ->
%%     reply_waiting({error, auth_error}, State),
%%     {stop, normal, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Sock = State#state.sock,
    case Sock of
	undefined -> ignore;
	_ -> connection:close(Sock)
    end,
    terminated.


handle_data(Data, State) ->
    {ok, Messages, Data1} = marshaller:unmarshal_data(Data),

%%     io:format("handle_data ~p ~p~n", [Messages, size(Data1)]),

    {ok, State1} = handle_messages(Messages, State#state{buf=Data1}),

    {ok, State1}.


handle_method_call(Header, Tag, Pid, State) ->
    Sock = State#state.sock,
    Serial = State#state.serial + 1,
%%     io:format("handle call ~p ~p ~p~n", [Header, Tag, Pid]),

    {ok, Call} = call:start_link(self(), Tag, Pid),
    Pending = [{Serial, Call} | State#state.pending],

    {ok, Data} = marshaller:marshal_message(Header#header{serial=Serial}),
    ok = transport:send(Sock, Data),

%%     io:format("sent call ~p ~p~n", [Sock, Data]),

    {ok, State#state{pending=Pending, serial=Serial}}.

handle_messages([], State) ->
    {ok, State};
handle_messages([Header|R], State) ->
    {ok, State1} = handle_message(Header#header.type, Header, State),
    handle_messages(R, State1).

handle_message(?TYPE_METHOD_RETURN, Header, State) ->
%%     io:format("Return ~p~n", [Header]),
    {_, SerialHdr} = message:header_fetch(?HEADER_REPLY_SERIAL, Header),
    Pending = State#state.pending,
    Serial = SerialHdr#variant.value,
    State1 =
	case lists:keysearch(Serial, 1, Pending) of
	    {value, {Serial, Pid}} ->
		ok = call:reply(Pid, Header),
		State#state{pending=lists:keydelete(Serial, 1, Pending)};
	    _ ->
		io:format("Ignore reply ~p~n", [Serial]),
		State
	end,
    {ok, State1};
handle_message(?TYPE_ERROR, Header, State) ->
%%     io:format("Error ~p~n", [Header]),
    {_, SerialHdr} = message:header_fetch(?HEADER_REPLY_SERIAL, Header),
    Pending = State#state.pending,
    Serial = SerialHdr#variant.value,
    State1 =
	case lists:keysearch(Serial, 1, Pending) of
	    {value, {Serial, Pid}} ->
		ok = call:error(Pid, Header),
		State#state{pending=lists:keydelete(Serial, 1, Pending)};
	    _ ->
		io:format("Ignore error ~p~n", [Serial]),
		State
	end,
    {ok, State1};
handle_message(?TYPE_METHOD_CALL, Header, State) ->
    Owner = State#state.owner,
%%     io:format("Method call ~p~n", [Owner]),
    Owner ! {dbus_method_call, Header, self()},
    {ok, State};

handle_message(?TYPE_SIGNAL, Header, State) ->
    Owner = State#state.owner,
    Owner ! {dbus_signal, Header, self()},
    {ok, State};

handle_message(Type, Header, State) ->
    io:format("Ignore ~p ~p~n", [Type, Header]),
    {ok, State}.

