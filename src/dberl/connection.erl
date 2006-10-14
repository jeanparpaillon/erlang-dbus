%%%
%%% Glue module to tcp_conn transport module
%%%
%%% Messages imlemented by transport modules
%%%
%%% {received, Conn, Data}
%%% {closed, Conn}

-module(dberl.connection).

-import(error_logger).
-import(gen_server).
-import(gen_tcp).
-import(inet).
-import(io).
-import(lists).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/2,
	 start_link/3,
	 close/1,
	 call/2,
	 call/3,
	 cast/2
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


start_link(Host, Port) ->
    start_link(Host, Port, []).

start_link(Host, Port, Options) ->
    gen_server:start_link(?MODULE, [Host, Port, Options, self()], []).

close(Conn) ->
    gen_server:cast(Conn, close).

call(Conn, Header) ->
    gen_server:cast(Conn, {call, Header, self()}).

call(Conn, Header, From) ->
    gen_server:cast(Conn, {call, Header, From, self()}).

cast(Conn, Header) ->
    gen_server:cast(Conn, {cast, Header}).

%%
%% gen_server callbacks
%%
init([Host, Port, Options, Owner]) ->
    true = link(Owner),
    {ok, Sock} = tcp_conn:connect(Host, Port, Options),
%%     {ok, Auth} = auth:start_link(DbusHost, DbusPort),
    {ok, Auth} = auth:start_link(Sock),
    {ok, #state{sock=Sock,
		auth=Auth,
		owner=Owner}}.


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
    %% FIXME serial
    {ok, Data} = marshaller:marshal_message(Header),
    ok = transport:send(State#state.sock, Data),
    {noreply, State};

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
    io:format("handle call ~p ~p ~p~n", [Header, Tag, Pid]),

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

%% FIXME handle illegal messages
handle_message(?TYPE_METHOD_RETURN, Header, State) ->
    io:format("Return ~p~n", [Header]),
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
    io:format("Error ~p~n", [Header]),
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
%%     io:format("Handle call ~p~n", [Header]),

    ErrorName = "org.freedesktop.DBus.Error.UnknownObject",
    ErrorText = "Erlang: Object not found.",
    {ok, Reply} = build_error(Header, ErrorName, ErrorText),
    io:format("Reply ~p~n", [Reply]),

%%     ok = connection:cast(State#state.conn, Reply),
    %% TODO send reply

    {ok, State};
handle_message(?TYPE_SIGNAL, Header, State) ->
    io:format("Signal ~p~n", [Header]),
%%     {_, SerialHdr} = message:header_fetch(?HEADER_REPLY_SERIAL, Header),
%%     Pending = State#state.pending,
%%     Serial = SerialHdr#variant.value,
%%     State1 =
%% 	case lists:keysearch(Serial, 1, Pending) of
%% 	    {value, {Serial, Pid}} ->
%% 		ok = call:reply(Pid, Header),
%% 		State#state{pending=lists:keydelete(Serial, 1, Pending)};
%% 	    _ ->
%% 		io:format("Ignore reply ~p~n", [Serial]),
%% 		State
%% 	end,
%%     {ok, State1};
    {ok, State};
    
handle_message(Type, Header, State) ->
    io:format("Ignore ~p ~p~n", [Type, Header]),
    {ok, State}.

build_error(Header, ErrorName, ErrorText) ->
%%     Path = message:header_fetch(?HEADER_PATH, Header),
%%     Iface = message:header_fetch(?HEADER_INTERFACE, Header),
%%     {_Type1, To} = message:header_fetch(?HEADER_DESTINATION, Header),
    {_Type2, From} = message:header_fetch(?HEADER_SENDER, Header),
    Error = #variant{type=string, value=ErrorName},
    ReplySerial = #variant{type=uint32, value=Header#header.serial},

    {ok, ReplyBody, _Pos} = 
	marshaller:marshal_list([string], [ErrorText]),
    Headers = [
	       {?HEADER_ERROR_NAME, Error},
	       {?HEADER_REPLY_SERIAL, ReplySerial},
 	       {?HEADER_DESTINATION, From},
	       {?HEADER_SIGNATURE, #variant{type=signature, value="s"}}
	      ],

    ReplyHeader = #header{type=?TYPE_ERROR,
			  serial=Header#header.serial,
			  headers=Headers,
			  body=ReplyBody},
    {ok, ReplyHeader}.
