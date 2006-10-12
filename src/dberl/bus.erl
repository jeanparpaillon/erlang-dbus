-module(dberl.bus).

-import(call).
-import(error_logger).
-import(gen_server).
-import(io).
-import(lists).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([start_link/0, stop/0]).

-export([get_object/3,
	 call/2,
	 call/3,
	 wait_ready/1]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  serial=0,
	  auth,
	  sock,
	  state,
	  buf= <<>>,
	  pending=[],
	  waiting=[]
	 }).

-define(SERVER, ?MODULE).
-define(PORT, 1236).
-define(HOST, "localhost").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

get_object(Bus, Service, Path) ->
    proxy:start_link(Bus, Service, Path).

call(Bus, Header) ->
    gen_server:cast(Bus, {call, Header, self()}).

call(Bus, Header, From) ->
    gen_server:cast(Bus, {call, Header, From, self()}).

wait_ready(Bus) ->
    io:format("wait_ready enter ~p~n", [Bus]),
    ok = gen_server:call(Bus, wait_ready),
    io:format("wait_ready exit ~p~n", [Bus]),
    ok.

%%
%% gen_server callbacks
%%
init([]) ->
    DbusHost = ?HOST,
    DbusPort = ?PORT,
    {ok, Auth} = auth:start_link(DbusHost, DbusPort),
    {ok, #state{auth=Auth}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(wait_ready, _From, #state{state=up}=State) ->
    {reply, ok, State};

handle_call(wait_ready, From, State) ->
    Waiting = [ From | State#state.waiting ],
    io:format("wait_ready received ~p~n", [Waiting]),
    {noreply, State#state{waiting=Waiting}};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({call, Header, Pid}, State) ->
    handle_call(Header, none, Pid, State);

handle_cast({call, Header, From, Pid}, State) ->
    handle_call(Header, From, Pid, State);

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({received, Sock, Data}, #state{sock=Sock}=State) ->
    Buf = State#state.buf,
    {ok, State1} = handle_data(<<Buf/binary, Data/binary>>, State),
    {noreply, State1};

handle_info({auth_ok, Auth, Sock}, #state{auth=Auth}=State) ->
    ok = connection:change_owner(Sock, Auth, self()),

    {ok, State1} = send_hello(State#state{sock=Sock}),
%%     {ok, State2} = send_introspect(State1),

    Reply = fun(From) ->
		    io:format("reply_waiting ~p~n", [From]),
		    gen_server:reply(From, ok)
	    end,


    lists:map(Reply, State1#state.waiting),

    {noreply, State1#state{state=up, waiting=[], auth=terminated}};
%%     {stop, normal, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    terminated.


handle_call(Header, Tag, Pid, State) ->
%%     io:format("handle call ~p ~p~n", [Header, Body]),
    Sock = State#state.sock,
    Serial = State#state.serial + 1,

    {ok, Call} = call:start_link(self(), Tag, Pid),
    Pending = [{Serial, Call} | State#state.pending],

    {ok, Data} = marshaller:marshal_message(Header#header{serial=Serial}),
    ok = connection:send(Sock, Data),
    
    {noreply, State#state{pending=Pending, serial=Serial}}.

send_hello(State) ->
    Serial = State#state.serial + 1,
    Hello = build_hello(Serial),
    {ok, Data} = marshaller:marshal_message(Hello),
%%     io:format("send_hello ~p~n", [Hello]),
    ok = connection:send(State#state.sock, Data),
    {ok, State#state{serial=Serial}}.

send_list_names(State) ->
    Serial = State#state.serial + 1,
    Msg = build_list_names(Serial),
    {ok, Data} = marshaller:marshal_message(Msg),
    ok = connection:send(State#state.sock, Data),
    {ok, State#state{serial=Serial}}.

send_introspect(State) ->
    Serial = State#state.serial + 1,
    Msg = introspect:build_introspect("org.freedesktop.DBus", "/"),
    {ok, Data} = marshaller:marshal_message(Msg#header{serial=Serial}),
    ok = connection:send(State#state.sock, Data),
    {ok, State#state{serial=Serial}}.

handle_data(Data, State) ->
    {ok, Messages, Data1} = marshaller:unmarshal_data(Data),

%%     io:format("handle_data ~p ~p~n", [Messages, size(Data1)]),

    {ok, State1} = handle_messages(Messages, State#state{buf=Data1}),

    {ok, State1}.

handle_messages([], State) ->
    {ok, State};
handle_messages([Header|R], State) ->
    {ok, State1} = handle_message(Header#header.type, Header, State),
    handle_messages(R, State1).

%% FIXME handle illegal messages
handle_message(?TYPE_METHOD_RETURN, Header, State) ->
    [_, SerialHdr] = header_fetch(?HEADER_REPLY_SERIAL, Header),
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
    [_, SerialHdr] = header_fetch(?HEADER_REPLY_SERIAL, Header),
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
    io:format("Handle call ~p~n", [Header]),

    Serial = State#state.serial + 1,
    Path = header_fetch(?HEADER_PATH, Header),
    Iface = header_fetch(?HEADER_INTERFACE, Header),
    [_Type1, To] = header_fetch(?HEADER_DESTINATION, Header),
    [_Type2, From] = header_fetch(?HEADER_SENDER, Header),
    Error = #variant{type=string, value="org.freedesktop.DBus.Error.UnknownObject"},
    ReplySerial = #variant{type=uint32, value=Header#header.serial},

    {ok, ReplyBody, _Pos} = 
	marshaller:marshal_list([string], ["Erlang: Object not found."]),
    Headers = [
	       [?HEADER_ERROR_NAME, Error],
	       [?HEADER_REPLY_SERIAL, ReplySerial],
 	       [?HEADER_DESTINATION, From],
	       [?HEADER_SIGNATURE, #variant{type=signature, value="s"}]
	      ],

    ReplyHeader = #header{type=?TYPE_ERROR,
			  serial=Header#header.serial,
			  headers=Headers},

    io:format("Reply ~p ~p~n", [ReplyHeader, ReplyBody]),

    {ok, Data} = marshaller:marshal_message(ReplyHeader, ReplyBody),
    ok = connection:send(State#state.sock, Data),

    {ok, State#state{serial=Serial}};
    
handle_message(Type, Header, State) ->
    io:format("Ignore ~p ~p~n", [Type, Header]),
    {ok, State}.

handle_method_call(Header, Body) ->
    ok.


header_fetch(Code, Header) ->
    Headers = Header#header.headers,
    Fun = fun(F) ->
		  case F of
		      [Code | _] ->
			  true;
		      _ ->
			  false
		  end
	  end,

    [Field] = lists:filter(Fun, Headers),
    Field.

build_hello(Serial) ->
    Headers = [
	       [?HEADER_PATH, #variant{type=object_path, value="/org/freedesktop/DBus"}],
	       [?HEADER_DESTINATION, #variant{type=string, value="org.freedesktop.DBus"}],
	       [?HEADER_INTERFACE, #variant{type=string, value="org.freedesktop.DBus"}],
	       [?HEADER_MEMBER, #variant{type=string, value="Hello"}]
	      ],

    #header{type=?TYPE_METHOD_CALL,
	    serial=Serial,
	    headers=Headers}.

build_list_names(Serial) ->
    Headers = [
	       [?HEADER_PATH, #variant{type=object_path, value="/org/freedesktop/DBus"}],
	       [?HEADER_DESTINATION, #variant{type=string, value="org.freedesktop.DBus"}],
	       [?HEADER_INTERFACE, #variant{type=string, value="org.freedesktop.DBus"}],
	       [?HEADER_MEMBER, #variant{type=string, value="ListNames"}]
	      ],

    #header{type=?TYPE_METHOD_CALL,
	    serial=Serial,
	    headers=Headers}.
