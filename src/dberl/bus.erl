-module(dberl.bus).

-import(error_logger).
-import(gen_server).
-import(io).
-import(lists).

-include("dbus.hrl").

-behaviour(gen_server).

%% api
-export([connect/2, stop/1]).

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
	  waiting=[],
	  hello_ref,
	  id,
	  dbus_object
	 }).

connect(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

stop(Bus) ->
    gen_server:cast(Bus, stop).

get_object(Bus, Service, Path) ->
    proxy:start_link(Bus, Service, Path).

call(Bus, Header) ->
    gen_server:cast(Bus, {call, Header, self()}).

call(Bus, Header, From) ->
    gen_server:cast(Bus, {call, Header, From, self()}).

wait_ready(Bus) ->
    gen_server:call(Bus, wait_ready).

%%
%% gen_server callbacks
%%
init([DbusHost, DbusPort]) ->
    {ok, Sock} = tcp_conn:connect(DbusHost, DbusPort, [list, {packet, 0}]),
%%     {ok, Auth} = auth:start_link(DbusHost, DbusPort),
    {ok, Auth} = auth:start_link(Sock),
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
    {ok, State1} = handle_call(Header, none, Pid, State),
    {noreply, State1};

handle_cast({call, Header, From, Pid}, State) ->
    {ok, State1} = handle_call(Header, From, Pid, State),
    {noreply, State1};

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

    DBusRootNode = default_dbus_node(),
    {ok, DBusObj} =
	proxy:start_link(self(), 'org.freedesktop.DBus', '/', DBusRootNode),
    {ok, DBusIfaceObj} = proxy:interface(DBusObj, 'org.freedesktop.DBus'),
    ok = proxy:call(DBusIfaceObj, 'Hello', [], [{reply, self(), hello}]),
    io:format("Call returned~n"),

    {noreply, State#state{sock=Sock,
			  state=up,
			  auth=terminated,
			  dbus_object=DBusObj
			 }};

handle_info({auth_rejected, Auth}, #state{auth=Auth}=State) ->
    reply_waiting({error, auth_error}, State),
    {stop, normal, State};

handle_info({reply, hello, {ok, Header}}, State) ->
    DBusObj = State#state.dbus_object,
    error_logger:error_msg("Hello reply ~p~n", [Header]),
    [Id] = Header#header.body,

    {ok, DBusIntrospectable} = proxy:interface(DBusObj, 'org.freedesktop.DBus.Introspectable'),
    ok = proxy:call(DBusIntrospectable, 'Introspect', [], [{reply, self(), introspect}]),

    reply_waiting(ok, State),
    {noreply, State#state{id=Id}};

handle_info({reply, introspect, {ok, Header}}, State) ->
    error_logger:error_msg("Introspect reply ~p~n", [Header]),
    reply_waiting(ok, State),
    {noreply, State#state{waiting=[]}};

handle_info({reply, Ref, {error, Reason}}, #state{hello_ref=Ref}=State) ->
    {stop, {error, Reason}, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


reply_waiting(Reply, State) ->
    Fun = fun(From) ->
		    io:format("reply_waiting ~p~n", [From]),
		    gen_server:reply(From, Reply)
	    end,

    lists:map(Fun, State#state.waiting).


handle_call(Header, Tag, Pid, State) ->
%%     io:format("handle call ~p ~p ~p~n", [Header, Tag, Pid]),
    Sock = State#state.sock,
    Serial = State#state.serial + 1,

    {ok, Call} = call:start_link(self(), Tag, Pid),
    Pending = [{Serial, Call} | State#state.pending],

    {ok, Data} = marshaller:marshal_message(Header#header{serial=Serial}),
    ok = connection:send(Sock, Data),
%%     io:format("sent call ~p ~p~n", [Sock, Data]),
    
    {ok, State#state{pending=Pending, serial=Serial}}.

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
    [_, SerialHdr] = message:header_fetch(?HEADER_REPLY_SERIAL, Header),
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
    [_, SerialHdr] = message:header_fetch(?HEADER_REPLY_SERIAL, Header),
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
    {ok, Reply, State1} = build_error(Header, ErrorName, ErrorText, State),
    io:format("Reply ~p~n", [Reply]),

    {ok, Data} = marshaller:marshal_message(Reply),
    ok = connection:send(State#state.sock, Data),

    {ok, State1};
    
handle_message(Type, Header, State) ->
    io:format("Ignore ~p ~p~n", [Type, Header]),
    {ok, State}.

build_error(Header, ErrorName, ErrorText, State) ->
    Serial = State#state.serial + 1,
%%     Path = message:header_fetch(?HEADER_PATH, Header),
%%     Iface = message:header_fetch(?HEADER_INTERFACE, Header),
%%     [_Type1, To] = message:header_fetch(?HEADER_DESTINATION, Header),
    [_Type2, From] = message:header_fetch(?HEADER_SENDER, Header),
    Error = #variant{type=string, value=ErrorName},
    ReplySerial = #variant{type=uint32, value=Header#header.serial},

    {ok, ReplyBody, _Pos} = 
	marshaller:marshal_list([string], [ErrorText]),
    Headers = [
	       [?HEADER_ERROR_NAME, Error],
	       [?HEADER_REPLY_SERIAL, ReplySerial],
 	       [?HEADER_DESTINATION, From],
	       [?HEADER_SIGNATURE, #variant{type=signature, value="s"}]
	      ],

    ReplyHeader = #header{type=?TYPE_ERROR,
			  serial=Header#header.serial,
			  headers=Headers,
			  body=ReplyBody},
    {ok, ReplyHeader, State#state{serial=Serial}}.


default_dbus_node() ->
    HelloMethod = #method{name='Hello', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    DBusIface = #interface{name='org.freedesktop.DBus', methods=[HelloMethod]},

    IntrospectMethod = #method{name='Introspect', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    DBusIntrospectableIface = #interface{name='org.freedesktop.DBus.Introspectable', methods=[IntrospectMethod]},

    DBusRootNode = #node{elements=[], interfaces=[DBusIface, DBusIntrospectableIface]},
    DBusRootNode.
