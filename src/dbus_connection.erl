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
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_fsm).

-include("dbus.hrl").

%% api
-export([start_link/1,
	 start_link/2,
	 close/1,
	 auth/1,
	 call/2,
	 cast/2]).

%% gen_fsm callbacks
-export([init/1,
	 code_change/4,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3]).

%% gen_fsm states
-export([connected/3,
	 waiting_for_ok/3,
	 waiting_for_data/3,
	 waiting_for_reject/3,
	 waiting_for_agree/3,
	 authenticated/3]).

-record(state, {serial   = 1,
		mod,
		sock,
		buf      = <<>>,
		pending           :: term(),  % tid(),
		waiting  = [],
		mechs    = [],
		mech_state,
		guid,
		owner,
		unix_fd           :: boolean()
	       }).

-define(TIMEOUT, 10000).

-define(auth_mechs_tcp, [dbus_auth_cookie_sha1]).
-define(auth_mechs_unix, [dbus_auth_external, dbus_auth_cookie_sha1]).

start_link(BusId) ->
    start_link(BusId, []).

start_link(BusId, Options) when is_record(BusId, bus_id),
				is_list(Options) ->
    gen_fsm:start_link(?MODULE, [BusId, Options], []).

close(Conn) ->
    gen_fsm:send_all_state_event(Conn, close).

-spec call(pid(), term()) -> {ok, term()} | {error, term()}.
call(Conn, Header) ->
    R = gen_fsm:sync_send_event(Conn, {call, Header}),
    lager:info("connection:call R= ~p",[R]),
    case  R of
	{ok, Tag} ->
	    receive
		{reply, Tag, Res} ->
		    {ok, Res};
		{error, Tag, Err} ->
		    {error, Err};
		Other ->
		    throw({error, {dbus, Other}})
	    after ?TIMEOUT ->
		    lager:error("DBUS timeout~n", []),
		    throw({error, timeout})
	    end;
	{error, Err} ->
	    throw({error, Err})
    end.

-spec cast(pid(), term()) -> ok | {error, term()}.
cast(Conn, Header) ->
    gen_fsm:send_event(Conn, Header).

-spec auth(pid()) -> ok | {error, term()}.
auth(Conn) ->
    case gen_fsm:sync_send_event(Conn, auth) of
	authenticated ->
	    ok;
	{ok, Tag} ->
	    receive
		{authenticated, Tag} -> ok;
		{error, Res} -> {error, Res};
		Other -> 
		    throw({error, {dbus, Other}})
	    end
    end.

%%
%% gen_fsm callbacks
%%
init([#bus_id{scheme=tcp,options=BusOptions}, Options]) ->
    {Host, Port} = case {lists:keysearch(host, 1, BusOptions),
			 lists:keysearch(port, 1, BusOptions)} of
		       {{value, {host, Host1}}, {value, {port,Port1}}} ->
			   {Host1, Port1};
		       _ ->
			   throw(no_host_or_port)
		   end,

    {ok, Sock} = dbus_transport_tcp:connect(Host, Port, Options),
    lager:info("connection:init tcp  "),
    init_connection(Sock, ?auth_mechs_tcp);

init([#bus_id{scheme=unix, options=BusOptions}, Options]) ->
    {ok, Sock} = dbus_transport_unix:connect(BusOptions, Options),
     lager:info("connection:init unix  "),
    init_connection(Sock, ?auth_mechs_unix).


code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.


handle_sync_event(_Evt, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_event(close, _StateName, #state{sock=Sock}=State) ->
    ok = dbus_transport:close(Sock),
    {stop, normal, State#state{sock=undefined}};

handle_event(Evt, StateName, State) ->
    lager:error("Unhandled event: ~p~n", [Evt]),
    {next_state, StateName, State}.


%% STATE: connected
handle_info({received, _Bin}, connected, #state{sock=Sock}=State) ->
    lager:error("Unexpected info: ~p", [_Bin]),
    ok = dbus_transport:send(Sock, <<"ERROR \r\n">>),
    {next_state, connected, State};

%% STATE: waiting_for_ok OR waiting_for_data
handle_info({received, <<"ERROR ", _Line/binary>>}, StateName,
	   #state{mechs=[]}=State) 
  when StateName =:= waiting_for_ok; StateName =:= waiting_for_data ->
    {stop, disconnect, State};

handle_info({received, <<"ERROR ", _Line/binary>>}, StateName,
	   #state{sock=Sock, mechs=[_|Mechs]}=State) 
  when StateName =:= waiting_for_ok; StateName =:= waiting_for_data ->
    lager:info("connection:handle_info ERROR"),
    ok = dbus_transport:send(Sock, <<"CANCEL \r\n">>),
    {next_state, waiting_for_reject, #state{mechs=Mechs}=State};    

handle_info({received, <<"OK ", Line/binary>>}, StateName, 
	    #state{sock=Sock, waiting=Waiting}=State) 
  when StateName =:= waiting_for_ok; StateName =:= waiting_for_data ->
    lager:info("connection:handle_info OK"),
    Guid = strip_eol(Line),
    lager:debug("Authenticated: GUID=~p~n", [Guid]),
    case dbus_transport:support_unix_fd(Sock) of
	true ->
	    ok = dbus_transport:send(Sock, <<"NEGOTIATE_UNIX_FD \r\n">>),
	    {next_state, waiting_for_agree, State#state{guid=Guid}};
	false ->
	    ok = dbus_transport:send(Sock, <<"BEGIN \r\n">>),
	    lists:foreach(fun ({Pid, Tag}) ->
				  Pid ! {authenticated, {self(), Tag}}
			  end, Waiting),
	    ok = dbus_transport:set_raw(Sock, true),
	    {next_state, authenticated, State#state{waiting=[], guid=Guid, unix_fd=false}}
    end;

handle_info({received, <<"REJECTED ", _Line/binary>>}, StateName,
	   #state{mechs=[]}=State) 
  when StateName =:= waiting_for_ok; StateName =:= waiting_for_data ->
    {stop, disconnect, State};

handle_info({received, <<"REJECTED ", _Line/binary>>}, StateName,
	   #state{sock=Sock, mechs=[Mech|Rest]}=State) 
  when StateName =:= waiting_for_ok; StateName =:= waiting_for_data ->
    lager:debug("Trying next authentication mechanism~n", []),
    case Mech:init() of 
	{ok, Resp} ->
	    lager:debug("DBUS auth: waiting for OK~n", []),
	    dbus_transport:send(Sock, Resp),
	    {next_state, waiting_for_ok, State#state{mechs=Rest}};
	{continue, Resp, MechState} ->
	    lager:debug("DBUS auth: waiting for Data~n", []),
	    dbus_transport:send(Sock, Resp),
	    {next_state, waiting_for_data, State#state{mechs=Rest, mech_state={Mech, MechState}}}
    end;

%% STATE: waiting_for_ok
handle_info({received, <<"DATA ", _Line/binary>>}, waiting_for_ok, 
	    #state{sock=Sock}=State) ->
	lager:info("connection:handle_info DATA waiting_for_ok"),
    ok = dbus_transport:send(Sock, <<"CANCEL \r\n">>),
    {next_state, waiting_for_reject, State};

handle_info({received, _Bin}, waiting_for_ok, #state{sock=Sock}=State) ->
    lager:error("Unexpected info: ~p", [_Bin]),
    ok = dbus_transport:send(Sock, <<"ERROR \r\n">>),
    {next_state, waiting_for_ok, State};

%% STATE: waiting_for_data
handle_info({received, <<"DATA ", Line/binary>>}, waiting_for_data,
	    #state{sock=Sock, mech_state={Mech, MechState}}=State) ->
	lager:info("connection:handle_info DATA waiting_for_data"),
    Bin = dbus_hex:from(strip_eol(Line)),
    case Mech:challenge(Bin, MechState) of
	{ok, Resp} ->
	    lager:debug("DBUS auth: answering challenge~n", []),
	    dbus_transport:send(Sock, Resp),
	    {next_state, waiting_for_ok, State};
	{continue, Resp, MechState} ->
	    lager:debug("DBUS auth: answering challenge (continue)~n", []),
	    dbus_transport:send(Sock, Resp),
	    {next_state, waiting_for_data, State#state{mech_state={Mech, MechState}}};
	{error, Err} ->
	    lager:debug("Error with authentication challenge: ~p~n", [Err]),
	    ok = dbus_transport:send(<<"CANCEL \r\n">>),
	    {next_state, waiting_for_reject, State}
    end;

handle_info({received, _Bin}, waiting_for_data, #state{sock=Sock}=State) ->
    lager:error("Unexpected info: ~p", [_Bin]),
    ok = dbus_transport:send(Sock, <<"ERROR \r\n">>),
    {next_state, waiting_for_data, State};

%% STATE: waiting_for_reject
handle_info({received, <<"REJECTED ", Line/binary>>}, waiting_for_reject,
	   #state{sock=Sock}=State) ->
	lager:info("connection:handle_info REJECTED"),
    case parse_mechs(strip_eol(Line)) of
	{ok, [Mech|Rest]} ->
	    case Mech:init() of 
		{ok, Resp} ->
		    lager:debug("DBUS auth: waiting for OK~n", []),
		    dbus_transport:send(Sock, Resp),
		    {next_state, waiting_for_ok, State#state{mechs=Rest}};
		{continue, Resp, MechState} ->
		    lager:debug("DBUS auth: waiting for Data~n", []),
		    dbus_transport:send(Sock, Resp),
		    {next_state, waiting_for_data, State#state{mechs=Rest, mech_state={Mech, MechState}}}
	    end;
	{error, Err} ->
	    lager:error("Invalid mechanismes: ~p~n", [Err]),
	    {stop, disconnect, State}
    end;

handle_info({received, _Bin}, waiting_for_reject, State) ->
    lager:error("Unexpected info: ~p", [_Bin]),
    {stop, disconnect, State};

%% STATE: waiting_for_agree
handle_info({received, <<"AGREE_UNIX_FD\r\n">>}, waiting_for_agree,
	   #state{sock=Sock, waiting=Waiting}=State) ->
    ok = dbus_transport:send(Sock, <<"BEGIN \r\n">>),
    lists:foreach(fun ({Pid, Tag}) ->
			  Pid ! {authenticated, {self(), Tag}}
		  end, Waiting),
    ok = dbus_transport:set_raw(Sock, true),
    {next_state, authenticated, State#state{unix_fd=true, waiting=[]}};

handle_info({received, <<"ERROR ", _Line/binary>>}, waiting_for_agree,
	    #state{sock=Sock, waiting=Waiting}=State) ->
    ok = dbus_transport:send(Sock, <<"BEGIN \r\n">>),
    lists:foreach(fun ({Pid, Tag}) ->
			  Pid ! {authenticated, {self(), Tag}}
		  end, Waiting),
    ok = dbus_transport:set_raw(Sock, true),
    {next_state, authenticated, State#state{unix_fd=false, waiting=[]}};    

handle_info({received, _Bin}, waiting_for_agree, State) ->
    lager:error("Unexpected info: ~p", [_Bin]),
    {stop, disconnect, State};

%% STATE: authenticated
handle_info({received, Data}, authenticated, #state{buf=Buf}=State) ->
    lager:info("connection:authenticated handle_info"),
    {ok, Msgs, Rest} = dbus_marshaller:unmarshal_data(<<Buf/binary, Data/binary>>),
    lager:info("connection:authenticated handle_info Msgs=~p, Rest=~p",[Msgs,Rest]),
    R = handle_messages(Msgs, State#state{buf=Rest}),
    lager:info("connection:authenticated handle_info R=~p",[R]),
    case R of
	{ok, State2} ->
	    {next_state, authenticated, State2};
	{error, Err, State2} ->
	    {stop, {error, Err}, State2}
    end;

%% Other
handle_info(closed, StateName, State) ->
    lager:error("Connection closed...~n", []),
    lager:info("connection:close StatName=~p",[StateName]),
    {stop, closed, State};

handle_info(_Evt, StateName, State) ->
    lager:error("Unexpected event: ~p~n", [_Evt]),
    {next_state, StateName, State}.


terminate(_Reason, _StateName, #state{sock=Sock}) ->
    case Sock of
	    undefined -> ignore;
	    _ -> dbus_connection:close(Sock)
    end,
    ok.

%%%
%%% gen_fsm states
%%%
connected(auth, {_Pid, Tag}=From, #state{sock=Sock, mechs=[]}=State) ->
    dbus_transport:send(Sock, <<"AUTH\r\n">>),
    {reply, {ok, {self(), Tag}}, waiting_for_reject, 
     State#state{waiting=[From]}};
connected(auth, {_Pid, Tag}=From, #state{sock=Sock, mechs=[Mech|Rest]}=State) ->
    case Mech:init() of
	{ok, Resp} ->
	    lager:debug("DBUS auth: sending initial data~n", []),
	    dbus_transport:send(Sock, Resp),
	    {reply, {ok, {self(), Tag}}, waiting_for_ok, 
	     State#state{waiting=[From],
			 mechs=Rest}};
	{continue, Resp, MechState} ->
	    lager:debug("DBUS auth: sending initial data (continue)~n", []),
	    dbus_transport:send(Sock, Resp),
	    {reply, {ok, {self(), Tag}}, waiting_for_data,
	     State#state{waiting=[From],
			 mechs=Rest, mech_state={Mech, MechState}}}
    end;

connected({call, _}, _From, State) ->
    {reply, {error, authentication_needed}, connected, State};

connected(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, connected, State}.


waiting_for_ok({call, _}, _From, State) ->
    {reply, {error, authentication_needed}, waiting_for_ok, State};

waiting_for_ok(auth, {_Pid, Tag}=From, #state{waiting=Waiting}=State) ->
    {reply, {ok, {self(), Tag}}, waiting_for_ok, 
     State#state{waiting=[From|Waiting]}};
waiting_for_ok(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, waiting_for_ok, State}.


waiting_for_data({call, _}, _From, State) ->
    {reply, {error, authentication_needed}, waiting_for_data, State};

waiting_for_data(auth, {_Pid, Tag}=From, #state{waiting=Waiting}=State) ->
    {reply, {ok, {self(), Tag}}, waiting_for_data, 
     State#state{waiting=[From|Waiting]}};
waiting_for_data(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, waiting_for_data, State}.


waiting_for_reject({call, _}, _From, State) ->
    {reply, {error, authentication_needed}, waiting_for_reject, State};

waiting_for_reject(auth, {_Pid, Tag}=From, #state{waiting=Waiting}=State) ->
    {reply, {ok, {self(), Tag}}, waiting_for_reject, 
     State#state{waiting=[From|Waiting]}};
waiting_for_reject(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, waiting_for_reject, State}.


waiting_for_agree({call, _}, _From, State) ->
    {reply, {error, authentication_needed}, waiting_for_agree, State};

waiting_for_agree(auth, {_Pid, Tag}=From, #state{waiting=Waiting}=State) ->
    {reply, {ok, {self(), Tag}}, waiting_for_agree, 
     State#state{waiting=[From|Waiting]}};
waiting_for_agree(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, waiting_for_agree, State}.


authenticated(auth, _From, State) ->
    {reply, authenticated, authenticated, State};

authenticated({call, #dbus_message{}=Msg}, {Pid, Tag}, 
	      #state{sock=Sock, serial=S, pending=Pending}=State) ->
	R = dbus_message:set_serial(S, Msg),
	lager:info("connection:authenticated call R = ~p",[R]),
    Data = dbus_marshaller:marshal_message(R),
    lager:info("connection:authenticated call Data=~p",[Data]),
    true = ets:insert(Pending, {S, Pid, Tag}),
    ok = dbus_transport:send(Sock, Data),
    {reply, {ok, {self(), Tag}}, authenticated, State#state{serial=S+1}};

authenticated(_Evt, _From, State) ->
    lager:debug("Unexpected event: ~p~n", [_Evt]),
    {reply, {error, invalid_event}, authenticated, State}.

%%%
%%% Priv
%%%
handle_messages([], State) ->
    {ok, State};

handle_messages([#dbus_message{header=#dbus_header{type=Type}}=Msg | R], State) ->
    case handle_message(Type, Msg, State) of
	    {ok, State2} ->
	         handle_messages(R, State2);
	    {error, Err} ->
	        {error, Err}
    end.

handle_message(?TYPE_METHOD_RETURN, #dbus_message{}=Msg, #state{pending=Pending}=State) ->
    Serial = dbus_message:get_field_value(?HEADER_REPLY_SERIAL, Msg),
    case ets:lookup(Pending, Serial) of
	    [{Serial, Pid, Tag}] ->
	        Pid ! {reply, Tag, Msg},
	        ets:delete(Pending, Serial),
	        {ok, State};
	    [_] ->
	        lager:debug("Unexpected message: ~p~n", [Serial]),
	        {error, unexpected_message, State}
    end;

handle_message(?TYPE_ERROR, Msg, #state{pending=Pending}=State) ->
    Serial = dbus_message:get_field_value(?HEADER_REPLY_SERIAL, Msg),
    case ets:lookup(Pending, Serial) of
	    [{Serial, Pid, Tag}] ->
	        Pid ! {error, Tag, Msg},
	        ets:delete(Pending, Serial),
	        {ok, State};
	    [_] ->
	        lager:debug("Unexpected message: ~p~n", [Serial]),
	        {error, unexpected_message, State}
    end;

handle_message(?TYPE_METHOD_CALL, Msg, #state{owner=Owner}=State) ->
    Owner ! {dbus_method_call, Msg, self()},
    {ok, State};

handle_message(?TYPE_SIGNAL, Msg, #state{owner=Owner}=State) ->
    Owner ! {dbus_signal, Msg, self()},
    {ok, State};

handle_message(Type, Msg, State) ->
    lager:debug("Ignore ~p ~p~n", [Type, Msg]),
    {error, unexpected_message, State}.

init_connection(Sock, Mechs) ->
    ok = dbus_transport:send(Sock, <<0>>),
    {ok, connected, #state{sock=Sock,
			   pending=ets:new(pending, [private]), 
			   mechs=Mechs}}.    

strip_eol(Bin) ->
    strip_eol(Bin, <<>>).

strip_eol(<<>>, Acc) ->
    Acc;
strip_eol(<<$\r, Rest/binary>>, Acc) ->
    strip_eol(Rest, Acc);
strip_eol(<<$\n, Rest/binary>>, Acc) ->
    strip_eol(Rest, Acc);
strip_eol(<<C:8, Rest/binary>>, Acc) ->
    strip_eol(Rest, <<C, Acc/binary>>).

parse_mechs(<<>>) ->
    {error, invalid_mechanisms};
parse_mechs(<<$\s, Rest/bits>>) ->
    parse_mechs(Rest);
parse_mechs(Bin) ->
    parse_mech(Bin, <<>>, []).

parse_mech(<<>>, SoFar, Mechs) ->
    case valid_mech(SoFar) of
	{ok, Mod} -> lists:reverse([Mod | Mechs]);
	error -> {error, {unsupported_mechanism, SoFar}}
    end;
parse_mech(<<$\s, Rest/bits>>, SoFar, Mechs) ->
    parse_mech(Rest, SoFar, Mechs);
parse_mech(<<C:8, Rest/bits>>, SoFar, Mechs) ->
    parse_mech(Rest, << SoFar/binary, C>>, Mechs).

valid_mech(<<"DBUS_COOKIE_SHA1">>) -> {ok, bus_auth_cookie_sha1};
valid_mech(<<"EXTERNAL">>) -> {ok, dbus_auth_external};
valid_mech(<<"KERBEROS_V4">>) -> error;
valid_mech(<<"SKEY">>) -> error;
valid_mech(_) -> error.
