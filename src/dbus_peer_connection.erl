%%
%% @copyright 2015 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Peer connections
%%
%% @end
-module(dbus_peer_connection).

-behaviour(gen_statem).
-behaviour(dbus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([start_link/1,
         start_link/2,
         start_link/3,
         set_controlling_process/2,
         auth/1]).

%% gen_dbus_connection callback
-export([close/1,
         call/2,
         cast/2]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         code_change/4,
         handle_event/4,
         terminate/3]).

-record(state, {serial   = 1,
                owner,
                service_reg,
                mod,
                sock,
                buf         = <<>>    :: binary(),
                pending               :: ets:tid(),
                waiting     = []      :: list(),
                mechs       = []      :: list(),
                round       = 1       :: number(),
		got_mechs   = false   :: boolean(),
                mech_state            :: term(),
                guid                  :: binary() | undefined,
                unix_fd               :: boolean() | undefined,
		side        = client  :: client | server
               }).

-define(ALL_MECHANISMS, [dbus_auth_external, dbus_auth_cookie_sha1, dbus_auth_anonymous]).
-define(TIMEOUT, 10000).


%% @equiv start_link(BusId, [list, {packet, 0}])
%% @end
-spec start_link(bus_id()) -> {ok, dbus_connection()} | {error, term()}.
start_link(BusId) ->
    start_link(BusId, [list, {packet, 0}]).


%% @doc Start a connection to a peer
%% @end
-spec start_link(bus_id(), pid() | list()) -> {ok, dbus_connection()} | {error, term()}.
start_link(BusId, Options) when is_record(BusId, bus_id),
                                is_list(Options) ->
    start_link(BusId, undefined, Options);

start_link(BusId, ServiceReg) when is_record(BusId, bus_id) ->
    start_link(BusId, ServiceReg, [list, {packet, 0}]).

start_link(BusId, ServiceReg, Options) when is_record(BusId, bus_id),
                                         is_list(Options) ->
    case gen_statem:start_link(?MODULE, [BusId, ServiceReg, Options, self()], []) of
        {ok, Pid} -> {ok, {?MODULE, Pid}};
        {error, Err} -> {error, Err}
    end.                           

%% @doc Close the connection
%% @end
-spec close(pid()) -> ok.
close(Conn) when is_pid(Conn) ->
    gen_statem:cast(Conn, close).


%% @doc Synchronously send a message
%% @end
-spec call(pid(), dbus_message()) -> {ok, term()} | {error, term()}.
call(Conn, #dbus_message{}=Msg) when is_pid(Conn) ->
    case gen_statem:call(Conn, {call, Msg}, infinity) of
        {ok, Tag} ->
            receive
                {reply, Tag, Res} ->
                    {ok, Res};
                {error, Tag, Res} ->
                    {error, Res}
            after ?TIMEOUT ->
                    ?error("DBUS timeout~n", []),
                    throw({error, timeout})
            end;
        {error, Err} ->
            throw({error, Err})
    end.


%% @doc Asynchronously send a message
%% @end
-spec cast(pid(), dbus_message()) -> ok | {error, term()}.
cast(Conn, #dbus_message{}=Msg) when is_pid(Conn) ->
    gen_statem:cast(Conn, Msg).


%% @doc Launch authentication on this connection
%% No message can be sent before authentication.
%%
%% @end
-spec auth(pid()) -> {ok, ConnexionId :: undefined | binary()} | {error, term()}.
auth(Conn) ->
    case gen_statem:call(Conn, auth) of
        authenticated ->
            ok;
        {ok, Tag} ->
            receive
                {authenticated, Tag} -> {ok, undefined};
                {error, Res} -> {error, Res};
                {dbus_signal, #dbus_message{body=[ConnId]}=Msg} ->
                    case dbus_message:match([{?FIELD_MEMBER, 'NameAcquired'},
                                             {?FIELD_INTERFACE, 'org.freedesktop.DBus'}], Msg) of
                        true -> {ok, ConnId};
                        false -> throw({error, {dbus, {dbus_signal, Msg}}})
                    end
            end
    end.


%% @doc Change controlling process for the connection.
%%
%% If called by someone else than current owner, `{error, unauthorized}' is returned.
%% @end
-spec set_controlling_process(Connection :: pid(), Client :: pid()) -> ok | {error, unauthorized}.
set_controlling_process(Conn, Client) ->
    case gen_statem:call(Conn, {set_controlling_process, Client}) of
      ok -> flush_messages(Client);
      Error -> Error
    end.

flush_messages(Client) ->
  receive
    {dbus_signal, Any} ->
      Client ! {dbus_signal, Any},
      flush_messages(Client)
  after
    0 -> ok
  end.

%%
%% gen_fsm callbacks
%%
init([#bus_id{scheme=tcp,options=BusOptions}, ServiceReg, Options, Owner]) ->
    {Host, Port} = case {lists:keysearch(host, 1, BusOptions),
                         lists:keysearch(port, 1, BusOptions)} of
                       {{value, {host, Host1}}, {value, {port,Port1}}} ->
                           {Host1, Port1};
                       _ ->
                           throw(no_host_or_port)
                   end,

    {ok, Sock} = dbus_transport_tcp:connect(Host, Port, Options),
    init_connection(Sock, ServiceReg, Owner);

init([#bus_id{scheme=unix, options=BusOptions}, ServiceReg, Options, Owner]) ->
    {ok, Sock} = dbus_transport_unix:connect(BusOptions, Options),
    init_connection(Sock, ServiceReg, Owner).


callback_mode() ->
    handle_event_function.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.


%% All states
handle_event({call, {Owner, _}=From}, {set_controlling_process, Client}, _StateName, #state{owner=Owner}=State) ->
    gen_statem:reply(From, ok),
    {keep_state, State#state{owner=Client}};

handle_event({call, From}, {set_controlling_process, _Client}, _StateName, _State) ->
    gen_statem:reply(From, {error, unauthorized}),
    keep_state_and_data;

handle_event(_, close, _StateName, #state{sock=Sock}=State) ->
    ok = dbus_transport:close(Sock),
    {stop, normal, State#state{sock=undefined}};

%% STATE: connected
handle_event(info, {received, Bin}, connected, State) ->
    ?debug("Unknown command: ~s", [Bin]),
    send_error(State, <<"Unknown command">>),
    {next_state, connected, State};

handle_event({call, {_Pid, Tag}=From}, auth, connected, #state{sock=Sock, mechs=[Mech|Rest]}=State) ->
    case Mech:init() of
        {ok, Resp} ->
            ?debug("DBUS auth: sending initial data~n", []),
            dbus_transport:send(Sock, << "AUTH ", Resp/binary, "\r\n" >>),
            gen_statem:reply(From, {ok, {self(), Tag}}),
            {next_state, waiting_for_ok, State#state{waiting=[From], mechs=Rest}};
        {continue, Resp, MechState} ->
            ?debug("DBUS auth: sending initial data (continue)~n", []),
            dbus_transport:send(Sock, << "AUTH ", Resp/binary, "\r\n" >>),
            gen_statem:reply(From, {ok, {self(), Tag}}),
            {next_state, waiting_for_data, State#state{waiting=[From], mechs=Rest, mech_state={Mech, MechState}}}
    end;

handle_event({call, From}, {call, _}, connected, _State) ->
    gen_statem:reply(From, {error, authentication_needed}),
    keep_state_and_data;

%% STATE: waiting_for_data
handle_event(info, {received, <<"DATA ", Line/binary>>}, waiting_for_data, State) ->
    process_data(Line, State);

handle_event(info, {received, <<"REJECTED", Line/binary>>}, waiting_for_data, State) ->
    process_rejected(Line, State);

handle_event(info, {received, <<"OK", Line/binary>>}, waiting_for_data, State) ->
    process_ok(Line, State);

handle_event(info, {received, <<"ERROR", Line/binary>>}, waiting_for_data, State) ->
    ?debug("state: waiting_for_data, error: ~s", [parse_error(Line)]),
    ok = send_cancel(State),
    {stop, waiting_for_reject, State};

handle_event(info, {received, Bin}, waiting_for_data, State) ->
    ?debug("Unknown command waiting for data: ~s", [Bin]),
    send_error(State, <<"Unknown command">>),
    {next_state, waiting_for_data, State};

handle_event({call, From}, {call, _}, waiting_for_data, _State) ->
    gen_statem:reply(From, {error, authentication_needed}),
    keep_state_and_data;

handle_event({call, {_Pid, Tag}=From}, auth, waiting_for_data, #state{waiting=Waiting}=State) ->
    gen_statem:reply(From, {ok, {self(), Tag}}),
    {keep_state, State#state{waiting=[From|Waiting]}};

%% STATE: waiting_for_ok
handle_event(info, {received, <<"REJECTED", Line/binary>>}, waiting_for_ok, State) ->
    process_rejected(Line, State);

handle_event(info, {received, <<"OK", Line/binary>>}, waiting_for_ok, State) ->
    process_ok(Line, State);

handle_event(info, {received, <<"DATA ", Data/binary>>}, waiting_for_ok, State) ->
    ?debug("Unexpected data: ~s", [Data]),
    ok = send_cancel(State),
    {next_state, waiting_for_ok, State};

handle_event(info, {received, <<"ERROR", Data/binary>>}, waiting_for_ok, State) ->
    ?debug("state: waiting_for_ok, error: ~s", [parse_error(Data)]),
    ok = send_cancel(State),
    {stop, waiting_for_ok, State};

handle_event(info, {received, Bin}, waiting_for_ok, State) ->
    ?debug("Unknown command waiting for ok: ~p", [Bin]),
    ok = send_error(State, <<"Unknown command">>),
    {next_state, waiting_for_ok, State};

handle_event({call, From}, {call, _}, waiting_for_ok, _State) ->
    gen_statem:reply(From, {error, authentication_needed}),
    keep_state_and_data;

handle_event({call, {_Pid, Tag}=From}, auth, waiting_for_ok, #state{waiting=Waiting}=State) ->
    gen_statem:reply(From, {ok, {self(), Tag}}),
    {keep_state, State#state{waiting=[From|Waiting]}};

%% STATE: waiting_for_reject
handle_event(info, {received, <<"REJECTED", Line/binary>>}, waiting_for_reject, State) ->
    process_rejected(Line, State);

handle_event(info, {received, Bin}, waiting_for_reject, State) ->
    ?debug("Unknown command waiting for reject: ~s", [Bin]),
    {stop, disconnect, State};

handle_event({call, From}, {call, _}, waiting_for_reject, _State) ->
    gen_statem:reply(From, {error, authentication_needed}),
    keep_state_and_data;

handle_event({call, {_Pid, Tag}=From}, auth, waiting_for_reject, #state{waiting=Waiting}=State) ->
    gen_statem:reply(From, {ok, {self(), Tag}}),
    {keep_state, State#state{waiting=[From|Waiting]}};

%% STATE: waiting_for_agree
handle_event(info, {received, <<"AGREE_UNIX_FD\r\n">>}, waiting_for_agree, #state{sock=Sock}=State) ->
    case dbus_transport:support_unix_fd(Sock) of
	true ->
	    ?debug("Succesfully negotiated UNIX FD passing~n", []),
	    begin_session(State#state{unix_fd=true});
	false ->
	    ?debug("Unknown command waiting for agree unix fd: AGREE_UNIX_FD", []),
	    ok = send_error(State, <<"Unknown command">>),
	    {next_state, waiting_for_agree}
    end;

handle_event(info, {received, <<"ERROR", Line/binary>>}, waiting_for_agree, #state{sock=Sock}=State) ->
    case dbus_transport:support_unix_fd(Sock) of
	true ->
	    ?debug("Failed to negotiate UNIX FD passing~n", []),
	    begin_session(State#state{unix_fd=false});
	false ->
	    ?debug("Unknown command waiting for agree unix fd : ~s", [Line]),
	    ok = send_error(State, <<"Unknown command">>),
	    {next_state, waiting_for_agreee, State}
    end;

handle_event(info, {received, Bin}, waiting_for_agree, State) ->
    ?debug("Unknown command waiting for agree unix fd : ~s", [Bin]),
    {next_state, waiting_for_agree, State};

handle_event({call, From}, {call, _}, waiting_for_agree, _State) ->
    gen_statem:reply(From, {error, authentication_needed}),
    keep_state_and_data;

handle_event({call, {_Pid, Tag}=From}, auth, waiting_for_agree, #state{waiting=Waiting}=State) ->
    gen_statem:reply(From, {ok, {self(), Tag}}),
    {keep_state, State#state{waiting=[From|Waiting]}};

%% STATE: authenticated
handle_event(info, {received, Data}, authenticated, #state{buf=Buf}=State) ->
    BufAndData = <<Buf/binary, Data/binary>>,
    case dbus_marshaller:unmarshal_data(BufAndData) of
        {ok, Msgs, Rest} ->
            case handle_messages(Msgs, State#state{buf=Rest}) of
                {ok, State2} ->
                    {next_state, authenticated, State2};
                {error, Err, State2} ->
                    {stop, {error, Err}, State2}
            end;
        more ->
            {next_state, authenticated, State#state{buf=BufAndData}}
    end;

handle_event({call, From}, auth, authenticated, _State) ->
    gen_statem:reply(From, authenticated),
    keep_state_and_data;

handle_event({call, {Pid, Tag}=From}, {call, #dbus_message{}=Msg}, authenticated,
              #state{sock=Sock, serial=S, pending=Pending}=State) ->
    Data = dbus_marshaller:marshal_message(dbus_message:set_serial(S, Msg)),
    ?debug("Calling ~p", [catch dbus_marshaller:unmarshal_data(list_to_binary(Data))]),
    true = ets:insert(Pending, {S, Pid, Tag}),
    ok = dbus_transport:send(Sock, Data),
    gen_statem:reply(From, {ok, {self(), Tag}}),
    {keep_state, State#state{serial=S+1}};

handle_event(cast, #dbus_message{}=Msg, authenticated,
              #state{sock=Sock, serial=S}=State) ->
    Data = dbus_marshaller:marshal_message(dbus_message:set_serial(S, Msg)),
    ?debug("Casting ~p", [catch dbus_marshaller:unmarshal_data(list_to_binary(Data))]),
    ok = dbus_transport:send(Sock, Data),
    {keep_state, State#state{serial=S+1}};

%% Other
handle_event(info, closed, _, State) ->
    ?debug("Connection closed...~n", []),
    {stop, closed, State};

%% Unhandled
handle_event(What, Evt, StateName, State) ->
    ?error("Unhandled event: ~p:~p @ ~p~n", [What, Evt, StateName]),
    {next_state, StateName, State}.


terminate(_Reason, _StateName, #state{sock=Sock}) ->
    case Sock of
        undefined -> ignore;
        _ -> dbus_transport:close(Sock)
    end,
    ok.

%%%
%%% Priv
%%%
handle_messages([], State) ->
    {ok, State};

handle_messages([#dbus_message{header=#dbus_header{type=Type}}=Msg | R], State) ->
    ?debug("Received ~p~n", [Msg]),
    case handle_message(Type, Msg, State) of
        {ok, State2} ->
            handle_messages(R, State2);
        {error, _, _}=Err ->
	    Err
    end.

handle_message(?TYPE_METHOD_RETURN, Msg, #state{pending=Pending}=State) ->
    Serial = dbus_message:get_field(?FIELD_REPLY_SERIAL, Msg),
    case ets:lookup(Pending, Serial) of
        [{Serial, Pid, Tag}] ->
            Pid ! {reply, {self(), Tag}, Msg},
            ets:delete(Pending, Serial),
	    %%?debug("<~p> returns: ~p", [Serial, Msg#dbus_message.body]),
            {ok, State};
        _ ->
            ?debug("Unexpected message: ~p~n", [Msg]),
            {error, unexpected_message, State}
    end;

handle_message(?TYPE_ERROR, Msg, #state{pending=Pending}=State) ->
    Serial = dbus_message:get_field(?FIELD_REPLY_SERIAL, Msg),
    case ets:lookup(Pending, Serial) of
        [{Serial, Pid, Tag}] ->
            Pid ! {error, {self(), Tag}, Msg},
            ets:delete(Pending, Serial),
	    ?debug("<~p> error: ~p~n", [Serial, Msg#dbus_message.body]),
            {ok, State};
        _Err ->
	    case dbus_message:is_error(Msg, <<"org.freedesktop.DBus.Error.NoReply">>) of
		true ->
		    ?debug("Ignoring NoReply on unknown serial~n", []),
		    {ok, State};
		false ->
		    ?debug("Unexpected message: ~p~n", [Msg]),
		    {error, unexpected_message, State}
	    end
    end;

handle_message(?TYPE_METHOD_CALL, Msg, #state{service_reg=undefined}=State) ->
    dbus_service_reg ! {dbus_method_call, Msg, {?MODULE, self()}},
    {ok, State};

handle_message(?TYPE_METHOD_CALL, Msg, #state{service_reg=ServiceReg}=State) ->
    ServiceReg ! {dbus_method_call, Msg, {?MODULE, self()}},
    {ok, State};

handle_message(?TYPE_SIGNAL, Msg, #state{owner=Owner}=State) ->
    Owner ! {dbus_signal, Msg},
    {ok, State};

handle_message(Type, Msg, State) ->
    ?debug("Ignore ~p ~p~n", [Type, Msg]),
    {error, unexpected_message, State}.

init_connection(Sock, ServiceReg, Owner) ->
    ok = dbus_transport:send(Sock, <<0>>),
    {ok, connected, #state{sock=Sock,
                           service_reg=ServiceReg,
                           pending=ets:new(pending, [private]),
                           mechs=?ALL_MECHANISMS,
                           owner=Owner}}.


process_ok(Data, #state{sock=Sock, waiting=Waiting}=State) ->
    Guid = parse_guid(Data),
    ?debug("Got GUID '~s' from the server~n", [Guid]),
    case dbus_transport:support_unix_fd(Sock) of
        true ->
            ok = dbus_transport:send(Sock, <<"NEGOTIATE_UNIX_FD\r\n">>),
            {next_state, waiting_for_agree, State#state{guid=Guid, mech_state=undefined}};
        false ->
	    ?debug("not negotiating unix fd passing, since not possible~n", []),
            ok = dbus_transport:send(Sock, <<"BEGIN\r\n">>),
            lists:foreach(fun ({Pid, Tag}) ->
                                  Pid ! {authenticated, {self(), Tag}}
                          end, Waiting),
            ok = dbus_transport:set_raw(Sock, true),
            {next_state, authenticated, State#state{waiting=[], guid=Guid, unix_fd=false, mech_state=undefined}}
    end.


process_data(Data, #state{sock=Sock, mech_state={Mech, MechState}}=State) ->
    case Mech:challenge(Data, MechState) of
        {ok, Resp} ->
            ?debug("DBUS auth: answering challenge~n", []),
            dbus_transport:send(Sock, << "DATA ", Resp/binary, "\r\n" >>),
            {next_state, waiting_for_ok, State};
        {continue, Resp, MechState} ->
            ?debug("DBUS auth: answering challenge (continue)~n", []),
            dbus_transport:send(Sock, << "DATA ", Resp/binary, "\r\n" >>),
            {next_state, waiting_for_data, State#state{mech_state={Mech, MechState}}};
        {error, Err} ->
            ?debug("Error with authentication challenge: ~p~n", [Err]),
            ok = dbus_transport:send(Sock, <<"CANCEL\r\n">>),
            {next_state, waiting_for_reject, State}
    end.

process_rejected(_Data, #state{mechs=[], got_mechs=true}=State) ->
    ?debug("~s: Disconnecting because we are out of mechanisms to try using~n", [State#state.side]),
    {stop, disconnect, State};

process_rejected(Data, #state{mechs=[], got_mechs=false}=State) ->
    case record_mechs(Data, State) of
	State2 = #state{mechs=[]} ->
	    process_rejected(Data, State2);
	State2 ->
	    try_next_auth(State2)
    end;

process_rejected(_Data, State) ->
    try_next_auth(State).


try_next_auth(#state{sock=Sock, mechs=[ Mech | Rest ]}=State) ->
    ?debug("~s: Trying mechanism ~s~n", [State#state.side, Mech]),
    try Mech:init() of
        {ok, Resp} ->
            ?debug("DBUS auth: waiting for OK~n", []),
            dbus_transport:send(Sock, << "AUTH ", Resp/binary, "\r\n" >>),
            {next_state, waiting_for_ok, State#state{mechs=Rest}};
        {continue, Resp, MechState} ->
            ?debug("DBUS auth: waiting for Data~n", []),
            dbus_transport:send(Sock, << "AUTH ", Resp/binary, "\r\n" >>),
            {next_state, waiting_for_data, State#state{mechs=Rest, mech_state={Mech, MechState}}};
        {error, Err} ->
            ?error("Error initializing authentication mechanism (~s): ~p", [Mech, Err]),
            try_next_auth(State#state{mechs=Rest})
    catch _Cls:Err ->
	    ?error("Exception initializing authentication mechanism (~s): ~p", [Mech, Err]),
            try_next_auth(State#state{mechs=Rest})
    end.


send_cancel(#state{sock=Sock}) ->
    dbus_transport:send(Sock, <<"CANCEL\r\n">>).


send_error(#state{sock=Sock}, Err) ->
    dbus_transport:send(Sock, <<"ERROR \"", Err/binary, "\"\r\n">>).


begin_session(#state{sock=Sock, waiting=Waiting}=State) ->
    ok = dbus_transport:send(Sock, <<"BEGIN\r\n">>),
    lists:foreach(fun ({Pid, Tag}) ->
                          Pid ! {authenticated, {self(), Tag}}
                  end, Waiting),
    ok = dbus_transport:set_raw(Sock, true),
    {next_state, authenticated, State#state{waiting=[]}}.


parse_guid(<< $\s, Rest/binary >>) -> parse_guid(Rest);

parse_guid(Rest) -> pg(Rest, <<>>).

pg(<< $\r, _Rest/binary >>, Acc) -> Acc;

pg(<< C:8, Rest/binary >>, Acc) -> pg(Rest, << Acc/binary, C >>).


record_mechs(<<>>, S) ->
    S#state{got_mechs=true};

record_mechs(<<$\s, Rest/bits>>, S) ->
    record_mechs(Rest, S);

record_mechs(Bin, #state{mechs=Mechs}=S) ->
    case parse_mech(Bin) of
	{unsupported, Name, Rest} ->
	    ?debug("~s: Server offered mechanism \"~s\" that we don't know how to use~n", [S#state.side, Name]),
            record_mechs(Rest, S);
        {Mech, Name, Rest} ->
	    case lists:member(Mech, ?ALL_MECHANISMS) of
		true ->
		    ?debug("~s: Already tried mechanism ~s; not adding to list we will try~n", [S#state.side, Name]),
		    record_mechs(Rest, S);
		false ->
		    ?debug("~s: Adding mechanism ~s to list we will try~n", [S#state.side, Name]),
		    record_mechs(Rest, S#state{mechs=[ Mech | Mechs ]})
	    end
    end.


parse_mech(Bin) ->
    parse_mech(Bin, <<>>).


parse_mech(<<>>, Acc) ->
    valid_mech(Acc, <<>>);

parse_mech(<<$\s, Rest/bits>>, Acc) ->
    valid_mech(Acc, Rest);

parse_mech(<<C, _/bits>>, Acc) when $\r =:= C
				    orelse $\n =:= C ->
    valid_mech(Acc, <<>>);

parse_mech(<<C:8, Rest/bits>>, SoFar) ->
    parse_mech(Rest, << SoFar/binary, C>>).


valid_mech(<<"DBUS_COOKIE_SHA1">> = Name, Rest) -> {dbus_auth_cookie_sha1, Name, Rest};

valid_mech(<<"EXTERNAL">> = Name, Rest) -> {dbus_auth_external, Name, Rest};

valid_mech(<<"ANONYMOUS">> = Name, Rest) -> {dbus_auth_anonymous, Name, Rest};

valid_mech(<<"KERBEROS_V4">> = Name, Rest) -> {unsupported, Name, Rest};

valid_mech(<<"SKEY">> = Name, Rest) -> {unsupported, Name, Rest};

valid_mech(Name, Rest) -> {unsupported, Name, Rest}.


parse_error(Bin) -> pe_begin(Bin).


pe_begin(<< $\s, Rest/binary >>) -> pe_begin(Rest);

pe_begin(<< $", Rest/binary >>) -> pe_text(Rest, <<>>).


pe_text(<< $", _Rest/binary >>, Acc) ->
    Acc;

pe_text(<< C, Rest/binary >>, Acc) ->
    pe_text(Rest, << Acc/binary, C >>).




%%%
%%% eunit
%%%
-ifdef(TEST).
-endif.
