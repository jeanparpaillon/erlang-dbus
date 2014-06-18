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
	 start_link/3,
	 close/1,
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
	 challenged/3,
	 authenticated/3]).
-export([connected/2,
	 challenged/2,
	 authenticated/2]).

-record(state, {serial   = 0,
		mod,
		sock,
		buf      = <<>>,
		owner,
		state,
		pending           :: term(),  % tid()
		waiting  = []}).

-define(TIMEOUT, 5000).

start_link(BusId) ->
    start_link(BusId, []).

start_link(BusId, Options) ->
    start_link(BusId, Options, self()).

start_link(BusId, Options, Owner) when is_record(BusId, bus_id),
				       is_list(Options),
				       is_pid(Owner) ->
    gen_fsm:start_link(?MODULE, [BusId, Options, Owner], []).

close(Conn) ->
    gen_fsm:send_all_state_event(Conn, close).

-spec call(pid(), term()) -> {ok, term()} | {error, term()}.
call(Conn, Header) ->
    gen_fsm:sync_send_event(Conn, Header),
    receive
	{reply, Res} ->
	    {ok, Res};
	{error, Res} ->
	    {error, Res}
    after ?TIMEOUT -> {error, timeout}
    end.	    

-spec cast(pid(), term()) -> ok | {error, term()}.
cast(Conn, Header) ->
    gen_fsm:send_event(Conn, Header).

%%
%% gen_fsm callbacks
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

    {ok, Sock} = dbus_transport_tcp:connect(Host, Port, Options),
    ok = auth(Sock, [{auth, cookie}]),
    {ok, connected, #state{sock=Sock, owner=Owner, pending=ets:new(pending, [private])}};

init([#bus_id{scheme=unix, options=BusOptions}, Options, Owner]) ->
    true = link(Owner),
    {ok, Sock} = dbus_transport_unix:connect(BusOptions, Options),
    ok = auth(Sock, [{auth, external}]),
    {ok, connected, #state{sock=Sock, owner=Owner, pending=ets:new(pending, [private])}}.


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


handle_info({received, <<"DATA ", Line/binary>>}, connected, #state{sock=Sock}=State) ->
    Bin = dbus_hex:from(strip_eol(Line)),
    case binary:split(Bin, <<$\ >>, [global]) of
	[Context, CookieId, ServerChallenge] ->
	    lager:debug("Received authentication data: ~p,~p,~p ~n", [Context, CookieId, ServerChallenge]),
	    case read_cookie(CookieId) of
		error ->
		    {stop, {error, {no_cookie, CookieId}}, State};
		{ok, Cookie} ->
		    Challenge = calc_challenge(),
		    Response = calc_response(ServerChallenge, Challenge, Cookie),
		    ok = dbus_transport:send(Sock, <<"DATA ", Response/binary, "\r\n">>),
		    {next_state, challenged, State}
	    end;
	_ ->
	    {stop, {error, invalid_data}, State}
    end;

handle_info({received, <<"REJECTED", _Line/binary>>}, connected, #state{sock=Sock}=State) ->
    ok = dbus_transport:close(Sock),
    {stop, {error, auth_rejected}, State};

handle_info({received, <<"OK", Line/binary>>}, challenged, #state{sock=Sock}=State) ->
    Guid = strip_eol(Line),
    lager:debug("GUID ~p~n", [Guid]),
    ok = dbus_transport:setopts(Sock, [binary, {packet, raw}]),%, {recbuf, 8196}]),
    ok = dbus_transport:send(Sock, <<"BEGIN\r\n">>),
    flush_waiting(State),
    {next_state, connected, State};

handle_info({received, Data}, StateName, #state{buf=Buf}=State) ->
    {ok, Msgs, Rest} = dbus_marshaller:unmarshal_data(<<Buf/binary, Data/binary>>),
    case handle_messages(Msgs, State#state{buf=Rest}) of
	{ok, State2} ->
	    {next_state, StateName, State2};
	{error, Err, State2} ->
	    {stop, {error, Err}, State2}
    end;

handle_info(Info, _StateName, State) ->
    lager:error("Unhandled info in: ~p~n", [Info]),
    {stop, {error, unexpected_message}, State}.


terminate(_Reason, _StateName, #state{sock=Sock}) ->
    case Sock of
	undefined -> ignore;
	_ -> dbus_connection:close(Sock)
    end,
    terminated.

%%%
%%% gen_fsm states
%%%
connected(_Evt, _From, State) ->
    {reply, {error, waiting_authentication}, connected, State}.

challenged(_Evt, _From, State) ->
    {reply, {error, waiting_authentication}, challenged, State}.

authenticated(#header{}=Header, _From, #state{sock=Sock, serial=S}=State) ->
    {ok, Data} = dbus_marshaller:marshal_message(Header#header{serial=S}),
    ok = dbus_transport:send(Sock, Data),
    {reply, ok, authenticated, State}.


connected(Evt, #state{waiting=W}=State) ->
    {next_state, connected, State#state{waiting=[Evt|W]}}.

challenged(Evt, #state{waiting=W}=State) ->
    {next_state, challenged, State#state{waiting=[Evt|W]}}.

authenticated(#header{}=Header, #state{sock=Sock, serial=S}=State) ->
    {ok, Data} = dbus_marshaller:marshal_message(Header#header{serial=S}),
    ok = dbus_transport:send(Sock, Data),
    {noreply, authenticated, State#state{serial=S+1}}.

%%%
%%% Priv
%%%
handle_messages([], State) ->
    {ok, State};
handle_messages([#header{type=Type}=Header | R], State) ->
    case handle_message(Type, Header, State) of
	{ok, State2} ->
	    handle_messages(R, State2);
	{error, Err} ->
	    {error, Err}
    end.


handle_message(?TYPE_METHOD_RETURN, Header, #state{pending=Pending}=State) ->
    {_, SerialHdr} = dbus_message:header_fetch(?HEADER_REPLY_SERIAL, Header),
    Serial = SerialHdr#variant.value,
    case ets:lookup(Pending, Serial) of
	[{Serial, Pid}] ->
	    Pid ! {reply, Header},
	    ets:delete(Pending, Serial),
	    {ok, State};
	[_] ->
	    lager:debug("Unexpected message: ~p~n", [Serial]),
	    {error, unexpected_message, State}
    end;

handle_message(?TYPE_ERROR, Header, #state{pending=Pending}=State) ->
    {_, SerialHdr} = dbus_message:header_fetch(?HEADER_REPLY_SERIAL, Header),
    Serial = SerialHdr#variant.value,
    case ets:lookup(Pending, Serial) of
	[{Serial, Pid}] ->
	    Pid ! {error, Header},
	    ets:delete(Pending, Serial),
	    {ok, State};
	[_] ->
	    lager:debug("Unexpected message: ~p~n", [Serial]),
	    {error, unexpected_message, State}
    end;

handle_message(?TYPE_METHOD_CALL, Header, #state{owner=Owner}=State) ->
    Owner ! {dbus_method_call, Header, self()},
    {ok, State};

handle_message(?TYPE_SIGNAL, Header, #state{owner=Owner}=State) ->
    Owner ! {dbus_signal, Header, self()},
    {ok, State};

handle_message(Type, Header, State) ->
    lager:debug("Ignore ~p ~p~n", [Type, Header]),
    {error, unexpected_message, State}.


flush_waiting(#state{waiting=[]}) ->
    ok;

flush_waiting(#state{sock=Sock, serial=S, waiting=[Header | W]}=State) ->
    {ok, Data} = dbus_marshaller:marshal_message(Header#header{serial=S}),
    ok = dbus_transport:send(Sock, Data),
    flush_waiting(State#state{serial=S+1, waiting=W}).


auth(Sock, Opts) ->
    User = os:getenv("USER"),
    ok = dbus_transport:send(Sock, <<0>>),
    AuthType = proplists:get_value(auth, Opts, detect),
    AuthBin = 
	case AuthType of
	    external ->
	        <<"AUTH EXTERNAL 31303030\r\n">>;
	    cookie ->
		HexUser = dbus_hex:to(User),
		<<"AUTH DBUS_COOKIE_SHA1 ", HexUser/binary, "\r\n">>;
	    detect ->
	        <<"AUTH\r\n">>
	end,
    lager:debug("Sending DBUS authentication~n", []),
    dbus_transport:send(Sock, AuthBin).


calc_challenge() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    UnixTime = MegaSecs * 1000000 + Secs,
    BinTime = integer_to_binary(UnixTime),
    dbus_hex:to(<<"Hello ", BinTime/binary>>).


calc_response(ServerChallenge, Challenge, Cookie) ->
    A1 = ServerChallenge ++ ":" ++ Challenge ++ ":" ++ Cookie,
    lager:debug("A1: ~p~n", [A1]),
    DigestHex = dbus_hex:to(crypto:hash(sha, A1)),
    <<Challenge/binary, " ", DigestHex/binary>>.


read_cookie(CookieId) ->
    Home = os:getenv("HOME"),
    Name = Home ++ "/.dbus-keyrings/org_freedesktop_general",
    {ok, File} = file:open(Name, [read, binary]),
    Result = read_cookie(File, CookieId),
    ok = file:close(File),
    Result.


read_cookie(Device, CookieId) ->
    case io:get_line(Device, "") of
	eof ->
	    error;
	Line ->
	    case binary:split(strip_eol(Line), <<$\ >>, [global]) of
		[CookieId1, _Time, Cookie] ->
		    if
			CookieId == CookieId1 ->
			    {ok, Cookie};
			true ->
			    read_cookie(Device, CookieId)
		    end;
		_ ->
		    error
	    end
    end.

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
