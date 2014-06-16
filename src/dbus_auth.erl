%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc authentication gen_server
%%
%% D-Bus outgoing transport authenticator
%%
%% Messages
%%
%% {auth_ok, Pid, Sock}
%% Sent after successfull authentication
-module(dbus_auth).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% api
-export([
	 start_link/1,
	 start_link/2,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {
	  owner,
	  sock,
	  type
	 }).

-define(SERVER, ?MODULE).

start_link(Sock) when is_pid(Sock) ->
    start_link(Sock, []).

start_link(DbusHost, DbusPort) when is_integer(DbusPort) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DbusHost, DbusPort, self()], []);

start_link(Sock, Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Sock, Options, self()], []).

stop() ->
    gen_server:cast(?SERVER, stop).


%%
%% gen_server callbacks
%%
init([DbusHost, DbusPort, Owner]) when is_integer(DbusPort) ->
    {ok, Sock} = dbus_tcp_conn:connect(DbusHost, DbusPort, [list, {packet, 0}]),
    init_common(Sock, Owner);
init([Sock, Options, Owner]) when is_list(Options) ->
    dbus_transport:change_owner(Sock, Owner, self()),
    init_common(Sock, Owner, Options).


init_common(Sock, Owner) ->
    init_common(Sock, Owner, []).

init_common(Sock, Owner, Options) ->
    User = os:getenv("USER"),
    ok = dbus_transport:send(Sock, <<0>>),

    Auth_type =
	case lists:keysearch(auth, 1, Options) of
	    {value, {auth, Type}} ->
	        Type;
	    false ->
	        detect
	end,

    AuthBin =
	case Auth_type of
	    external ->
	        <<"AUTH EXTERNAL 31303030\r\n">>;
	    cookie ->
		HexUser = dbus_hex:to(User),
		<<"AUTH DBUS_COOKIE_SHA1 ", HexUser/binary, "\r\n">>;
	    detect ->
	        <<"AUTH\r\n">>
	end,
    ok = dbus_transport:send(Sock, AuthBin),
    {ok, #state{sock=Sock,
		owner=Owner,
	        type=Auth_type}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    lager:error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    lager:error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({received, Sock, <<"DATA ", Line/binary>>}, #state{sock=Sock}=State) ->
    Bin = dbus_hex:from(strip_eol(Line)),
    case binary:split(Bin, <<$\ >>, [global]) of
	[Context, CookieId, ServerChallenge] ->
	    lager:debug("Data: ~p,~p,~p ~n", [Context, CookieId, ServerChallenge]),
	    case read_cookie(CookieId) of
		error ->
		    {stop, {error, {no_cookie, CookieId}}, State};
		{ok, Cookie} ->
		    Challenge = calc_challenge(),
		    Response = calc_response(ServerChallenge, Challenge, Cookie),
		    ok = dbus_transport:send(Sock, <<"DATA ", Response/binary, "\r\n">>),
		    {noreply, State}
	    end;
	_ ->
	    {stop, {error, invalid_data}, State}
    end;

handle_info({received, Sock, <<"OK ", Line/binary>>}, #state{sock=Sock}=State) ->
    Guid = strip_eol(Line),
    lager:debug("GUID ~p~n", [Guid]),
    ok = dbus_transport:setopts(Sock, [binary, {packet, raw}]),%, {recbuf, 8196}]),
    ok = dbus_transport:send(Sock, <<"BEGIN\r\n">>),
    Owner = State#state.owner,
    Owner ! {auth_ok, self(), Sock},
    {stop, normal, State};

handle_info({received, Sock, <<"REJECTED ", _Line/binary>>}, #state{sock=Sock}=State) ->
    ok = dbus_transport:close(Sock),
    {stop, {error, auth_rejected}, State};

handle_info(Info, State) ->
    lager:error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


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
