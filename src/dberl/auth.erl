%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc authentication gen_server
%%
%% D-Bus outgoing transport authenticator
%%
%% Messages
%%
%% {auth_ok, Pid, Sock}
%% Sent after successfull authentication

-module(dberl.auth).

-import(crypto).
-import(error_logger).
-import(file).
-import(gen_server).
%%-import(hex).
-import(io).
-import(lists).
-import(os).

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
	  sock
	 }).

-define(SERVER, ?MODULE).

start_link(DbusHost, DbusPort) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DbusHost, DbusPort, self()], []).

start_link(Sock) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Sock, self()], []).

stop() ->
    gen_server:cast(?SERVER, stop).


%%
%% gen_server callbacks
%%
init([DbusHost, DbusPort, Owner]) ->
    {ok, Sock} = tcp_conn:connect(DbusHost, DbusPort, [list, {packet, 0}]),
    init_common(Sock, Owner);
init([Sock, Owner]) ->
    transport:change_owner(Sock, Owner, self()),
    init_common(Sock, Owner).


init_common(Sock, Owner) ->
    User = os:getenv("USER"),
    ok = transport:send(Sock, <<0>>),
    ok = transport:send(Sock, ["AUTH DBUS_COOKIE_SHA1 ",
			     list_to_hexlist(User),
			     "\r\n"]),
    {ok, #state{sock=Sock,
		owner=Owner}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({received, Sock, "DATA " ++ Line}, #state{sock=Sock}=State) ->
    Data = hex_to_list(strip_eol(Line, [])),
    [Context, CookieId, ServerChallenge] = split(Data, $\ ),
    io:format("Data: ~p,~p,~p ~n", [Context, CookieId, ServerChallenge]),

    case read_cookie(CookieId) of
	error ->
	    {stop, {error, {no_cookie, CookieId}}, State};
	{ok, Cookie} ->
	    Challenge = calc_challenge(),
	    Response = calc_response(ServerChallenge, Challenge, Cookie),
	    ok = transport:send(Sock, ["DATA " ++ Response ++ "\r\n"]),

	    {noreply, State}
    end;

handle_info({received, Sock, "OK " ++ Line}, #state{sock=Sock}=State) ->
    Guid = strip_eol(Line, []),
    error_logger:info_msg("GUID ~p~n", [Guid]),
    ok = transport:setopts(Sock, [binary, {packet, raw}]),%, {recbuf, 8196}]),
    ok = transport:send(Sock, ["BEGIN\r\n"]),

    Owner = State#state.owner,
    Owner ! {auth_ok, self(), Sock},
    {stop, normal, State};

handle_info({received, Sock, "REJECTED " ++ _Line}, #state{sock=Sock}=State) ->
    ok = transport:close(Sock),
%%     Owner = State#state.owner,
%%     Owner ! {auth_rejected, self()},
    {stop, {error, auth_rejected}, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


calc_challenge() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    UnixTime = MegaSecs * 1000000 + Secs,
    Challenge = list_to_hexlist("Hello " ++ integer_to_list(UnixTime)),
    Challenge.

calc_response(ServerChallenge, Challenge, Cookie) ->
    A1 = ServerChallenge ++ ":" ++ Challenge ++ ":" ++ Cookie,
    io:format("A1: ~p~n", [A1]),
    Digest = crypto:sha(A1),
    DigestHex = list_to_hexlist(binary_to_list(Digest)),
    Response = list_to_hexlist(Challenge ++ " " ++ DigestHex),
    Response.


strip_eol([], Res) ->
    Res;
strip_eol([$\r|R], Res) ->
    strip_eol(R, Res);
strip_eol([$\n|R], Res) ->
    strip_eol(R, Res);
strip_eol([E|R], Res) ->
    strip_eol(R, Res ++ [E]).


list_to_hexlist(List) ->
    Fun = fun(E) ->
		  byte_to_hex(E)
	  end,
    
    lists:flatten(lists:map(Fun, List)).

byte_to_hex(E) ->
    High = E div 16,
    Low = E - High * 16,

    [nibble_to_hex(High), nibble_to_hex(Low)].

nibble_to_hex(Nibble) when Nibble >= 0, Nibble =< 9 ->
    Nibble + $0;
nibble_to_hex(Nibble) when Nibble >= 10, Nibble =< 15  ->
    Nibble - 10 + $a.
   

hex_to_list(Hex) ->
    hex_to_list(Hex, []).

hex_to_list([], List) ->
    List;
hex_to_list([H1, H2|R], List) ->
    List1 = List ++ [hex:from([H1, H2])],
    hex_to_list(R, List1).

read_cookie(CookieId) ->
    Home = os:getenv("HOME"),
    Name = Home ++ "/.dbus-keyrings/org_freedesktop_general",
    {ok, File} = file:open(Name, [read]),
    Result = read_cookie(File, CookieId),
    ok = file:close(File),
    Result.

read_cookie(Device, CookieId) ->
    case io:get_line(Device, "") of
	eof ->
	    error;
	Line ->
	    [CookieId1, _Time, Cookie] = split(strip_eol(Line, []), $\ ),
	    if
		CookieId == CookieId1 ->
		    {ok, Cookie};
		true ->
		    read_cookie(Device, CookieId)
	    end
    end.

split(List, Char) when is_list(List),
		       is_integer(Char) ->
    split(List, Char, "", []).

split([], _Char, Str, Res) ->
    Res ++ [Str];
split([Char|R], Char, Str, Res) ->
    split(R, Char, "", Res ++ [Str]);
split([C|R], Char, Str, Res) ->
    split(R, Char, Str ++ [C], Res).
