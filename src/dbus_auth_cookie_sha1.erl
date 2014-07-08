%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_cookie_sha1).
-compile([{parse_transform, lager_transform}]).

-behaviour(dbus_auth).

% dbus_auth callbacks
-export([init/0,
	challenge/2]).

init() ->
    lager:debug("Init DBUS_AUTH_COOKIE_SHA1 authentication~n", []),
    User = os:getenv("USER"),
    HexUser = dbus_hex:to(list_to_binary(User)),
    {continue, <<"AUTH DBUS_COOKIE_SHA1 ", HexUser/binary, "\r\n">>, waiting_challenge}.

challenge(Chall, waiting_challenge) ->
    case binary:split(Chall, <<$\ >>, [global]) of
	[Context, CookieId, ServerChallenge] ->
	    case read_cookie(Context, CookieId) of
		error ->
		    {error, no_cookie};
		{ok, Cookie} ->
		    Challenge = calc_challenge(),
		    {ok, calc_response(ServerChallenge, Challenge, Cookie)}
	    end;
	_ ->
            {error, invalid_challenge}
    end;
challenge(_, _) ->
    {error, invalid_challenge}.

%%%
%%% Priv
%%%
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


read_cookie(Context, CookieId) ->
    lager:debug("Reading DBUS cookie: (~p, ~p)~n", [Context, CookieId]),
    Name = filename:join([os:getenv("HOME"),
			 ".dbus-keyrings",
			 Context]),
    {ok, File} = file:open(Name, [read, binary]),
    Result = read_cookie2(File, CookieId),
    ok = file:close(File),
    Result.

read_cookie2(Device, CookieId) ->
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
			    read_cookie2(Device, CookieId)
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
