%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc Implements COOKIE_SHA1 authentication mechanism
%%%
%%% See <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#auth-mechanisms-sha" >D-Bus Specification</a>
%%% for complete specification.
%%%
%%% @end
%%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_cookie_sha1).

-include("dbus.hrl").

-behaviour(dbus_auth).

%% dbus_auth callbacks
-export([init/0,
         challenge/2]).

%% @doc Initialize DBUS_AUTH_COOKIE_SHA1 authentication
%% @end
init() ->
    ?debug("Init DBUS_AUTH_COOKIE_SHA1 authentication~n", []),
    case os:getenv("USER") of
        false ->
            ?error("DBUS_AUTH_COOKIE_SHA1 can not be used without USER env", []),
            {error, invalid_user};
        User ->
            HexUser = dbus_hex:encode(list_to_binary(User)),
            {continue, <<"DBUS_COOKIE_SHA1 ", HexUser/binary>>, waiting_challenge}
    end.


%% @doc Answer challenge
%% @end
challenge(HexChall, waiting_challenge) ->
    Chall = dbus_hex:decode(HexChall),
    ?debug("DBUS_COOKIE_SHA1 challenge: ~p", [Chall]),
    case binary:split(Chall, [<< $\s >>], [global]) of
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
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    UnixTime = MegaSecs * 1000000 + Secs,
    BinTime = integer_to_binary(UnixTime),
    dbus_hex:encode(<<"Hello ", BinTime/binary>>).


calc_response(ServerChallenge, Challenge, Cookie) ->
    A1 = << ServerChallenge/binary, $:, Challenge/binary, $:, Cookie/binary >>,
    DigestHex = dbus_hex:encode(crypto:hash(sha, A1)),
    dbus_hex:encode(<<Challenge/binary, " ", DigestHex/binary>>).


read_cookie(Context, CookieId) ->
    ?debug("Reading DBUS cookie: context=~s, cookie_id=~s~n", [Context, CookieId]),
    Name = filename:join([os:getenv("HOME"), ".dbus-keyrings", Context]),
    case file:open(Name, [read, binary]) of
        {ok, File} ->
            Result = read_cookie2(File, CookieId),
            ok = file:close(File),
            Result;
        {error, Err}->
            {error, Err}
    end.

read_cookie2(Device, CookieId) ->
    case file:read_line(Device) of
        eof ->
            {error, no_cookie};
        {ok, Line} ->
            case binary:split(Line, [<< $\s >>], [global]) of
                [CookieId, _Time, Cookie] ->
		    {ok, strip(Cookie)};
		[_Id, _Time, _] ->
		    read_cookie2(Device, CookieId);
                Else ->
                    {error, {malformed_cookie, Else}}
            end
    end.


strip(Bin) ->
    [S | _] = binary:split(Bin, [<<$\n>>]),
    S.
