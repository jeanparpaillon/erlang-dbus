%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_external).
-compile([{parse_transform, lager_transform}]).

-behaviour(dbus_auth).

% dbus_auth callbacks
-export([init/0,
	 challenge/2]).

-define(cookie, <<"31303030">>).

init() ->
    Cookie = get_cookie(),
    {ok, <<"AUTH EXTERNAL ", Cookie/binary, "\r\n">>}.

challenge(_, _) ->
    {error, invalid_challenge}.

%%%
%%% Priv
%%%
get_cookie() ->
    case application:get_env(dbus, external_cookie) of
	undefined -> ?cookie;
	{ok, Val} when is_list(Val) ->
	    list_to_binary(Val);
	{ok, Val} when is_binary(Val) ->
	    Val;
	{ok, Val} when is_integer(Val) ->
	    integer_to_binary(Val)
    end.
