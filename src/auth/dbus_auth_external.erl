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

init() ->
    {ok, <<"AUTH EXTERNAL 31303031\r\n">>}.

challenge(_, _) ->
    {error, invalid_challenge}.
