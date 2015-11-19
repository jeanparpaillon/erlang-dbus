%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 19 November 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_anonymous).

-behaviour(dbus_auth).

% dbus_auth callbacks
-export([init/0,
         challenge/2]).

init() ->
    {ok, <<"AUTH ANONYMOUS \r\n">>}.

challenge(_, _) ->
    {error, invalid_challenge}.

%%%
%%% Priv
%%%
