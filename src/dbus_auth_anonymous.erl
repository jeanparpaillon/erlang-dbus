%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc Implements ANONYMOUS authentication
%%%
%%% See <a href="https://tools.ietf.org/html/rfc2245" >RFC 2245</a>
%%% for complete specification.
%%%
%%% @end
%%% Created : 19 November 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_anonymous).

-behaviour(dbus_auth).

%% dbus_auth callbacks
-export([init/0,
         challenge/2]).

%% @doc Init ANONYMOUS mechanism
%% @end
-spec init() -> {ok, binary()}.
init() ->
    {ok, <<"AUTH ANONYMOUS\r\n">>}.


%% @doc Not implemented: ANONYMOUS does not require challenge
%% @end
-spec challenge(binary(), any()) -> {error, invalid_challenge}.
challenge(_, _) ->
    {error, invalid_challenge}.
