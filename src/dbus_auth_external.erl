%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc Implements EXTERNAL SASL mechanism.
%%%
%%% See <a href="https://tools.ietf.org/html/rfc4422" >RFC 4422</a> for complete
%%% specification.
%%%
%%% @end
%%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth_external).

-behaviour(dbus_auth).

%% dbus_auth callbacks
-export([init/0,
	 challenge/2]).

-define(cookie, <<"31303030">>).

%% @doc Initialize EXTERNAL mechanism.
%% 
%% @end
-spec init() -> {ok, binary()}.
init() ->
    Cookie = get_cookie(),
    {ok, <<"AUTH EXTERNAL ", Cookie/binary, "\r\n">>}.


%% @doc Not implemented: ANONYMOUS does not require challenge
%% @end
-spec challenge(binary(), any()) -> {error, invalid_challenge}.
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
