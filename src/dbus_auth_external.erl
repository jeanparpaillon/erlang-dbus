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

-include("dbus.hrl").

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
    {ok, <<"EXTERNAL ", Cookie/binary>>}.


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
	{ok, system_user} -> 
			get_current_user_uid();
	{ok, Val} when is_list(Val) ->
	    list_to_binary(Val);
	{ok, Val} when is_binary(Val) ->
	    Val;
	{ok, Val} when is_integer(Val) ->
	    integer_to_binary(Val)
    end.

get_current_user_uid() ->
	?debug("Getting current USER env~n", []),
	case os:getenv("USER") of
			false ->
					?debug("Missing USER env~n", []),
					?cookie;
			User ->
					% list_to_binary(User)
					resolve_user_name(User)
	end.

resolve_user_name(User) ->
	?debug("Resolving uid for user ~s~n", [User]),
	Cmd = io_lib:format("id -u ~s", [User]),
	case run_command(Cmd) of
		{0, Result} -> 
			Uid = string:trim(Result),
			?debug("Successfully resolved user ~s to uid ~s~n", [User, Uid]),
			dbus_hex:encode(list_to_binary(Uid));
		{1, Reason} ->
			?debug("Failed to resolve user ~s to uid: ~s~n", [User, Reason]),
			?cookie;
		_ -> ?cookie
	end.

run_command(Command) ->
	Opt = [stream, exit_status, use_stdio,
			   stderr_to_stdout, in, eof],
	P = open_port({spawn, Command}, Opt),
	get_data(P, []).

get_data(P, D) ->
    receive
	{P, {data, D1}} ->
	    get_data(P, [D1|D]);
	{P, eof} ->
	    port_close(P),    
	    receive
		{P, {exit_status, N}} ->
		    {N, normalize(lists:flatten(lists:reverse(D)))}
	    end
    end.

normalize([$\r, $\n | Cs]) ->
    [$\n | normalize(Cs)];
normalize([$\r | Cs]) ->
    [$\n | normalize(Cs)];
normalize([C | Cs]) ->
    [C | normalize(Cs)];
normalize([]) ->
    [].