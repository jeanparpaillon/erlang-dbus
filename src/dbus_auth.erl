%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth).

-callback init() ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, term()}.

-callback challenge(Chall :: binary(), State :: term()) ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, Reason :: term()}.
