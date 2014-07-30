%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_objects_manager).

-include("dbus_client.hrl").

-callback init(Manager :: dbus_proxy(), Env :: any()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback add_objects(Objects :: [{binary(), {binary(), term()}}], State :: term()) ->
    {ok, State :: term()} | {error, Reason :: term()}.
