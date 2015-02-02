%%
%% @copyright 2015 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc
-module(dbus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").

-callback close(dbus_connection()) -> ok.
-callback call(dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
-callback cast(dbus_connection(), dbus_message()) -> ok | {error, term()}.

-export([close/1,
	 call/2,
	 cast/2]).

-spec close(dbus_connection()) -> ok.
close({Mod, Conn}) ->
    Mod:close(Conn).


-spec call(dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
call({Mod, Conn}, #dbus_message{}=Msg) ->
    Mod:call(Conn, Msg).

-spec cast(dbus_connection(), dbus_message()) -> ok | {error, term()}.
cast({Mod, Conn}, #dbus_message{}=Msg) ->
    Mod:cast(Conn, Msg).
