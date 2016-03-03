%%
%% @copyright 2015 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Describe callbacks for modules implementing connections.
%% 
%% Actually implemented by:
%% * @see dbus_peer_connection
%% * @see dbus_bus_connection
%% 
%% @end
-module(dbus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").

-callback close(dbus_connection()) -> ok.
-callback call(dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
-callback cast(dbus_connection(), dbus_message()) -> ok | {error, term()}.

-export([close/1,
	 call/2,
	 cast/2]).

%% @doc Close the connection
%% @end
-spec close(dbus_connection()) -> ok.
close({Mod, Conn}) ->
    Mod:close(Conn).


%% @doc Synchronously send a message
%% @end
-spec call(dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
call({Mod, Conn}, #dbus_message{}=Msg) ->
    Mod:call(Conn, Msg).

%% @doc Asychronously send a message
%% @end
-spec cast(dbus_connection(), dbus_message()) -> ok | {error, term()}.
cast({Mod, Conn}, #dbus_message{}=Msg) ->
    Mod:cast(Conn, Msg).
