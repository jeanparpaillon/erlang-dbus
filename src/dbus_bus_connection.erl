%%%
%%% @copyright 2014 Jean Parpaillon
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @doc Implements the connection to a D-Bus bus.
%%%
%%% Actually, the following addresses classes are supported:
%%% * `unix'
%%% * `tcp'
%%%
%%% Other classes are _ignored_, in particular `kernel'.
%%%
%%% @end
%%% Created : 22 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_bus_connection).

-behaviour(dbus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").
-include("dbus_dbus.hrl").
-include("dbus_introspectable.hrl").

-export([get_bus_id/1,
	 connect/1,
     connect/2,
     get_unique_name/1]).

%% dbus_connection callbacks
-export([close/1,
	 call/2,
	 cast/2]).

-define(DEFAULT_BUS_SYSTEM, #bus_id{scheme=unix,options=[{path, "/var/run/dbus/system_bus_socket"}]}).
-define(SESSION_ENV, "DBUS_SESSION_BUS_ADDRESS").
-define(SERVER_DELIM, $;).
-define(TRANSPORT_DELIM, $:).
-define(PARAM_DELIM, $,).
-define(KEY_DELIM, $=).


%% @doc Retrieve a bus_id from well-known names
%%
%% @end
-spec get_bus_id(dbus_known_bus()) -> bus_id() | {unsupported, [bus_id()]}.
get_bus_id(session) ->
    Ids = env_to_bus_id(),
    case lists:filter(fun (#bus_id{scheme=unix}) -> true;
			  (#bus_id{scheme=tcp}) -> true;
			  (_) -> false
		      end, Ids) of
	[] -> {unsupported, Ids};
	[Id | _] -> Id
    end;

get_bus_id(system) ->
    ?DEFAULT_BUS_SYSTEM.


%% @doc Start a proxy to a bus.
%%
%% @end
-spec connect(bus_id() | dbus_known_bus()) -> {ok, dbus_connection()} | {error, term()}.
connect(#bus_id{}=BusId) ->
    connect(BusId, undefined);

connect(BusName) when BusName =:= system;
              BusName =:= session ->
    connect(get_bus_id(BusName)).

connect(#bus_id{}=BusId, ServiceReg) ->
    case dbus_peer_connection:start_link(BusId, ServiceReg) of
	{ok, {dbus_peer_connection, PConn} = Conn} ->
	    case dbus_peer_connection:auth(PConn) of
		{ok, undefined} ->
		    case dbus_proxy:start_link(Conn, ?DBUS_SERVICE, <<"/">>, ?DBUS_NODE) of
			{ok, DBus} ->
			    ConnId = hello(DBus),
			    ?debug("Hello connection id: ~p~n", [ConnId]),
                dbus_peer_connection:set_unique_name(PConn, ConnId),
			    dbus_peer_connection:set_controlling_process(PConn, DBus),
			    {ok, {?MODULE, DBus}};
			{error, Err} -> {error, Err}
		    end;
		{error, Err} -> {error, Err}
	    end;
	{error, Err} -> {error, Err}
    end;

connect(BusName, ServiceReg) when BusName =:= system;
		      BusName =:= session ->
    connect(get_bus_id(BusName), ServiceReg).


%% @doc Stop the bus proxy
%% @end
-spec close({?MODULE, dbus_connection()} | dbus_connection()) -> ok.
close({?MODULE, Bus}) ->     dbus_proxy:stop(Bus);
close(Bus) ->                dbus_proxy:stop(Bus).


%% @doc Send a message to the bus connection, synchronously.
%% @end
-spec call({?MODULE, dbus_connection()} | dbus_connection(), dbus_message()) -> {ok, term()} | {error, term()}.
call({?MODULE, Bus}, Msg) -> dbus_proxy:call(Bus, Msg);
call(Bus, Msg) ->            dbus_proxy:call(Bus, Msg).


%% @doc Send a message to the bus connection, asynchronously.
%% @end
-spec cast({?MODULE, dbus_connection()} | dbus_connection(), dbus_message()) -> ok | {error, term()}.
cast({?MODULE, Bus}, Msg) -> dbus_proxy:cast(Bus, Msg);
cast(Bus, Msg) ->            dbus_proxy:cast(Bus, Msg).

%% @doc Get the DBUS connection unique name.
%% @end
-spec get_unique_name({?MODULE, dbus_connection()} | dbus_connection()) -> {ok, binary()} | {error, term()}.
get_unique_name({?MODULE, Bus}) -> dbus_proxy:get_unique_name(Bus);
get_unique_name(Bus) ->            dbus_proxy:get_unique_name(Bus).

%%%
%%% Priv
%%%
env_to_bus_id() ->
    str_to_bus_id(os:getenv(?SESSION_ENV)).

str_to_bus_id(Addr) when is_list(Addr) ->
    list_to_bus_id(string:tokens(Addr, [?SERVER_DELIM]), []).

list_to_bus_id([], Acc) ->
    lists:reverse(Acc);
list_to_bus_id([L|Rest], Acc) ->
    list_to_bus_id(Rest, [to_bus_id(L) | Acc]).

to_bus_id(Server) when is_list(Server) ->
    {Transport, [?TRANSPORT_DELIM | Params]} =
	lists:splitwith(fun(A) -> A =/= ?TRANSPORT_DELIM end, Server),
    #bus_id{scheme=list_to_existing_atom(Transport),
	    options=parse_params(Params)}.

parse_params(Params) when is_list(Params) ->
    parse_params(string:tokens(Params, [?PARAM_DELIM]), []).

parse_params([], Acc) ->
    Acc;
parse_params([Param|Rest], Acc) ->
    parse_params(Rest, [parse_param(Param) | Acc]).

parse_param(Param) when is_list(Param) ->
    {Key, [?KEY_DELIM | Value]} =
	lists:splitwith(fun(A) -> A =/= ?KEY_DELIM end, Param),
    Key_name =
        case catch list_to_existing_atom(Key) of
            {'EXIT', {badarg, _Reason}} ->
                Key;
            Key_atom -> Key_atom
        end,
    {Key_name, parse_value(Key_name, Value)}.

parse_value(port, Value) ->
    list_to_integer(Value);
parse_value(_, Value) ->
    Value.

hello(DBusObj) ->
    {ok, Ret} = dbus_proxy:call(DBusObj, 'org.freedesktop.DBus', 'Hello', []),
    Ret.
