%%%
%%% @copyright 2014 Jean Parpaillon
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @doc 
%%%
%%% @end
%%% Created : 22 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_bus_connection).

-include("dbus.hrl").
-include("dbus_client.hrl").
-include("dbus_dbus.hrl").
-include("dbus_introspectable.hrl").

-export([connect/1,
	 get_object/3,
	 get_objects_manager/2]).


-define(DEFAULT_BUS_SYSTEM, #bus_id{scheme=unix,options=[{path, "/var/run/dbus/system_bus_socket"}]}).
-define(SESSION_ENV, "DBUS_SESSION_BUS_ADDRESS").
-define(SERVER_DELIM, $;).
-define(TRANSPORT_DELIM, $:).
-define(PARAM_DELIM, $,).
-define(KEY_DELIM, $=).

-define(DEFAULT_DBUS_SERVICE, 'org.freedesktop.DBus').
-define(DEFAULT_DBUS_NODE, 
	#dbus_node{elements=[], 
		   interfaces=gb_trees:from_orddict([{'org.freedesktop.DBus', ?DBUS_DBUS}, 
						     {'org.freedesktop.DBus.Introspectable', ?DBUS_INTROSPECTABLE}])}).

-spec connect(dbus_bus_name()) -> {ok, dbus_bus_conn()} | {error, term()}.
connect(BusName) ->
    BusId = get_bus_id(BusName),
    case dbus_connection:start_link(BusId, [list, {packet, 0}]) of
	{ok, Conn} ->
	    dbus_connection:auth(Conn),
	    case dbus_proxy:start_link(Conn, ?DEFAULT_DBUS_SERVICE, [{node, ?DEFAULT_DBUS_NODE}]) of
		{ok, DBus} ->
		    ConnId = hello(DBus),
		    ?debug("Got connection id: ~p~n", [ConnId]),
		    {ok, #dbus_bus_conn{conn=Conn, conn_id=ConnId, bus=DBus}};
		{error, Err} ->
		    {error, Err}
	    end;
	{error, Err} ->
	    {error, Err}
    end.

-spec get_object(dbus_bus_conn(), Service :: dbus_name(), Path :: binary()) -> {ok, dbus_proxy()} | {error, term()}.
get_object(#dbus_bus_conn{conn=Conn, bus=DBus}, ServiceName, Path) ->
    dbus_proxy:start_link(Conn, ServiceName, Path, [{bus_proxy, DBus}]).

-spec get_objects_manager(dbus_bus_conn(), dbus_name()) -> {ok, dbus_proxy()} | {error, term()}.
get_objects_manager(#dbus_bus_conn{conn=Conn, bus=DBus}, ServiceName) ->
    dbus_proxy:start_link(Conn, ServiceName, <<"/">>, [manager, {bus_proxy, DBus}]).

%%%
%%% Priv
%%%
get_bus_id(session) ->
    [BusId|_R] = env_to_bus_id(),
    BusId;
get_bus_id(system) ->
    ?DEFAULT_BUS_SYSTEM.


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
