-module(dbus_properties_proxy).
-moduledoc """
Specific methods for a proxy of an object implementing the
`org.freedesktop.DBus.Properties` interface.

See [D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties).
""".

-include("dbus_client.hrl").
-include("dbus_properties.hrl").

-export([
    get/3,
    set/4,
    get_all/2,
    connect/2
]).

-doc "Get a property value.".
-spec get(Proxy :: dbus_proxy:t(), Iface :: dbus_name(), Prop :: dbus_name()) -> term().
get(Proxy, Iface, Prop) ->
    case dbus_proxy:call(Proxy, ?DBUS_IFACE_PROPERTIES, <<"Get">>, [Iface, Prop]) of
        {ok, Value} ->
            Value;
        {error, Err} ->
            throw(Err)
    end.

-doc "Set a property value.".
-spec set(Proxy :: dbus_proxy:t(), Iface :: dbus_name(), Prop :: dbus_name(), Val :: term()) -> ok.
set(Proxy, Iface, Prop, Value) ->
    case dbus_proxy:call(Proxy, ?DBUS_IFACE_PROPERTIES, <<"Set">>, [Iface, Prop, Value]) of
        {ok, []} -> ok;
        {error, Err} -> throw(Err)
    end.

-doc "Get a key-value list of properties.".
-spec get_all(Proxy :: dbus_proxy:t(), Iface :: dbus_name()) -> [{dbus_name(), dbus_variant()}].
get_all(Proxy, Iface) ->
    case dbus_proxy:call(Proxy, ?DBUS_IFACE_PROPERTIES, <<"GetAll">>, [Iface]) of
        {ok, [Props]} -> Props;
        {error, Err} -> throw(Err)
    end.

-doc "Connect to the `PropertiesChanged` signal.".
-spec connect(Proxy :: dbus_proxy:t(), Handler :: dbus_proxy:handler()) -> ok | {error, term()}.
connect(Proxy, Handler) ->
    dbus_proxy:connect_signal(Proxy, ?DBUS_IFACE_PROPERTIES, <<"PropertiesChanged">>, Handler).
