Implementation of the D-Bus protocol in Erlang
==============================================

# Examples

## Connecting to a remote service

```
{ok, Bus} = dbus_bus_connection(system).
{ok, Service} = dbus_proxy:start_link(Bus, <<"org.freedesktop.Accounts">>).
```

# dbus application parameters

* external_cookie: magic cookie for external authentication (default: 31303030)
