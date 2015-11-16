A native erlang implementation of D-Bus
==============================================

D-BUs is now largely used in a lot of applications for
language-independant, object-oriented RPC system.

The erlang platform needs an erlang native implementation.

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=jeanparpaillon&url=https://github.com/lizenn/erlang-dbus.git&title=erlang-dbus&language=erlang&tags=github&category=software)

# Examples

## Connecting to a remote service

```
{ok, Bus} = dbus_bus_connection(system).
{ok, Service} = dbus_proxy:start_link(Bus, <<"org.freedesktop.Accounts">>).
```

# dbus application parameters

* external_cookie: magic cookie for external authentication (default: 31303030)
