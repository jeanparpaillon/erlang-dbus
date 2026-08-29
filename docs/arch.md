erlang-dbus arch
================

# Message Protocol, parsers

- `dbus_address` - D-Bus addresses parser
- `dbus_marshaller` - D-Bus wire (de)serialization
- `dbus_hex` - hex encoding/decoding
    Really needed ?
- `dbus_introspect` - D-Bus XML introspection module
- `dbus_message` - High level API for building messages : calls, signals,
  errors, etc

# Authentication layer / Session

- `dbus_auth` - behaviour
- `dbus_auth_anonymous` - ANONYMOUS
- `dbus_auth_cookie_sha1` - DBUS_COOKIE_SHA1
- `dbus_auth_external` - EXTERNAL

# Transport

- `dbus_transport` - behaviour
- `dbus_transport_unix` - UNIX domain sockets (path or abstract)
- `dbus_transport_tcp` - TCP sockets

# Connection

- `dbus_connection` - 

# Bus layer

- `dbus_bus_connection` - Connection to a D-Bus bus
- `dbus_bus_registry` - ?
- `dbus_bus` - broken ?

# Object / RPC

- `dbus_names` - well known binaries to atom
- `dbus_sup` - Top level supervisor (should be started only with service, or proxy)
- `gen_dbus` - D-Bus object behaviour (?)

## Application

### Client

- `dbus_proxy` - D-Bus object proxy
- `dbus_properties_proxy` - 'org.freedesktop.DBus.Properties' proxy

### Service

- `dbus_service` - 
- `dbus_remote_service` -
- `dbus_service_reg` - 