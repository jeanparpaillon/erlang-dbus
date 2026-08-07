A native erlang implementation of D-Bus
==============================================

D-Bus is now largely used in a lot of applications for
language-independant, object-oriented RPC system.

The erlang platform needs an erlang native implementation.

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=jeanparpaillon&url=https://github.com/lizenn/erlang-dbus.git&title=erlang-dbus&language=erlang&tags=github&category=software)
[![Build Status](https://travis-ci.org/lizenn/erlang-dbus.svg?branch=master)](https://travis-ci.org/lizenn/erlang-dbus)
[![Hex.pm](https://img.shields.io/hexpm/v/dbus.svg)](https://hex.pm/packages/dbus)
[![Hex.pm](https://img.shields.io/hexpm/dt/dbus.svg)](https://hex.pm/packages/dbus)
[![Project Stats](https://www.openhub.net/p/erlang-dbus2/widgets/project_thin_badge.gif)](https://www.openhub.net/p/erlang-dbus2)

## Usage as Client

This example is making a dbus call to the `org.freedesktop.DBus` system service (under linux) and a list of registered services.

```erlang
  {ok, Bus} = dbus_bus_reg:get_bus(session),
  {ok, Service} = dbus_bus:get_service(Bus, 'org.freedesktop.DBus'),
  {ok, RemoteObject} = dbus_remote_service:get_object(Service, '/org/freedesktop/DBus'),
  {ok, Iface} = dbus_proxy:interface(RemoteObject, 'org.freedesktop.DBus'),
  {ok, Names} = dbus_proxy:call(Iface, 'ListNames', []),
  io:format("ListNames: ~p~n", [lists:sort(Names)]),
  ok = dbus_remote_service:release_object(Service, RemoteObject),
  ok = dbus_bus:release_service(Bus, Service),
```

## Usage as Service

In the demo folder there is a bigger example, but is a minimal service callback module:

```erlang
-module(my_service).
-include_lib("dbus/include/dbus.hrl").
-behaviour(gen_dbus).

-export([
%% api
  start_link/2,
  handle_info/2,

%% dbus object callbacks
  'HelloWorld'/1,
  'HelloWorld'/3,

%% gen_dbus callbacks
  init/1
]).

-record(state, {}).

start_link() ->
  gen_dbus:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Service, Path]) ->
  State = #state{},
  Methods = ['HelloWorld'],
  {ok, {"com.example.MyService", '/SomeObject', [
    {interface, 'com.example.MyInterface'},
    {methods, Methods},
    {signals, []}
    ]}, State}.

'HelloWorld'(dbus_info) ->
  [{interface, 'com.example.MyInterface'},
    {signature, [string], [{array, string}]}].

'HelloWorld'([HelloMessage], From, State) ->
  {reply, ["Hello from Erlang"], State}.

handle_info(Info, State) ->
  error_logger:warning_msg("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

```

When the `dbus` application is running you can start this service with `my_module:start_link().` or add it to your supervision tree.

*Caveat* at the moment the service creation does not open a dbus connection and as a result the service will not be visible until you create the first dbus connection e.g. via `dbus_bus_reg:get_bus(session).`

## Documentation

* [API documentation](doc/README.md)
* [Manual](https://github.com/lizenn/erlang-dbus/wiki)

## Current status

The status: 
* Consuming D-Bus services: ok
* Providing D-Bus services: ok
* Tests for both are working!
* Connect through TCP and UNIX socket: ok

### TODO
* Fix signal emission from services
* Make dializer happy
* Some authentication mechanisms are not implemented, but architectures allows for easy extension (see https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_cookie_sha1.erl, https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_external.erl and https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_anonymous.erl)
* Create new gen_dbus.erl that uses `handle_dbus_call(Name, Args)` form instead of current.
* Cleanup Supervisor & gen_server hierarchy
  * Remove superfluos gen_servers (proxy / peer / dbus)
  * Make service registration more explicit when using multiple busses
* Fix indentation
* More docs & examples
* More tests: unit tests regarding (un)marshaling would be really great, even if the above mentioned xample has rather complex interfaces which works both with Python and Java implementations
* Provide facilities for standard interfaces: Properties, ObjectManager, etc.
