A native erlang implementation of D-Bus
==============================================

D-Bus is now largely used in a lot of applications for
language-independant, object-oriented RPC system.

The erlang platform needs an erlang native implementation.

[![CI](https://github.com/jeanparpaillon/erlang-dbus/actions/workflows/ci.yml/badge.svg)](https://github.com/jeanparpaillon/erlang-dbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/jeanparpaillon/erlang-dbus/branch/main/graph/badge.svg)](https://codecov.io/gh/jeanparpaillon/erlang-dbus)
[![Hex.pm](https://img.shields.io/hexpm/v/dbus.svg)](https://hex.pm/packages/dbus)

# Usage as Client

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

# Usage as Service

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

# Documentation

* [API documentation](doc/README.md)
* [Manual](https://github.com/jeanparpaillon/erlang-dbus/wiki)
* [D-Bus Specifications](https://dbus.freedesktop.org/doc/dbus-specification.html)

## Current status

The status: 
* Consuming D-Bus services: ok
* Providing D-Bus services: ok
* Tests for both are working!
* Connect through TCP and UNIX socket: ok

# Issue format

`submit_issue.sh` parses these files, so the header is structural, not decorative.
It takes the **title from the first line** (stripping `# `), reads the four `**Key:**`
lines, and uses **everything from the first `##` heading onward** as the issue body.

```markdown
# Fix parsing env var

**Workstream:** 2.3 - Fixes
**Context:** [arch.md](doc/connections.md) 
**Requires:** [1.2]
**State:** draft

## What

...

## Checklist

- [ ] ...

## Acceptance

- [ ] ...
```

Rules the script enforces, or that follow from how it works:

- **Filename is the issue number with dots as underscores** — `3_1.md` is issue 3.1.
- **The number must be in the title.** `Requires:` refs are resolved with
  `gh issue list --search "<ref> in:title"` to build `--blocked-by`, so a title without
  its number cannot be depended on.
- **`Requires:` refs are `[N.N]` in square brackets**, comma-separated or in prose;
  the extractor only sees the bracketed forms. Use `none — <why>` when there are no
  dependencies.
- **Submit in dependency order.** Refs resolve against issues that already exist on
  GitHub; an issue submitted before its dependency gets no `--blocked-by` link and the
  script says nothing about it.
- **`Workstream:` and `Context:` are re-emitted as `##` headings** at the top of the
  body. Do not repeat them in the prose.
- **`State:`** — `draft` until the issue is submitted.
- Requires `gh` and `gum`.
