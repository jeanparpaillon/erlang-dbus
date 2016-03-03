

# Module dbus_bus #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Bus gen_server (broken).

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td></td></tr><tr><td valign="top"><a href="#export_service-2">export_service/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_service-2">get_service/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#release_service-2">release_service/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unexport_service-2">unexport_service/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cast-2"></a>

### cast/2 ###

`cast(Bus, Dbus_message) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="connect-1"></a>

### connect/1 ###

`connect(BusId) -> any()`

<a name="export_service-2"></a>

### export_service/2 ###

`export_service(Bus, ServiceName) -> any()`

<a name="get_service-2"></a>

### get_service/2 ###

`get_service(Bus, ServiceName) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Dbus_message, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="release_service-2"></a>

### release_service/2 ###

`release_service(Bus, Service) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Bus) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="unexport_service-2"></a>

### unexport_service/2 ###

`unexport_service(Bus, ServiceName) -> any()`

