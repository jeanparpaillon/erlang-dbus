

# Module dbus_service #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Exported D-BUS service gen_server.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_object-3">register_object/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_object-2">unregister_object/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Request, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="register_object-3"></a>

### register_object/3 ###

`register_object(Service, Path, Object) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(ServiceName) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="unregister_object-2"></a>

### unregister_object/2 ###

`unregister_object(Service, Object) -> any()`

