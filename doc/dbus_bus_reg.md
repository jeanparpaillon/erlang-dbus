

# Module dbus_bus_reg #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Bus registry.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cast-1">cast/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#export_service-2">export_service/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_bus-1">get_bus/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#release_bus-1">release_bus/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_service_reg-1">set_service_reg/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#unexport_service-2">unexport_service/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cast-1"></a>

### cast/1 ###

`cast(Dbus_message) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="export_service-2"></a>

### export_service/2 ###

`export_service(Service, ServiceName) -> any()`

<a name="get_bus-1"></a>

### get_bus/1 ###

<pre><code>
get_bus(Bus_id::#bus_id{}) -&gt; {ok, pid()}
</code></pre>
<br />

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

<a name="release_bus-1"></a>

### release_bus/1 ###

<pre><code>
release_bus(Bus::pid()) -&gt; ok | {error, not_registered}
</code></pre>
<br />

<a name="set_service_reg-1"></a>

### set_service_reg/1 ###

`set_service_reg(ServiceReg) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | {error, term()} | ignore
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="unexport_service-2"></a>

### unexport_service/2 ###

`unexport_service(Service, ServiceName) -> any()`

