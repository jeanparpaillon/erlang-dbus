

# Module dbus_proxy #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

proxy representing a remote D-BUS object.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="types"></a>

## Data Types ##




### <a name="type-handler">handler()</a> ###


<pre><code>
handler() = mfa() | {function(), any()} | pid()
</code></pre>




### <a name="type-t">t()</a> ###


<pre><code>
t() = <a href="#type-dbus_proxy">dbus_proxy()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td>Sync send an arbitrary message.</td></tr><tr><td valign="top"><a href="#call-4">call/4</a></td><td>Sync call a method.</td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Async send a message.</td></tr><tr><td valign="top"><a href="#cast-4">cast/4</a></td><td>Async call a method.</td></tr><tr><td valign="top"><a href="#children-1">children/1</a></td><td>Get children of an object.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect_signal-2">connect_signal/2</a></td><td>Connect to every signal (eg for object manager).</td></tr><tr><td valign="top"><a href="#connect_signal-4">connect_signal/4</a></td><td>Connect to a particular signal.</td></tr><tr><td valign="top"><a href="#connect_signal-6">connect_signal/6</a></td><td>Connect to a particular signal on a particular children object.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#has_interface-2">has_interface/2</a></td><td>Check if object implements the given interface.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Equivalent to <a href="#start_link-3"><tt>start_link(Conn, Service, &lt;&lt;"/"&gt;&gt;)</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Connect to an object, and introspect it.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>Connect to an object, with known interfaces.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Disconnect proxy.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, Msg::<a href="#type-dbus_message">dbus_message()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Sync send an arbitrary message

<a name="call-4"></a>

### call/4 ###

<pre><code>
call(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, IfaceName::<a href="#type-dbus_name">dbus_name()</a>, MethodName::<a href="#type-dbus_name">dbus_name()</a>, Args::term()) -&gt; ok | {ok, term()} | {error, term()}
</code></pre>
<br />

Sync call a method

<a name="cast-2"></a>

### cast/2 ###

<pre><code>
cast(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, Msg::<a href="#type-dbus_message">dbus_message()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Async send a message

<a name="cast-4"></a>

### cast/4 ###

<pre><code>
cast(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, IfaceName::<a href="#type-dbus_name">dbus_name()</a>, MethodName::<a href="#type-dbus_name">dbus_name()</a>, Args::term()) -&gt; ok
</code></pre>
<br />

Async call a method

<a name="children-1"></a>

### children/1 ###

<pre><code>
children(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>) -&gt; [binary()]
</code></pre>
<br />

Get children of an object

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="connect_signal-2"></a>

### connect_signal/2 ###

<pre><code>
connect_signal(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, Handler::<a href="#type-handler">handler()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Connect to every signal (eg for object manager)

<a name="connect_signal-4"></a>

### connect_signal/4 ###

<pre><code>
connect_signal(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, IfaceName::<a href="#type-dbus_name">dbus_name()</a>, SignalName::<a href="#type-dbus_name">dbus_name()</a>, Handler::<a href="#type-handler">handler()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Connect to a particular signal

<a name="connect_signal-6"></a>

### connect_signal/6 ###

<pre><code>
connect_signal(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, Service::<a href="#type-dbus_name">dbus_name()</a>, IfaceName::<a href="#type-dbus_name">dbus_name()</a>, SignalName::<a href="#type-dbus_name">dbus_name()</a>, Path::binary(), MFA::<a href="#type-handler">handler()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Connect to a particular signal on a particular children object

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Request, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="has_interface-2"></a>

### has_interface/2 ###

<pre><code>
has_interface(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>, InterfaceName::<a href="#type-dbus_name">dbus_name()</a>) -&gt; true | false
</code></pre>
<br />

Check if object implements the given interface

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Conn::<a href="#type-dbus_connection">dbus_connection()</a>, Service::<a href="#type-dbus_name">dbus_name()</a>) -&gt; {ok, <a href="#type-dbus_proxy">dbus_proxy()</a>} | {error, term()}
</code></pre>
<br />

Equivalent to [`start_link(Conn, Service, <<"/">>)`](#start_link-3).

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Conn::<a href="#type-dbus_connection">dbus_connection()</a>, Service::<a href="#type-dbus_name">dbus_name()</a>, Path::binary()) -&gt; {ok, <a href="#type-dbus_proxy">dbus_proxy()</a>} | {error, term()}
</code></pre>
<br />

Connect to an object, and introspect it.

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Conn::<a href="#type-dbus_connection">dbus_connection()</a>, Service::<a href="#type-dbus_name">dbus_name()</a>, Path::binary(), Node::<a href="#type-dbus_node">dbus_node()</a>) -&gt; {ok, <a href="#type-dbus_proxy">dbus_proxy()</a>} | {error, term()}
</code></pre>
<br />

Connect to an object, with known interfaces.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Proxy::<a href="#type-dbus_proxy">dbus_proxy()</a>) -&gt; ok
</code></pre>
<br />

Disconnect proxy

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

