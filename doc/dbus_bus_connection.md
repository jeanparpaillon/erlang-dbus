

# Module dbus_bus_connection #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Implements the connection to a D-Bus bus.

Copyright (c) 2014 Jean Parpaillon

__Behaviours:__ [`dbus_connection`](dbus_connection.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##

Actually, the following addresses classes are supported:
* `unix`
* `tcp`

Other classes are _ignored_, in particular `kernel`.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td>Send a message to the bus connection, synchronously.</td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Send a message to the bus connection, asynchronously.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Stop the bus proxy.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>Start a proxy to a bus.</td></tr><tr><td valign="top"><a href="#get_bus_id-1">get_bus_id/1</a></td><td>Retrieve a bus_id from well-known names.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(Bus::{'?MODULE', <a href="#type-dbus_connection">dbus_connection()</a>} | <a href="#type-dbus_connection">dbus_connection()</a>, Msg::<a href="#type-dbus_message">dbus_message()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message to the bus connection, synchronously.

<a name="cast-2"></a>

### cast/2 ###

<pre><code>
cast(Bus::{'?MODULE', <a href="#type-dbus_connection">dbus_connection()</a>} | <a href="#type-dbus_connection">dbus_connection()</a>, Msg::<a href="#type-dbus_message">dbus_message()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Send a message to the bus connection, asynchronously.

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Bus::{'?MODULE', <a href="#type-dbus_connection">dbus_connection()</a>} | <a href="#type-dbus_connection">dbus_connection()</a>) -&gt; ok
</code></pre>
<br />

Stop the bus proxy

<a name="connect-1"></a>

### connect/1 ###

<pre><code>
connect(Bus_id::<a href="#type-bus_id">bus_id()</a> | <a href="#type-dbus_known_bus">dbus_known_bus()</a>) -&gt; {ok, <a href="#type-dbus_connection">dbus_connection()</a>} | {error, term()}
</code></pre>
<br />

Start a proxy to a bus.

<a name="get_bus_id-1"></a>

### get_bus_id/1 ###

<pre><code>
get_bus_id(X1::<a href="#type-dbus_known_bus">dbus_known_bus()</a>) -&gt; <a href="#type-bus_id">bus_id()</a> | {unsupported, [<a href="#type-bus_id">bus_id()</a>]}
</code></pre>
<br />

Retrieve a bus_id from well-known names

