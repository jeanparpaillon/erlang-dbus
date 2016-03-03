

# Module dbus_connection #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Describe callbacks for modules implementing connections.

Copyright (c) 2015 Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
Actually implemented by:
* @see dbus_peer_connection
* @see dbus_bus_connection
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td>Synchronously send a message.</td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Asychronously send a message.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close the connection.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(X1::<a href="#type-dbus_connection">dbus_connection()</a>, Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Synchronously send a message

<a name="cast-2"></a>

### cast/2 ###

<pre><code>
cast(X1::<a href="#type-dbus_connection">dbus_connection()</a>, Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Asychronously send a message

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(X1::<a href="#type-dbus_connection">dbus_connection()</a>) -&gt; ok
</code></pre>
<br />

Close the connection

