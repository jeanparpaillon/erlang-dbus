

# Module dbus_transport #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Abstract transports.

Copyright (c) 2006-2007 Mikael Magnusson

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free`](mailto:jean.parpaillon@free)).

<a name="description"></a>

## Description ##

Glue module to dbus_transprot_tcp and dbus_transport_unix transport modules

Messages implemented by transport modules
* `{received, Conn, Data}`
* `{closed, Conn}`

While unix socket and TCP transports are the commonly used transports,
[D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.md#transports)
describes additional transports.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close the transport.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send data to a transport.</td></tr><tr><td valign="top"><a href="#set_raw-2">set_raw/2</a></td><td>Set transport in raw mode (used after authentication is done).</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop transport.</td></tr><tr><td valign="top"><a href="#support_unix_fd-1">support_unix_fd/1</a></td><td>Check if this transport support UNIX FD passing.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Conn::pid()) -&gt; ok
</code></pre>
<br />

Close the transport

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Conn::pid(), Data::binary()) -&gt; ok
</code></pre>
<br />

Send data to a transport

<a name="set_raw-2"></a>

### set_raw/2 ###

<pre><code>
set_raw(Conn::pid(), Raw::boolean()) -&gt; ok
</code></pre>
<br />

Set transport in raw mode (used after authentication is done)

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Conn::pid()) -&gt; ok
</code></pre>
<br />

Stop transport

<a name="support_unix_fd-1"></a>

### support_unix_fd/1 ###

<pre><code>
support_unix_fd(Conn::pid()) -&gt; boolean()
</code></pre>
<br />

Check if this transport support UNIX FD passing

