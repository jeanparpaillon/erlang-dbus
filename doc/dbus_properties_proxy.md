

# Module dbus_properties_proxy #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Specific methods for a proxy of an object implementing
'org.freedesktop.DBus.Properties' interface.

Copyright (c) (C) 2015, Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.md#standard-interfaces-properties)<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>Connect to the 'PropertiesChanged' signal.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Get a property value.</td></tr><tr><td valign="top"><a href="#get_all-2">get_all/2</a></td><td>Get a key-value list of properties.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Set a property value.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-2"></a>

### connect/2 ###

<pre><code>
connect(Proxy::<a href="dbus_proxy.md#type-t">dbus_proxy:t()</a>, Handler::<a href="dbus_proxy.md#type-handler">dbus_proxy:handler()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Connect to the 'PropertiesChanged' signal.

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Proxy::<a href="dbus_proxy.md#type-t">dbus_proxy:t()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>, Prop::<a href="#type-dbus_name">dbus_name()</a>) -&gt; term()
</code></pre>
<br />

Get a property value

<a name="get_all-2"></a>

### get_all/2 ###

<pre><code>
get_all(Proxy::<a href="dbus_proxy.md#type-t">dbus_proxy:t()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>) -&gt; [{<a href="#type-dbus_name">dbus_name()</a>, <a href="#type-dbus_variant">dbus_variant()</a>}]
</code></pre>
<br />

Get a key-value list of properties

<a name="set-4"></a>

### set/4 ###

<pre><code>
set(Proxy::<a href="dbus_proxy.md#type-t">dbus_proxy:t()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>, Prop::<a href="#type-dbus_name">dbus_name()</a>, Val::term()) -&gt; ok
</code></pre>
<br />

Set a property value

