

# Module dbus_introspect #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

API Introspection support module.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [D-Bus specifications](https://dbus.freedesktop.org/doc/dbus-specification.md#introspection-format)
for introspection XML schema.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find_interface-2">find_interface/2</a></td><td>Find an interface definition.</td></tr><tr><td valign="top"><a href="#find_method-3">find_method/3</a></td><td>Find a method definition.</td></tr><tr><td valign="top"><a href="#find_signal-3">find_signal/3</a></td><td>Find a signal definition.</td></tr><tr><td valign="top"><a href="#from_xml-1">from_xml/1</a></td><td>Parse a <code>dbus_node()</code> from a filename.</td></tr><tr><td valign="top"><a href="#from_xml_string-1">from_xml_string/1</a></td><td>Parse a <code>dbus_node()</code> from an XML string.</td></tr><tr><td valign="top"><a href="#to_xml-1">to_xml/1</a></td><td>Export a <code>dbus_node()</code> into an XML introspection document.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="find_interface-2"></a>

### find_interface/2 ###

<pre><code>
find_interface(Dbus_node::<a href="#type-dbus_node">dbus_node()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>) -&gt; {ok, <a href="#type-dbus_iface">dbus_iface()</a>} | {error, <a href="#type-dbus_err">dbus_err()</a>}
</code></pre>
<br />

Find an interface definition.

<a name="find_method-3"></a>

### find_method/3 ###

<pre><code>
find_method(Dbus_node::<a href="#type-dbus_node">dbus_node()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>, Method::<a href="#type-dbus_name">dbus_name()</a>) -&gt; {ok, <a href="#type-dbus_method">dbus_method()</a>} | {error, <a href="#type-dbus_err">dbus_err()</a>}
</code></pre>
<br />

Find a method definition.

<a name="find_signal-3"></a>

### find_signal/3 ###

<pre><code>
find_signal(Dbus_node::<a href="#type-dbus_node">dbus_node()</a>, Iface::<a href="#type-dbus_name">dbus_name()</a>, Signal::<a href="#type-dbus_name">dbus_name()</a>) -&gt; {ok, <a href="#type-dbus_signal">dbus_signal()</a>} | {error, <a href="#type-dbus_err">dbus_err()</a>}
</code></pre>
<br />

Find a signal definition

<a name="from_xml-1"></a>

### from_xml/1 ###

<pre><code>
from_xml(Filename::<a href="file.md#type-filename">file:filename()</a>) -&gt; <a href="#type-dbus_node">dbus_node()</a>
</code></pre>
<br />

throws `{error, parse_error}`

Parse a `dbus_node()` from a filename.

<a name="from_xml_string-1"></a>

### from_xml_string/1 ###

<pre><code>
from_xml_string(Data::binary()) -&gt; <a href="#type-dbus_node">dbus_node()</a>
</code></pre>
<br />

throws `{error, parse_error}`

Parse a `dbus_node()` from an XML string.

<a name="to_xml-1"></a>

### to_xml/1 ###

<pre><code>
to_xml(Dbus_node::<a href="#type-dbus_node">dbus_node()</a>) -&gt; list()
</code></pre>
<br />

Export a `dbus_node()` into an XML introspection document.

