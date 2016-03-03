

# Module dbus_marshaller #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

D-Bus binary format (un)marshaling.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2106 Jean Parpaillon

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [D-Bus Specification](https://dbus.freedesktop.org/doc/dbus-specification.md#message-protocol-marshaling).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#marshal_list-2">marshal_list/2</a></td><td>Encode objects, given a signature.</td></tr><tr><td valign="top"><a href="#marshal_message-1">marshal_message/1</a></td><td>Encode a message.</td></tr><tr><td valign="top"><a href="#marshal_signature-1">marshal_signature/1</a></td><td>Encode a signature.</td></tr><tr><td valign="top"><a href="#unmarshal_data-1">unmarshal_data/1</a></td><td>Decode messages.</td></tr><tr><td valign="top"><a href="#unmarshal_signature-1">unmarshal_signature/1</a></td><td>Decode a signature.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="marshal_list-2"></a>

### marshal_list/2 ###

<pre><code>
marshal_list(Types::<a href="#type-dbus_signature">dbus_signature()</a>, Value::term()) -&gt; iolist()
</code></pre>
<br />

Encode objects, given a signature

<a name="marshal_message-1"></a>

### marshal_message/1 ###

<pre><code>
marshal_message(Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; iolist()
</code></pre>
<br />

Encode a message

<a name="marshal_signature-1"></a>

### marshal_signature/1 ###

<pre><code>
marshal_signature(R::<a href="#type-dbus_signature">dbus_signature()</a>) -&gt; iolist()
</code></pre>
<br />

Encode a signature

<a name="unmarshal_data-1"></a>

### unmarshal_data/1 ###

<pre><code>
unmarshal_data(Data::binary()) -&gt; {ok, Msgs::[<a href="#type-dbus_message">dbus_message()</a>], Rest::binary()} | more
</code></pre>
<br />

Decode messages

Returns:
* `{ok, [dbus_message()], binary()}`: if binary describe a complete list of messages, eventually with remaining binary.
* `more`: if no complete message could be decoded.

<a name="unmarshal_signature-1"></a>

### unmarshal_signature/1 ###

<pre><code>
unmarshal_signature(Bin::binary()) -&gt; {ok, <a href="#type-dbus_signature">dbus_signature()</a>} | more
</code></pre>
<br />

Decode a signature

Returns `more` if no complete signature could be decoded.

