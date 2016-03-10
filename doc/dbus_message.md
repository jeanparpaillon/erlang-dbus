

# Module dbus_message #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Build messages.

Copyright (c) 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon

__Authors:__ Mikael Magnusson ([`mikma@users.sourceforge.net`](mailto:mikma@users.sourceforge.net)), Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [D-Bus Specification](https://dbus.freedesktop.org/doc/dbus-specification.md#message-protocol-messages)

<a name="types"></a>

## Data Types ##




### <a name="type-type">type()</a> ###


<pre><code>
type() = '?TYPE_INVALID' | '?TYPE_METHOD_CALL' | '?TYPE_METHOD_RETURN' | '?TYPE_ERROR' | '?TYPE_SIGNAL'
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-4">call/4</a></td><td>Equivalent to <a href="#call-5"><tt>call(Destination, Path, Interface, Member, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#call-5">call/5</a></td><td>Build a method call message.</td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>Build an error message.</td></tr><tr><td valign="top"><a href="#find_field-2">find_field/2</a></td><td>Find a specific field of a message.</td></tr><tr><td valign="top"><a href="#get_field-2">get_field/2</a></td><td>Get a specific field of a message.</td></tr><tr><td valign="top"><a href="#get_field_value-2">get_field_value/2</a></td><td>Get a field value.</td></tr><tr><td valign="top"><a href="#get_serial-1">get_serial/1</a></td><td>Get serial number from message.</td></tr><tr><td valign="top"><a href="#introspect-2">introspect/2</a></td><td>Build <code>Introspect</code> method call message.</td></tr><tr><td valign="top"><a href="#is_error-2">is_error/2</a></td><td>Check message is an error and of the given type.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Check message headers matches some values.</td></tr><tr><td valign="top"><a href="#return-3">return/3</a></td><td>Build a return message.</td></tr><tr><td valign="top"><a href="#set_body-3">set_body/3</a></td><td>Set body of a message.</td></tr><tr><td valign="top"><a href="#set_body-4">set_body/4</a></td><td>Set body of a message.</td></tr><tr><td valign="top"><a href="#set_serial-2">set_serial/2</a></td><td>Set serial number of a message.</td></tr><tr><td valign="top"><a href="#signal-5">signal/5</a></td><td>Equivalent to <a href="#signal-6"><tt>signal(Destination, Path, Interface, Signal, Args, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#signal-6">signal/6</a></td><td>Build a signal message.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Get message type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-4"></a>

### call/4 ###

<pre><code>
call(Destination::<a href="#type-dbus_name">dbus_name()</a>, Path::<a href="#type-dbus_name">dbus_name()</a>, Interface::<a href="#type-dbus_name">dbus_name()</a>, Member::<a href="#type-dbus_name">dbus_name()</a> | <a href="#type-dbus_method">dbus_method()</a>) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Equivalent to [`call(Destination, Path, Interface, Member, [])`](#call-5).

<a name="call-5"></a>

### call/5 ###

<pre><code>
call(Destination::<a href="#type-dbus_name">dbus_name()</a>, Path::<a href="#type-dbus_name">dbus_name()</a>, Interface::<a href="#type-dbus_name">dbus_name()</a>, Member::<a href="#type-dbus_name">dbus_name()</a> | <a href="#type-dbus_method">dbus_method()</a>, Opts::[<a href="#type-dbus_option">dbus_option()</a>]) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Build a method call message

<a name="error-3"></a>

### error/3 ###

<pre><code>
error(Orig::<a href="#type-dbus_message">dbus_message()</a>, ErrName::binary(), ErrText::binary()) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Build an error message

<a name="find_field-2"></a>

### find_field/2 ###

<pre><code>
find_field(Code::integer(), Dbus_message::<a href="#type-dbus_header">dbus_header()</a> | <a href="#type-dbus_message">dbus_message()</a>) -&gt; <a href="#type-dbus_variant">dbus_variant()</a> | undefined
</code></pre>
<br />

Find a specific field of a message

Returns `undefined` if not found

<a name="get_field-2"></a>

### get_field/2 ###

<pre><code>
get_field(Code::integer(), Header::#dbus_header{}) -&gt; <a href="#type-dbus_variant">dbus_variant()</a>
</code></pre>
<br />

throws `{no_such_field, integer()}`

Get a specific field of a message.

Throws error if not found.

<a name="get_field_value-2"></a>

### get_field_value/2 ###

<pre><code>
get_field_value(Code::integer(), Header::<a href="#type-dbus_header">dbus_header()</a>) -&gt; term()
</code></pre>
<br />

throws `{no_such_field, integer()}`

Get a field value.

Throws error if not found.

<a name="get_serial-1"></a>

### get_serial/1 ###

<pre><code>
get_serial(Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; integer()
</code></pre>
<br />

Get serial number from message

<a name="introspect-2"></a>

### introspect/2 ###

<pre><code>
introspect(Service::<a href="#type-dbus_name">dbus_name()</a>, Path::<a href="#type-dbus_name">dbus_name()</a>) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Build `Introspect` method call message

<a name="is_error-2"></a>

### is_error/2 ###

<pre><code>
is_error(Msg::<a href="#type-dbus_message">dbus_message()</a>, ErrName::<a href="#type-dbus_name">dbus_name()</a>) -&gt; boolean()
</code></pre>
<br />

Check message is an error and of the given type

<a name="match-2"></a>

### match/2 ###

<pre><code>
match(HeaderMatches::[{integer(), <a href="#type-dbus_name">dbus_name()</a> | '_'}], Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; boolean()
</code></pre>
<br />

Check message headers matches some values.

'_' means the header exists with any value

<a name="return-3"></a>

### return/3 ###

<pre><code>
return(Orig::<a href="#type-dbus_message">dbus_message()</a>, Types::[<a href="#type-dbus_type">dbus_type()</a>], Body::term()) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Build a return message

<a name="set_body-3"></a>

### set_body/3 ###

<pre><code>
set_body(Method::<a href="#type-dbus_method">dbus_method()</a>, Body::term(), Message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; <a href="#type-dbus_message">dbus_message()</a> | {error, <a href="#type-dbus_err">dbus_err()</a>}
</code></pre>
<br />

Set body of a message.

<a name="set_body-4"></a>

### set_body/4 ###

<pre><code>
set_body(Signature::binary(), Types::[<a href="#type-dbus_type">dbus_type()</a>], Body::term(), Message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; <a href="#type-dbus_message">dbus_message()</a> | {error, <a href="#type-dbus_err">dbus_err()</a>}
</code></pre>
<br />

Set body of a message.

<a name="set_serial-2"></a>

### set_serial/2 ###

<pre><code>
set_serial(Serial::integer(), Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Set serial number of a message

<a name="signal-5"></a>

### signal/5 ###

<pre><code>
signal(Destination::<a href="#type-dbus_name">dbus_name()</a>, Path::<a href="#type-dbus_name">dbus_name()</a>, Interface::<a href="#type-dbus_name">dbus_name()</a>, Signal::<a href="#type-dbus_signal">dbus_signal()</a>, Args::[<a href="#type-dbus_arg">dbus_arg()</a>]) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Equivalent to [`signal(Destination, Path, Interface, Signal, Args, [])`](#signal-6).

<a name="signal-6"></a>

### signal/6 ###

<pre><code>
signal(Destination::<a href="#type-dbus_name">dbus_name()</a>, Path::<a href="#type-dbus_name">dbus_name()</a>, Interface::<a href="#type-dbus_name">dbus_name()</a>, Signal::<a href="#type-dbus_signal">dbus_signal()</a>, Args::[<a href="#type-dbus_arg">dbus_arg()</a>], Opts::[<a href="#type-dbus_option">dbus_option()</a>]) -&gt; <a href="#type-dbus_message">dbus_message()</a>
</code></pre>
<br />

Build a signal message

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; <a href="#type-type">type()</a>
</code></pre>
<br />

Get message type

