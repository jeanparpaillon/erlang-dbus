

# Module dbus_names #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Transform well-known D-Bus names from binary to atoms.

Copyright (c) 2014 Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin_to_error-1">bin_to_error/1</a></td><td>Well-known error names.</td></tr><tr><td valign="top"><a href="#bin_to_iface-1">bin_to_iface/1</a></td><td>Well-known interface names:.</td></tr><tr><td valign="top"><a href="#bin_to_member-1">bin_to_member/1</a></td><td>Well-known method OR signal name.</td></tr><tr><td valign="top"><a href="#bin_to_method-1">bin_to_method/1</a></td><td>Well-known method names.</td></tr><tr><td valign="top"><a href="#bin_to_signal-1">bin_to_signal/1</a></td><td>Well-known signal names.</td></tr><tr><td valign="top"><a href="#list_to_iface-1">list_to_iface/1</a></td><td>Equivalent to <a href="#bin_to_iface-1"><tt>bin_to_iface(list_to_binary(Str))</tt></a>.</td></tr><tr><td valign="top"><a href="#list_to_method-1">list_to_method/1</a></td><td>Equivalent to <a href="#bin_to_method-1"><tt>bin_to_method(list_to_binary(Str))</tt></a>.</td></tr><tr><td valign="top"><a href="#list_to_signal-1">list_to_signal/1</a></td><td>Equivalent to <a href="#bin_to_signal-1"><tt>bin_to_signal(list_to_binary(Str))</tt></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin_to_error-1"></a>

### bin_to_error/1 ###

<pre><code>
bin_to_error(Bin::binary()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Well-known error names

* `org.freedesktop.DBus.Error.NameHasNoOwner`
* `org.freedesktop.DBus.Error.OOM`
* `org.freedesktop.DBus.Error.MatchRuleNotFound`

<a name="bin_to_iface-1"></a>

### bin_to_iface/1 ###

<pre><code>
bin_to_iface(Bin::binary()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Well-known interface names:

* `org.freedesktop.DBus`
* `org.freedesktop.DBus.Peer`
* `org.freedesktop.DBus.Introspectable`
* `org.freedesktop.DBus.Properties`
* `org.freedesktop.DBus.ObjectManager`

<a name="bin_to_member-1"></a>

### bin_to_member/1 ###

<pre><code>
bin_to_member(Bin::binary()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Well-known method OR signal name

<a name="bin_to_method-1"></a>

### bin_to_method/1 ###

<pre><code>
bin_to_method(Bin::binary()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Well-known method names

* `AddMatch`
* `GetAdtAuditSessionData`
* `GetConnectionCredentials`
* `GetConnectionProcessID`
* `GetConnectionSELinuxSecurityContext`
* `GetConnectionUnixUser`
* `GetId`
* `GetNameOwner`
* `Hello`
* `ListActivatableNames`
* `ListNames`
* `NameHasOwner`
* `ReleaseName`
* `RemoveMatch`
* `RequestName`
* `StartServiceByName`
* `UpdateActivationEnvironment`
* `Ping`
* `GetmachineId`
* `Introspect`
* `Get`
* `Set`
* `GetAll`
* `GetManagedObjects`

<a name="bin_to_signal-1"></a>

### bin_to_signal/1 ###

<pre><code>
bin_to_signal(Bin::binary()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Well-known signal names

* `NameAcquired`
* `NameLost`
* `NameAcquired`
* `PropertiesChanged`
* `InterfacesAdded`
* `InterfacesRemoved`

<a name="list_to_iface-1"></a>

### list_to_iface/1 ###

<pre><code>
list_to_iface(Str::string()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Equivalent to [`bin_to_iface(list_to_binary(Str))`](#bin_to_iface-1).

<a name="list_to_method-1"></a>

### list_to_method/1 ###

<pre><code>
list_to_method(Str::string()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Equivalent to [`bin_to_method(list_to_binary(Str))`](#bin_to_method-1).

<a name="list_to_signal-1"></a>

### list_to_signal/1 ###

<pre><code>
list_to_signal(Str::string()) -&gt; <a href="#type-dbus_name">dbus_name()</a>
</code></pre>
<br />

Equivalent to [`bin_to_signal(list_to_binary(Str))`](#bin_to_signal-1).

