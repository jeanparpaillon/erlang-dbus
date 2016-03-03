

# Module dbus_auth_anonymous #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Implements ANONYMOUS authentication.

Copyright (c) (C) 2014, Jean Parpaillon

__Behaviours:__ [`dbus_auth`](dbus_auth.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [RFC 2245](https://tools.ietf.org.md/rfc2245)
for complete specification.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#challenge-2">challenge/2</a></td><td>Not implemented: ANONYMOUS does not require challenge.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>Init ANONYMOUS mechanism.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="challenge-2"></a>

### challenge/2 ###

<pre><code>
challenge(X1::binary(), X2::any()) -&gt; {error, invalid_challenge}
</code></pre>
<br />

Not implemented: ANONYMOUS does not require challenge

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; {ok, binary()}
</code></pre>
<br />

Init ANONYMOUS mechanism

