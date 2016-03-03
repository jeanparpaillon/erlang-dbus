

# Module dbus_auth_cookie_sha1 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Implements COOKIE_SHA1 authentication mechanism.

Copyright (c) (C) 2014, Jean Parpaillon

__Behaviours:__ [`dbus_auth`](dbus_auth.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="description"></a>

## Description ##
See [D-Bus Specification](https://dbus.freedesktop.org/doc/dbus-specification.md#auth-mechanisms-sha)
for complete specification.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#challenge-2">challenge/2</a></td><td>Answer challenge.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>Initialize DBUS_AUTH_COOKIE_SHA1 authentication.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="challenge-2"></a>

### challenge/2 ###

`challenge(Chall, X2) -> any()`

Answer challenge

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

Initialize DBUS_AUTH_COOKIE_SHA1 authentication

