

# Module dbus_hex #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) 2014 Jean Parpaillon

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert hex string into an integer.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Convert a binary (for example the result of erlang:md5/1)
to a hex binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Bin::binary()) -&gt; integer()
</code></pre>
<br />

Convert hex string into an integer.

<a name="to-1"></a>

### to/1 ###

<pre><code>
to(Binary::binary()) -&gt; binary()
</code></pre>
<br />

Convert a binary (for example the result of erlang:md5/1)
to a hex binary.

