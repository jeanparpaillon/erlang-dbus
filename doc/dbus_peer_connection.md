

# Module dbus_peer_connection #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Peer connections.

Copyright (c) 2015 Jean Parpaillon

__Behaviours:__ [`dbus_connection`](dbus_connection.md), [`gen_fsm`](gen_fsm.md).

__Authors:__ Jean Parpaillon ([`jean.parpaillon@free.fr`](mailto:jean.parpaillon@free.fr)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth-1">auth/1</a></td><td>Launch authentication on this connection
No message can be sent before authentication.</td></tr><tr><td valign="top"><a href="#authenticated-3">authenticated/3</a></td><td></td></tr><tr><td valign="top"><a href="#call-2">call/2</a></td><td>Synchronously send a message.</td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td>Asynchronously send a message.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close the connection.</td></tr><tr><td valign="top"><a href="#code_change-4">code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#connected-3">connected/3</a></td><td>Default is to use EXTERNAL mechanism first.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-3">handle_info/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_sync_event-4">handle_sync_event/4</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_controlling_process-2">set_controlling_process/2</a></td><td>Change controlling process for the connection.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Equivalent to <a href="#start_link-2"><tt>start_link(BusId, [list, {packet, 0}])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Start a connection to a peer.</td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td></td></tr><tr><td valign="top"><a href="#waiting_for_agree-3">waiting_for_agree/3</a></td><td></td></tr><tr><td valign="top"><a href="#waiting_for_data-3">waiting_for_data/3</a></td><td></td></tr><tr><td valign="top"><a href="#waiting_for_ok-3">waiting_for_ok/3</a></td><td></td></tr><tr><td valign="top"><a href="#waiting_for_reject-3">waiting_for_reject/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth-1"></a>

### auth/1 ###

<pre><code>
auth(Conn::pid()) -&gt; {ok, ConnexionId::undefined | binary()} | {error, term()}
</code></pre>
<br />

Launch authentication on this connection
No message can be sent before authentication.

<a name="authenticated-3"></a>

### authenticated/3 ###

`authenticated(Evt, From, State) -> any()`

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(Conn::pid(), Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Synchronously send a message

<a name="cast-2"></a>

### cast/2 ###

<pre><code>
cast(Conn::pid(), Dbus_message::<a href="#type-dbus_message">dbus_message()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Asynchronously send a message

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Conn::pid()) -&gt; ok
</code></pre>
<br />

Close the connection

<a name="code_change-4"></a>

### code_change/4 ###

`code_change(OldVsn, StateName, State, Extra) -> any()`

<a name="connected-3"></a>

### connected/3 ###

`connected(Evt, From, State) -> any()`

Default is to use EXTERNAL mechanism first. If it fails, server
will answer with list of possible authentications.
Mimic C implementation

<a name="handle_event-3"></a>

### handle_event/3 ###

`handle_event(Evt, StateName, State) -> any()`

<a name="handle_info-3"></a>

### handle_info/3 ###

`handle_info(Evt, StateName, State) -> any()`

<a name="handle_sync_event-4"></a>

### handle_sync_event/4 ###

`handle_sync_event(Evt, From, StateName, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="set_controlling_process-2"></a>

### set_controlling_process/2 ###

<pre><code>
set_controlling_process(Connection::pid(), Client::pid()) -&gt; ok | {error, unauthorized}
</code></pre>
<br />

Change controlling process for the connection.

If called by someone else than current owner, `{error, unauthorized}` is returned.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(BusId::<a href="#type-bus_id">bus_id()</a>) -&gt; {ok, <a href="#type-dbus_connection">dbus_connection()</a>} | {error, term()}
</code></pre>
<br />

Equivalent to [`start_link(BusId, [list, {packet, 0}])`](#start_link-2).

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(BusId::<a href="#type-bus_id">bus_id()</a>, Options::list()) -&gt; {ok, <a href="#type-dbus_connection">dbus_connection()</a>} | {error, term()}
</code></pre>
<br />

Start a connection to a peer

<a name="terminate-3"></a>

### terminate/3 ###

`terminate(Reason, StateName, State) -> any()`

<a name="waiting_for_agree-3"></a>

### waiting_for_agree/3 ###

`waiting_for_agree(Evt, From, State) -> any()`

<a name="waiting_for_data-3"></a>

### waiting_for_data/3 ###

`waiting_for_data(Evt, From, State) -> any()`

<a name="waiting_for_ok-3"></a>

### waiting_for_ok/3 ###

`waiting_for_ok(Evt, From, State) -> any()`

<a name="waiting_for_reject-3"></a>

### waiting_for_reject/3 ###

`waiting_for_reject(Evt, From, State) -> any()`

