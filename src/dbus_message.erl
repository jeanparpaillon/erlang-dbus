-module(dbus_message).
-moduledoc """
handle D-Bus messages
""".

-include("dbus.hrl").

-elvis([
    {elvis_style, export_used_types, #{ignore => [dbus_message]}}
]).

-export([
    get_serial/1,
    set_serial/2,
    find_field/2
]).

-doc "Get serial number from message.".
-spec get_serial(dbus_message()) -> serial().
get_serial(#dbus_message{header = #dbus_header{serial = Serial}}) ->
    Serial.

-doc "Set serial number of a message.".
-spec set_serial(serial(), dbus_message()) -> dbus_message().
set_serial(Serial, #dbus_message{header = Header} = Message) ->
    Header2 = Header#dbus_header{serial = Serial},
    Message#dbus_message{header = Header2}.

-doc """
Find a specific field of a message, or of a message header.

The header form is what the marshaller needs: while unmarshaling, the header is
decoded before the body, so there is no message to look the signature field up in
yet.

Returns `undefined` if not found.
""".
-spec find_field(Code :: integer(), dbus_message() | dbus_header()) ->
    term() | undefined.
find_field(Code, #dbus_message{header = Header}) ->
    find_field(Code, Header);
find_field(Code, #dbus_header{fields = Fields}) ->
    proplists:get_value(Code, Fields, undefined).
