-module(dbus_method_return).
-moduledoc """
Functions for D-Bus messages of type `METHOD_RETURN`.
""".
-include("dbus.hrl").

-export([
    build/3,
    cast/1
]).

-spec build(dbus_message(), dbus_signature(), term()) -> dbus_message().
build(Orig, Signature, Body) ->
    From = dbus_message:find_field(?FIELD_SENDER, Orig),
    Fields = [
        {?FIELD_REPLY_SERIAL, #dbus_variant{type = uint32, value = dbus_message:get_serial(Orig)}},
        {?FIELD_DESTINATION, #dbus_variant{type = string, value = From}},
        {?FIELD_SIGNATURE, #dbus_variant{type = signature, value = Signature}}
    ],
    Header = #dbus_header{type = ?TYPE_METHOD_RETURN, fields = Fields},
    #dbus_message{header = Header, body_sig = Signature, body = Body}.

-spec cast(dbus_message()) -> term().
cast(Message) ->
    dbus_message:get_body(Message).
