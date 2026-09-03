-module(dbus_method_return).
-moduledoc """
Defines a method return struct
""".
-include("dbus.hrl").

-export([
    build/3,
    from_message/1,
    to_message/1
]).

-record(dbus_method_return, {
    reply_serial = undefined :: dbus_serial() | undefined,
    destination = undefined :: binary() | undefined,
    sender = undefined :: binary() | undefined,
    signature = undefined :: dbus_signature() | undefined,
    body = undefined :: term() | undefined
}).
-opaque t() :: #dbus_method_return{}.

-export_type([t/0]).

-spec from_message(dbus_message()) -> t().
from_message(Message) ->
    #dbus_method_return{
        reply_serial = dbus_message:find_field(?FIELD_REPLY_SERIAL, Message),
        destination = dbus_message:find_field(?FIELD_DESTINATION, Message),
        sender = dbus_message:find_field(?FIELD_SENDER, Message),
        signature = dbus_message:find_field(?FIELD_SIGNATURE, Message),
        body = dbus_message:get_body(Message)
    }.

-doc """
Build the reply to a method call.

The serial the reply refers to and its destination are taken from the call: a
reply is addressed to whoever sent the call, which on a bus is the `SENDER`
field the daemon has filled in.
""".
-spec build(
    Call :: dbus_message(),
    Types :: dbus_signature(),
    OutArgs :: [term()]
) -> t().
build(Call, Types, OutArgs) ->
    #dbus_method_return{
        reply_serial = dbus_message:get_serial(Call),
        destination = dbus_message:find_field(?FIELD_SENDER, Call),
        signature = Types,
        body = OutArgs
    }.

-spec to_message(t()) -> dbus_message().
to_message(#dbus_method_return{
    reply_serial = ReplySerial,
    destination = Destination,
    sender = Sender,
    signature = Signature,
    body = OutArgs
}) ->
    Fields = [
        {?FIELD_REPLY_SERIAL, #dbus_variant{type = uint32, value = ReplySerial}}
    ],
    Fields1 =
        case Destination of
            undefined ->
                Fields;
            _ ->
                [{?FIELD_DESTINATION, #dbus_variant{type = string, value = Destination}} | Fields]
        end,
    Fields2 =
        case Sender of
            undefined ->
                Fields1;
            _ ->
                [{?FIELD_SENDER, #dbus_variant{type = string, value = Sender}} | Fields1]
        end,
    Header = #dbus_header{
        type = ?TYPE_METHOD_RETURN,
        fields = Fields2
    },
    #dbus_message{header = Header, body = body(Signature, OutArgs)}.

%%%
%%% Priv
%%%
%% A reply carrying no out args carries no body: the `SIGNATURE' field is
%% synthesised from the body, and the marshaller expects `undefined' rather
%% than an empty one.
body(undefined, _Args) -> undefined;
body([], _Args) -> undefined;
body(Signature, Args) -> {Signature, Args}.
