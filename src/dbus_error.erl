-module(dbus_error).
-moduledoc """
Defines a D-Bus error struct
""".
-include("dbus.hrl").

-export([
    build/3,
    build/4,
    from_message/1,
    to_message/1
]).

-type error_name() :: binary() | atom().

-export_type([error_name/0]).

-record(dbus_error, {
    error_name = undefined :: error_name() | undefined,
    reply_serial = undefined :: dbus_serial() | undefined,
    destination = undefined :: binary() | undefined,
    sender = undefined :: binary() | undefined,
    signature = undefined :: dbus_signature() | undefined,
    body = undefined :: term() | undefined
}).
-opaque t() :: #dbus_error{}.

-export_type([t/0]).

-spec from_message(dbus_message()) -> t().
from_message(Message) ->
    #dbus_error{
        error_name = dbus_message:find_field(?FIELD_ERROR_NAME, Message),
        reply_serial = dbus_message:find_field(?FIELD_REPLY_SERIAL, Message),
        destination = dbus_message:find_field(?FIELD_DESTINATION, Message),
        sender = dbus_message:find_field(?FIELD_SENDER, Message),
        signature = dbus_message:find_field(?FIELD_SIGNATURE, Message),
        body = dbus_message:get_body(Message)
    }.

-doc """
Build the error reply to a method call.

The convention every D-Bus binding follows is a single string argument holding
the human-readable message; `undefined` builds an error carrying no body at
all, which is equally legal. Use `build/4` for anything else.
""".
-spec build(
    Call :: dbus_message(),
    ErrName :: error_name(),
    ErrText :: binary() | undefined
) -> t().
build(Call, ErrName, undefined) ->
    build(Call, ErrName, undefined, undefined);
build(Call, ErrName, ErrText) ->
    build(Call, ErrName, [string], [ErrText]).

-doc """
Build the error reply to a method call, with an arbitrary body.

As for `dbus_method_return:build/3`, the serial the reply refers to and its
destination are taken from the call.
""".
-spec build(
    Call :: dbus_message(),
    ErrName :: error_name(),
    Types :: dbus_signature() | undefined,
    OutArgs :: [term()] | undefined
) -> t().
build(Call, ErrName, Types, OutArgs) ->
    #dbus_error{
        error_name = ErrName,
        reply_serial = dbus_message:get_serial(Call),
        destination = dbus_message:find_field(?FIELD_SENDER, Call),
        signature = Types,
        body = OutArgs
    }.

-spec to_message(t()) -> dbus_message().
to_message(#dbus_error{
    error_name = ErrName,
    reply_serial = ReplySerial,
    destination = Destination,
    sender = Sender,
    signature = Signature,
    body = OutArgs
}) ->
    Fields = [
        {?FIELD_ERROR_NAME, #dbus_variant{type = string, value = ErrName}},
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
        type = ?TYPE_ERROR,
        fields = Fields2
    },
    #dbus_message{header = Header, body = body(Signature, OutArgs)}.

%%%
%%% Priv
%%%
%% An error carrying no argument carries no body: the `SIGNATURE' field is
%% synthesised from the body, and the marshaller expects `undefined' rather
%% than an empty one.
body(undefined, _Args) -> undefined;
body([], _Args) -> undefined;
body(Signature, Args) -> {Signature, Args}.
