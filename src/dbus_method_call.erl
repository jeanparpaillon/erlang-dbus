-module(dbus_method_call).
-moduledoc """
Defines a method call struct
""".
-include("dbus.hrl").

-type method_opt() ::
    {interface, binary()}
    | {destination, binary()}
    | no_reply_expected
    | no_auto_start.

-export_type([method_opt/0]).

-export([
    build/5,
    from_message/1,
    to_message/1,
    no_reply_expected/1
]).

-record(dbus_method_call, {
    method_name = undefined :: binary() | undefined,
    object_path = undefined :: binary() | undefined,
    interface = undefined :: binary() | undefined,
    destination = undefined :: binary() | undefined,
    signature = undefined :: dbus_signature() | undefined,
    body = undefined :: term() | undefined,
    no_reply_expected = false :: boolean(),
    no_auto_start = false :: boolean()
}).
-opaque t() :: #dbus_method_call{}.

-export_type([t/0]).

-spec from_message(dbus_message()) -> t().
from_message(Message) ->
    #dbus_method_call{
        method_name = dbus_message:find_field(?FIELD_MEMBER, Message),
        object_path = dbus_message:find_field(?FIELD_PATH, Message),
        interface = dbus_message:find_field(?FIELD_INTERFACE, Message),
        destination = dbus_message:find_field(?FIELD_DESTINATION, Message),
        signature = dbus_message:find_field(?FIELD_SIGNATURE, Message),
        body = dbus_message:get_body(Message)
    }.

-doc """
Build a message of type METHOD_CALL.
""".
-spec build(
    MethodName :: binary(),
    ObjectPath :: binary(),
    Signature :: dbus_signature(),
    InArgs :: term(),
    Options :: [method_opt()]
) -> {ok, t()}.
build(MethodName, ObjectPath, Signature, InArgs, Options) ->
    NoReplyExpected = proplists:get_value(no_reply_expected, Options, false),
    NoAutoStart = proplists:get_value(no_auto_start, Options, false),
    Call = #dbus_method_call{
        method_name = MethodName,
        object_path = ObjectPath,
        interface = proplists:get_value(interface, Options, undefined),
        destination = proplists:get_value(destination, Options, undefined),
        signature = Signature,
        body = InArgs,
        no_reply_expected = NoReplyExpected,
        no_auto_start = NoAutoStart
    },
    {ok, Call}.

-spec no_reply_expected(t()) -> boolean().
no_reply_expected(Call) ->
    Call#dbus_method_call.no_reply_expected.

-spec to_message(t()) -> dbus_message().
to_message(#dbus_method_call{
    method_name = MethodName,
    object_path = ObjectPath,
    interface = Interface,
    destination = Destination,
    signature = Signature,
    body = InArgs,
    no_reply_expected = NoReplyExpected,
    no_auto_start = NoAutoStart
}) ->
    Fields = [
        {?FIELD_PATH, #dbus_variant{type = object_path, value = ObjectPath}},
        {?FIELD_MEMBER, #dbus_variant{type = string, value = MethodName}}
    ],
    Fields1 =
        case Destination of
            undefined ->
                Fields;
            _ ->
                [{?FIELD_DESTINATION, #dbus_variant{type = string, value = Destination}} | Fields]
        end,
    Fields2 =
        case Interface of
            undefined ->
                Fields1;
            _ ->
                [{?FIELD_INTERFACE, #dbus_variant{type = string, value = Interface}} | Fields1]
        end,
    Flags = [
        {no_reply_expected, NoReplyExpected},
        {no_auto_start, NoAutoStart}
    ],
    Header = #dbus_header{
        type = ?TYPE_METHOD_CALL,
        flags = process_flags(Flags),
        fields = Fields2
    },
    #dbus_message{header = Header, body = {Signature, InArgs}}.

%%%
%%% Priv
%%%
process_flags(Opts) ->
    process_flags(Opts, 0).

process_flags([], Acc) ->
    Acc;
process_flags([{no_reply_expected, true} | Opts], Acc) ->
    process_flags(Opts, Acc bor ?NO_REPLY_EXPECTED);
process_flags([{no_auto_start, true} | Opts], Acc) ->
    process_flags(Opts, Acc bor ?NO_AUTO_START);
process_flags([_ | Opts], Acc) ->
    process_flags(Opts, Acc).
