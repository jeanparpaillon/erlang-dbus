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
    build/2,
    build/3,
    build/4,
    no_reply_expected/1
]).

-spec build(
    MethodName :: binary(),
    ObjectPath :: binary()
) -> dbus_message().
build(MethodName, ObjectPath) ->
    build(MethodName, ObjectPath, [], []).

-spec build(
    MethodName :: binary(),
    ObjectPath :: binary(),
    Options :: [method_opt()]
) -> dbus_message().
build(MethodName, ObjectPath, Options) ->
    build(MethodName, ObjectPath, {[], []}, Options).

-doc """
Build a message of type METHOD_CALL.
""".
-spec build(
    MethodName :: binary(),
    ObjectPath :: binary(),
    InArgs :: {dbus_signature(), term()} | [],
    Options :: [method_opt()]
) -> dbus_message().
build(MethodName, ObjectPath, [], Options) ->
    build(MethodName, ObjectPath, {[], []}, Options);
build(MethodName, ObjectPath, {Signature, InArgs}, Options) ->
    NoReplyExpected = proplists:get_value(no_reply_expected, Options, false),
    NoAutoStart = proplists:get_value(no_auto_start, Options, false),
    Interface = proplists:get_value(interface, Options, undefined),
    Destination = proplists:get_value(destination, Options, undefined),
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
    #dbus_message{header = Header, body_sig = Signature, body = InArgs}.

-spec no_reply_expected(dbus_message()) -> boolean().
no_reply_expected(#dbus_message{header = #dbus_header{flags = Flags}}) ->
    (Flags band ?NO_REPLY_EXPECTED) =/= 0.

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
