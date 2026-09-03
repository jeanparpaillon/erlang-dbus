-module(dbus_message).
-moduledoc """
Creates, update `t:dbus_message()` structures.
""".

-include("dbus.hrl").

-elvis([
    {elvis_style, export_used_types, #{ignore => [dbus_message]}}
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type method_opt() ::
    {interface, binary()}
    | {destination, binary()}
    | no_reply_expected
    | no_auto_start.

-type casted() ::
    dbus_method_call:t()
    | dbus_method_return:t()
    | dbus_signal:t().

-export_type([method_opt/0, casted/0]).

-export([
    cast/1,
    get_serial/1,
    set_serial/2,
    get_type/1,
    get_body/1,
    find_field/2,
    find_field/3,
    fd/2
]).

-doc "Cast message into proper struct".
-spec cast(dbus_message()) -> casted().
cast(Message) ->
    case get_type(Message) of
        method_call -> dbus_method_call:from_message(Message);
        method_return -> dbus_method_return:from_message(Message);
        signal -> dbus_signal:from_message(Message)
    end.

-doc "Get serial number from message.".
-spec get_serial(dbus_message()) -> dbus_serial().
get_serial(#dbus_message{header = #dbus_header{serial = Serial}}) ->
    Serial.

-doc "Set serial number of a message.".
-spec set_serial(dbus_serial(), dbus_message()) -> dbus_message().
set_serial(Serial, #dbus_message{header = Header} = Message) ->
    Header2 = Header#dbus_header{serial = Serial},
    Message#dbus_message{header = Header2}.

-doc "Returns message type".
-spec get_type(dbus_message()) -> dbus_message_type().
get_type(#dbus_message{header = #dbus_header{type = Type}}) ->
    type_name(Type).

-doc "Returns message body".
-spec get_body(dbus_message()) -> term().
get_body(#dbus_message{body = Body}) ->
    Body.

-doc """
Find a specific field of a message, or of a message header.

The header form is what the marshaller needs: while unmarshaling, the header is
decoded before the body, so there is no message to look the signature field up in
yet.

Returns `Default` if not found.
""".
-spec find_field(Code :: integer(), dbus_message() | dbus_header(), Default) ->
    Default.
find_field(Code, #dbus_message{header = Header}, Default) ->
    find_field(Code, Header, Default);
find_field(Code, #dbus_header{fields = Fields}, Default) ->
    proplists:get_value(Code, Fields, Default).

find_field(Code, MessageOrHeader) ->
    find_field(Code, MessageOrHeader, undefined).

-doc """
Resolve a `unix_fd` value against the descriptors a message carries.

The value of an `h` on the wire is an index into the array of file descriptors
that accompany the message, not a descriptor, so a body value has to be looked up
here before it can be used. This is where an index is validated -- bodies are not
scanned for `h` values while unmarshaling, so an out-of-range one is an error
only when someone asks for it.

The descriptor belongs to the owner of the message: the library will not close
it.
""".
-spec fd(Index :: non_neg_integer(), dbus_message()) ->
    {ok, non_neg_integer()} | {error, {bad_fd_index, term()}}.
fd(Index, #dbus_message{fds = Fds}) when is_integer(Index), Index >= 0, Index < length(Fds) ->
    {ok, lists:nth(Index + 1, Fds)};
fd(Index, #dbus_message{}) ->
    {error, {bad_fd_index, Index}}.

%%%
%%% Priv
%%%
type_name(?TYPE_INVALID) -> invalid;
type_name(?TYPE_METHOD_CALL) -> method_call;
type_name(?TYPE_METHOD_RETURN) -> method_return;
type_name(?TYPE_ERROR) -> error;
type_name(?TYPE_SIGNAL) -> signal.

%%%
%%% Tests
%%%
-ifdef(TEST).

fd_message() ->
    #dbus_message{header = #dbus_header{serial = 1}, fds = [11, 12]}.

fd_resolves_an_index_test_() ->
    Msg = fd_message(),
    [
        ?_assertEqual({ok, 11}, fd(0, Msg)),
        ?_assertEqual({ok, 12}, fd(1, Msg))
    ].

fd_out_of_range_test_() ->
    Msg = fd_message(),
    [
        ?_assertEqual({error, {bad_fd_index, 2}}, fd(2, Msg)),
        ?_assertEqual({error, {bad_fd_index, -1}}, fd(-1, Msg)),
        ?_assertEqual({error, {bad_fd_index, 0}}, fd(0, #dbus_message{}))
    ].

-endif.
