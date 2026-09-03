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

-export([
    get_serial/1,
    set_serial/2,
    find_field/2,
    fd/2
]).

-doc "Get serial number from message.".
-spec get_serial(dbus_message()) -> dbus_serial().
get_serial(#dbus_message{header = #dbus_header{serial = Serial}}) ->
    Serial.

-doc "Set serial number of a message.".
-spec set_serial(dbus_serial(), dbus_message()) -> dbus_message().
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
