-elvis([
    {elvis_style, no_types, #{ignore => [dbus]}}
]).

-ifndef(DBUS_HRL).
-define(DBUS_HRL, true).

-define(DBUS_VERSION_MAJOR, 1).

-define(DBUS_AUTH_EXTERNAL, <<"EXTERNAL">>).
-define(DBUS_AUTH_DBUS_COOKIE_SHA1, <<"DBUS_COOKIE_SHA1">>).
-define(DBUS_AUTH_ANONYMOUS, <<"ANONYMOUS">>).

-define(TYPE_INVALID, 0).
-define(TYPE_METHOD_CALL, 1).
-define(TYPE_METHOD_RETURN, 2).
-define(TYPE_ERROR, 3).
-define(TYPE_SIGNAL, 4).

-define(FIELD_INVALID, 0).
-define(FIELD_PATH, 1).
-define(FIELD_INTERFACE, 2).
-define(FIELD_MEMBER, 3).
-define(FIELD_ERROR_NAME, 4).
-define(FIELD_REPLY_SERIAL, 5).
-define(FIELD_DESTINATION, 6).
-define(FIELD_SENDER, 7).
-define(FIELD_SIGNATURE, 8).

-type endianness() ::
    $l
    | $B.

-type dbus_message_type() ::
    ?TYPE_INVALID
    | ?TYPE_METHOD_CALL
    | ?TYPE_METHOD_RETURN
    | ?TYPE_ERROR
    | ?TYPE_SIGNAL.

-type dbus_type() ::
    byte
    | boolean
    | int16
    | uint16
    | int32
    | uint32
    | int64
    | uint64
    | double
    | string
    | object_path
    | signature
    | {array, dbus_type()}
    | {struct, [dbus_type()]}
    | variant
    | {dict, dbus_type(), dbus_type()}
    | empty.
-type dbus_signature() :: [dbus_type()].

-record(dbus_variant, {
    type = empty :: dbus_type(),
    value :: term()
}).
-type dbus_variant() :: #dbus_variant{}.

-type dbus_serial() :: non_neg_integer().

-record(dbus_header, {
    endian = $l :: endianness(),
    type = ?TYPE_INVALID :: dbus_message_type(),
    flags = 0 :: integer(),
    version = ?DBUS_VERSION_MAJOR :: integer(),
    size = 0 :: integer(),
    serial = 0 :: dbus_serial(),
    fields = [] :: list()
}).
-type dbus_header() :: #dbus_header{}.

-record(dbus_message, {
    header = #dbus_header{} :: dbus_header(),
    body = undefined :: undefined | {dbus_signature(), term()}
}).

-type dbus_message() :: #dbus_message{}.

%% The transport name, verbatim, as written in the address. Known names are
%% `unix', `launchd', `systemd', `tcp', `nonce-tcp', `unixexec' and
%% `autolaunch', but the format is extensible and dbus_address only checks
%% syntax, so any name matching the literal set may appear here.
-type scheme() :: binary().

-type dbus_address_option() :: atom().

-record(dbus_address, {
    scheme :: scheme(),
    guid = undefined :: binary() | undefined,
    options = [] :: [{dbus_address_option(), binary()}]
}).
-type dbus_address() :: #dbus_address{}.

-endif.
