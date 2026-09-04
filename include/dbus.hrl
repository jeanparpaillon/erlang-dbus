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
-define(FIELD_UNIX_FDS, 9).

%% The header flags a METHOD_CALL may carry. `ALLOW_INTERACTIVE_AUTHORIZATION'
%% is the one added after 1.0 (specification 0.30, `dbus-daemon' 1.10): a
%% caller sets it to say it can wait while the peer's policy asks the user,
%% and a peer that does not know the flag ignores it.
-define(NO_REPLY_EXPECTED, 1).
-define(NO_AUTO_START, 2).
-define(ALLOW_INTERACTIVE_AUTHORIZATION, 4).

%% The number of file descriptors one message may carry. `libdbus' allows 16,
%% and `dbus-daemon' publishes the number as `max_message_unix_fds' --
%% `/usr/share/dbus-1/system.conf' carries the stock value, commented out, at
%% 16.
-define(MAX_UNIX_FDS, 16).

-type endianness() ::
    $l
    | $B.

-type dbus_message_type_code() ::
    ?TYPE_INVALID
    | ?TYPE_METHOD_CALL
    | ?TYPE_METHOD_RETURN
    | ?TYPE_ERROR
    | ?TYPE_SIGNAL.

-type dbus_message_type() ::
    invalid
    | method_call
    | method_return
    | error
    | signal.

-type dbus_header_field() :: {integer(), term()}.

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
    | unix_fd
    | string
    | object_path
    | signature
    | {array, dbus_type()}
    | {struct, [dbus_type()]}
    | variant
    | {dict, dbus_type(), dbus_type()}
    | empty.

-type dbus_arg_index() :: non_neg_integer().
-type dbus_signature() :: [dbus_type()].

-record(dbus_variant, {
    type = empty :: dbus_type(),
    value :: term()
}).
-type dbus_variant() :: #dbus_variant{}.

-type dbus_serial() :: non_neg_integer().

-type dbus_match_rule() ::
    {type, dbus_message_type()}
    | {sender, binary()}
    | {interface, binary()}
    | {member, binary()}
    | {path, binary()}
    | {path_namespace, binary()}
    | {destination, binary()}
    | {arg, {dbus_arg_index(), binary()}}
    | {arg_path, {dbus_arg_index(), binary()}}
    | {arg0_namespace, binary()}
    | {eavesdrop, boolean()}.

-record(dbus_header, {
    endian = $l :: endianness(),
    type = ?TYPE_INVALID :: dbus_message_type_code(),
    flags = 0 :: integer(),
    version = ?DBUS_VERSION_MAJOR :: integer(),
    size = 0 :: integer(),
    serial = 0 :: dbus_serial(),
    fields = [] :: [dbus_header_field()]
}).
-type dbus_header() :: #dbus_header{}.

-record(dbus_message, {
    header = #dbus_header{} :: dbus_header(),
    body_sig = undefined :: undefined | dbus_signature(),
    body = undefined :: undefined | term(),
    %% The file descriptors that accompany the message. A `unix_fd' value in
    %% the body is an index into this list, not a descriptor;
    %% `dbus_message:fd/2' resolves one. The `UNIX_FDS' header field is
    %% synthesised from it while marshalling, so a caller sets this and never
    %% the field.
    fds = [] :: [non_neg_integer()]
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

-record(dbus_auth, {
    guid :: binary() | undefined,
    agree_unix_fd = false :: boolean()
}).
-type dbus_auth() :: #dbus_auth{}.

-endif.
