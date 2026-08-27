-ifndef(dbus_hrl).
-define(dbus_hrl, true).

-ifndef(debug).
-define(debug(Msg), error_logger:info_msg(Msg)).
-define(debug(Msg, Data), error_logger:info_msg(Msg, Data)).
-endif.

-ifndef(info).
-define(info(Msg), error_logger:info_msg(Msg)).
-define(info(Msg, Data), error_logger:info_msg(Msg, Data)).
-endif.

-ifndef(warn).
-define(warn(Msg), error_logger:warning_msg(Msg)).
-define(warn(Msg, Data), error_logger:warning_msg(Msg, Data)).
-endif.

-ifndef(error).
-define(error(Msg), error_logger:error_msg(Msg)).
-define(error(Msg, Data), error_logger:error_msg(Msg, Data)).
-endif.

-define(DBUS_VERSION_MAJOR, 1).

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

-define(NO_REPLY_EXPECTED, 1).
-define(NO_AUTO_START, 2).

-define(DBUS_NAME_FLAG_ALLOW_REPLACEMENT, 1).
-define(DBUS_NAME_FLAG_REPLACE_EXISTING, 2).
-define(DBUS_NAME_FLAG_DO_NOT_QUEUE, 4).

-define(DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER, 1).
-define(DBUS_REQUEST_NAME_REPLY_IN_QUEUE, 2).
-define(DBUS_REQUEST_NAME_REPLY_EXISTS, 3).
-define(DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER, 4).

-type dbus_connection() :: {atom(), any()}.

-type dbus_known_bus() :: system | session.
-type dbus_name() :: atom() | binary().
-type dbus_path() :: atom() | binary().
-type dbus_option() :: no_reply_expected | no_auto_start.
%% Transports defined by the specification, sections "Transports" and
%% "Meta Transports", keeping their spec spelling, hence the quoted 'nonce-tcp'.
%% `systemd' is listenable but not connectable; `unix' and `tcp' also have
%% listen-only forms (dir, tmpdir, runtime). Telling those apart is the
%% transport layer's job, not the address parser's.
%%
%% The address format is extensible, so dbus_address:parse/1 accepts a
%% transport it does not know rather than rejecting it: the trailing atom()
%% is what makes an unknown scheme well-typed. It costs the union its
%% checking value -- dialyzer collapses the whole thing to atom() -- so the
%% names below are documentation of the known set, not a constraint.

%% transports-unix-domain-sockets
-type dbus_address_scheme() ::
    unix
    %% transports-launchd
    | launchd
    %% transports-systemd
    | systemd
    %% transports-tcp-sockets
    | tcp
    %% transports-nonce-tcp-sockets
    | 'nonce-tcp'
    %% transports-exec
    | unixexec
    %% transports-autolaunch
    | autolaunch
    %% unknown or future transport
    | atom().

-type dbus_address_option() :: atom().

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

-record(bus_id, {
    scheme :: dbus_address_scheme(),
    guid :: binary() | undefined,
    options :: [{dbus_address_option(), binary()}]
}).
-type bus_id() :: #bus_id{}.

-record(dbus_message, {
    header :: dbus_header() | undefined,
    body :: term() | tuple() | undefined
}).
-type dbus_message() :: #dbus_message{}.

% $l (little) or $B (big)
-type endianness() :: integer().

-record(dbus_header, {
    endian = $l :: endianness(),
    type :: integer() | undefined,
    flags = 0 :: integer(),
    version = ?DBUS_VERSION_MAJOR :: integer(),
    size = 0 :: integer(),
    serial :: integer() | undefined,
    fields :: map() | list() | undefined
}).
-type dbus_header() :: #dbus_header{}.

-record(dbus_variant, {
    type :: dbus_type() | undefined,
    value :: term()
}).
-type dbus_variant() :: #dbus_variant{}.

-record(dbus_node, {
    name :: binary() | undefined,
    elements = [] :: [dbus_node()],
    % gb_tree()
    interfaces :: term()
}).
-type dbus_node() :: #dbus_node{}.

-type dbus_annotation_name() ::
    'org.freedesktop.DBus.Deprecated'
    | 'org.freedesktop.DBus.GLib.CSymbol'
    | 'org.freedesktop.DBus.Method.NoReply'
    | 'org.freedesktop.DBus.Property.EmitsChangedSignal'
    | binary().
-type dbus_annotation_value() ::
    true
    | false
    | invalidates
    | binary().
-type dbus_annotation() :: {dbus_annotation_name(), dbus_annotation_value()}.

-record(dbus_iface, {
    name :: dbus_name() | undefined,
    % gb_tree()
    methods :: term(),
    % gb_tree()
    signals :: term(),
    % gb_tree()
    properties :: term(),
    annotations :: [dbus_annotation()] | undefined
}).
-type dbus_iface() :: #dbus_iface{}.

-record(dbus_method, {
    name :: dbus_name() | undefined,
    args = [] :: [dbus_arg()],
    result :: none | undefined | dbus_arg(),
    in_sig :: binary() | undefined,
    in_types :: dbus_signature() | undefined,
    annotations = [] :: [dbus_annotation()]
}).
-type dbus_method() :: #dbus_method{}.

-record(dbus_signal, {
    name :: dbus_name() | undefined,
    args = [] :: [dbus_arg()],
    result :: none | undefined | dbus_arg(),
    out_sig :: binary() | undefined,
    out_types :: dbus_signature() | undefined,
    annotations = [] :: [dbus_annotation()]
}).
-type dbus_signal() :: #dbus_signal{}.

-record(dbus_property, {
    name :: dbus_name() | undefined,
    type :: binary() | undefined,
    access :: read | write | readwrite | undefined,
    annotations = [] :: [dbus_annotation()]
}).
-type dbus_property() :: #dbus_property{}.

-record(dbus_arg, {
    name = <<>> :: dbus_name(),
    direction :: in | out | undefined,
    type :: iolist() | binary() | undefined
}).
-type dbus_arg() :: #dbus_arg{}.

-endif.
