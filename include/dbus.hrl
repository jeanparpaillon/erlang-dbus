
-define(DBUS_VERSION_MAJOR, 1).

-define(TYPE_INVALID, 0).
-define(TYPE_METHOD_CALL, 1).
-define(TYPE_METHOD_RETURN, 2).
-define(TYPE_ERROR, 3).
-define(TYPE_SIGNAL, 4).

-define(HEADER_INVALID, 0).
-define(HEADER_PATH, 1).
-define(HEADER_INTERFACE, 2).
-define(HEADER_MEMBER, 3).
-define(HEADER_ERROR_NAME, 4).
-define(HEADER_REPLY_SERIAL, 5).
-define(HEADER_DESTINATION, 6).
-define(HEADER_SENDER, 7).
-define(HEADER_SIGNATURE, 8).

-define(NO_REPLY_EXPECTED, 1).
-define(NO_AUTO_START, 2).

-define(DBUS_NAME_FLAG_ALLOW_REPLACEMENT, 1).
-define(DBUS_NAME_FLAG_REPLACE_EXISTING, 2).
-define(DBUS_NAME_FLAG_DO_NOT_QUEUE, 4).

-define(DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER, 1).
-define(DBUS_REQUEST_NAME_REPLY_IN_QUEUE, 2).
-define(DBUS_REQUEST_NAME_REPLY_EXISTS, 3).
-define(DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER, 4).

-type dbus_name() :: atom() | binary().
-type dbus_option() :: no_reply_expected | no_auto_start.

-type dbus_type() :: byte | 
		     boolean |
		     int16 |
		     uint16 | 
		     int32 |
		     uint32 |
		     int64 |
		     uint64 |
		     double |
		     string |
		     object_path |
		     signature |
		     struct |
		     variant |
		     dict_entry.

-record(bus_id, {
	  scheme,				% tcp or unix
	  options				% tcp: address, port
						% unix: path|abstract
	 }).

-record(dbus_message, {
	  header,
	  body}).
-type dbus_message() :: #dbus_message{}.

-record(dbus_header, {
	  type                           :: integer(),
	  flags    = 0                   :: integer(),
	  version  = ?DBUS_VERSION_MAJOR :: integer(),
	  size     = 0                   :: integer(),
	  serial                         :: integer(),
	  fields                         :: [dbus_variant()]
	 }).
-type dbus_header() :: #dbus_header{}.

-record(dbus_variant, {
	  type           :: integer(),
	  value          :: term()
	 }).
-type dbus_variant() :: #dbus_variant{}.

-record(dbus_node, {
	  name            :: dbus_name(),
	  elements   = [] :: [dbus_node()],
	  interfaces = [] :: [dbus_iface()]
	 }).
-type dbus_node() :: #dbus_node{}.

-record(dbus_iface, {
	  name            :: dbus_name(),
	  methods    = [] :: [dbus_name()],
	  signals    = [] :: dbus_name(),
	  properties = [] :: [dbus_property()]
	  }).
-type dbus_iface() :: #dbus_iface{}.

-record(dbus_method, {
	  name            :: dbus_name(),
	  args            :: [dbus_arg()],
	  result          :: none | undefined | dbus_arg(),
	  in_sig          :: binary(),
	  in_types
	 }).
-type dbus_method() :: #dbus_method{}.

-record(dbus_signal, {
	  name            :: dbus_name(),
	  args            :: [dbus_arg()],
	  result          :: none | dbus_arg(),
	  out_sig         :: binary(),
	  out_types
	 }).
-type dbus_signal() :: #dbus_signal{}.

-record(dbus_property, {
	  name            :: dbus_name(),
	  type            :: binary(),
	  access          :: read | write | readwrite
	  }).
-type dbus_property() :: #dbus_property{}.

-record(dbus_arg, {
	  name            :: dbus_name(),
	  direction       :: in | out,
	  type            :: binary()
	 }).
-type dbus_arg() :: #dbus_arg{}.
