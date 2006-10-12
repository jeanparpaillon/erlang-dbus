
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


-record(message, {
	  header,
	  body}).

-record(header, {
	  type,
	  flags=0,
	  serial,
	  headers
	 }).

-record(variant, {
	  type,
	  value
	 }).

-record(node, {
	  name,					% atom()
	  elements,				% [node()]
	  interfaces				% [#interface()] | unknown
	 }).

-record(interface, {
	  name,					% atom
	  methods,				% [#member]
	  signals,				% [#member]
	  properties				% [#propery]
	  }).

-record(method, {
	  name,					% atom
	  args,					% [#arg]
	  result,				% #arg | none
	  in_sig,				% string()
	  in_types
	 }).

-record(signal, {
	  name,					% atom
	  args,					% [#arg]
	  result,				% #arg | none
	  out_sig,				% string()
	  out_types
	 }).

-record(property, {
	  name,					% atom()
	  type,					% string()
	  access				% read | write | readwrite
	  }).

-record(arg, {
	  name,					% atom() | none
	  direction,				% in | out
	  type					% string()
	 }).
