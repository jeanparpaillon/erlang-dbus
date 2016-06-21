%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
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
-define(warn(Msg),error_logger:warning_msg(Msg)).
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
                     {array, dbus_type()} |
                     {struct, [dbus_type()]} |
                     variant |
                     {dict, dbus_type(), dbus_type()} |
                     empty.
-type dbus_signature() :: [dbus_type()].

-record(bus_id, {
          scheme,               %% tcp or unix
          options               %% * tcp: address, port
	                        %% * unix: path|abstract
         }).
-type bus_id() :: #bus_id{}.

-record(dbus_message, {
          header     :: dbus_header(),
          body       :: term() | tuple() | undefined}).
-type dbus_message() :: #dbus_message{}.

-type endianness() :: integer().         % $l (little) or $B (big)

-record(dbus_header, {
          endian   = $l                  :: endianness(),
          type                           :: integer(),
          flags    = 0                   :: integer(),
          version  = ?DBUS_VERSION_MAJOR :: integer(),
          size     = 0                   :: integer(),
          serial                         :: integer(),
          fields                         :: maps:map()
         }).
-type dbus_header() :: #dbus_header{}.

-record(dbus_variant, {
          type           :: dbus_type(),
          value          :: term()
         }).
-type dbus_variant() :: #dbus_variant{}.

-record(dbus_node, {
          name            :: binary(),
          elements   = [] :: [dbus_node()],
          interfaces      :: term()           % gb_tree()
         }).
-type dbus_node() :: #dbus_node{}.

-type dbus_annotation_name() :: 'org.freedesktop.DBus.Deprecated'
                              | 'org.freedesktop.DBus.GLib.CSymbol'
                              | 'org.freedesktop.DBus.Method.NoReply'
                              | 'org.freedesktop.DBus.Property.EmitsChangedSignal'
                              | binary().
-type dbus_annotation_value() :: true 
                               | false
                               | invalidates
                               | binary().
-type dbus_annotation() :: {dbus_annotation_name(), dbus_annotation_value()}.

-record(dbus_iface, {
          name             :: dbus_name(),
          methods          :: term(),              % gb_tree()
          signals          :: term(),              % gb_tree()
          properties       :: term(),              % gb_tree()
          annotations      :: [dbus_annotation()]
         }).
-type dbus_iface() :: #dbus_iface{}.

-record(dbus_method, {
          name             :: dbus_name(),
          args       = []  :: [dbus_arg()],
          result           :: none | undefined | dbus_arg(),
          in_sig           :: binary(),
          in_types         :: dbus_signature(),
          annotations = [] :: [dbus_annotation()]
         }).
-type dbus_method() :: #dbus_method{}.

-record(dbus_signal, {
          name             :: dbus_name(),
          args        = [] :: [dbus_arg()],
          result           :: none | dbus_arg(),
          out_sig          :: binary(),
          out_types        :: dbus_signature(),
          annotations = [] :: [dbus_annotation()]
         }).
-type dbus_signal() :: #dbus_signal{}.

-record(dbus_property, {
          name             :: dbus_name(),
          type             :: binary(),
          access           :: read | write | readwrite,
          annotations = [] :: [dbus_annotation()]
         }).
-type dbus_property() :: #dbus_property{}.

-record(dbus_arg, {
          name       = <<>> :: dbus_name(),
          direction         :: in | out,
          type              :: binary()
         }).
-type dbus_arg() :: #dbus_arg{}.

-endif.
