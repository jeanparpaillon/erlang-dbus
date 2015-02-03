%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_errors_hrl).
-define(dbus_errors_hrl, true).

-type dbus_err_name() :: 'org.freedesktop.DBus.InvalidParameters'
		       | 'org.freedesktop.DBus.UnknownMethod'
		       | 'org.freedesktop.DBus.UnknownInterface'
		       | 'org.freedesktop.DBus.UnknownSignal'.
-type dbus_err_value() :: term().
-type dbus_err() :: {dbus_err_name(), dbus_err_value()}.

-endif.
