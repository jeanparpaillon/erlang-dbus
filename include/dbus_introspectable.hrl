%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_introspectable_hrl).
-define(dbus_introspectable_hrl, true).

-include("dbus.hrl").

-define(DBUS_INTROSPECTABLE_INTROSPECT, 
	#dbus_method{name ='Introspect', 
		     args=[], 
		     result=#dbus_arg{direction=out, type = <<"s">>}, 
		     in_sig= <<>>, in_types=[]}).

-define(DBUS_INTROSPECTABLE_IFACE, 'org.freedesktop.DBus.Introspectable').

-define(DBUS_INTROSPECTABLE, 
	#dbus_iface{name=?DBUS_INTROSPECTABLE_IFACE,
		    methods=gb_trees:from_orddict([{'Introspect', ?DBUS_INTROSPECTABLE_INTROSPECT}])}).

-endif.
