%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_object_manager_hrl).
-define(dbus_object_manager_hrl, true).

-include("dbus.hrl").

-define(DBUS_OBJECT_GET_MANAGED_OBJECTS, 
	#dbus_method{name= 'GetManagedObjects', 
		     args=[], 
		     result=#dbus_arg{name='objects', direction=out, type = <<"{o{s{sv}}}">>}, 
		     in_sig = <<>>, in_types=[]}).


-define(DBUS_OBJECT_INTERFACES_ADDED, 
	#dbus_signal{name= 'InterfacesAdded', 
		     args=[#dbus_arg{name='object_path', type= <<"o">>},
			   #dbus_arg{name='interfaces', type= <<"{s{sv}}">>}], 
		     out_sig = <<"o{s{sv}}">>, out_types=[object_path, {dict, string, {dict, string, variant}}]}).

-define(DBUS_OBJECT_INTERFACES_REMOVED, 
	#dbus_signal{name= 'InterfacesRemoved', 
		     args=[#dbus_arg{name='object_path', type= <<"o">>},
			   #dbus_arg{name='interfaces', type= <<"as">>}], 
		     out_sig = <<"oas">>, out_types=[object_path, {array, string}]}).

-define(DBUS_OBJECT_MANAGER_IFACE, 'org.freedesktop.DBus.ObjectManager').

-define(DBUS_OBJECT_MANAGER, 
	#dbus_iface{name='org.freedesktop.DBus.ObjectManager',
		    methods=gb_trees:from_orddict([
						   {'GetManagedObjects', ?DBUS_OBJECT_GET_MANAGED_OBJECTS}
						  ]),
		    signals=gb_trees:from_orddict([
						   {'InterfacesAdded', ?DBUS_OBJECT_INTERFACES_ADDED},
						   {'InterfacesRemoved', ?DBUS_OBJECT_INTERFACES_REMOVED}
						  ])}).

-endif.
