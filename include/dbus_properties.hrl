%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_properties_hrl).
-define(dbus_properties_hrl, true).

-include("dbus.hrl").

-define(DBUS_PROPERTIES_GET, 
	#dbus_method{name= 'Get', 
		     args=[#dbus_arg{name='interface_name', type= <<"s">>},
			   #dbus_arg{name='property_name', type= <<"s">>}], 
		     result=#dbus_arg{name='value', direction=out, type = <<"v">>}, 
		     in_sig = <<"ss">>, in_types=[string, string]}).

-define(DBUS_PROPERTIES_SET, 
	#dbus_method{name= 'Set', 
		     args=[#dbus_arg{name='interface_name', type= <<"s">>},
			   #dbus_arg{name='property_name', type= <<"s">>},
			   #dbus_arg{name='value', type= <<"v">>}], 
		     in_sig = <<"ssv">>, in_types=[string, string, variant]}).

-define(DBUS_PROPERTIES_GET_ALL, 
	#dbus_method{name= 'GetAll', 
		     args=[#dbus_arg{name='interface_name', type= <<"s">>}], 
		     result=#dbus_arg{name='props', direction=out, type = <<"{sv}">>}, 
		     in_sig = <<"s">>, in_types=[string]}).

-define(DBUS_PROPERTIES_PROPERTIES_CHANGED, 
	#dbus_signal{name= 'PropertiesChanged', 
		     args=[#dbus_arg{name='name', type= <<"s">>},
			   #dbus_arg{name='changed_properties', type= <<"{sv}">>},
			   #dbus_arg{name='invalidated_properties', type= <<"as">>}], 
		     out_sig = <<"s{sv}as">>, out_types=[string, {dict, string, variant}, {array, string}]}).

-define(DBUS_PROPERTIES, 
	#dbus_iface{name='org.freedesktop.DBus.Properties',
		    methods=gb_trees:from_orddict([
						   {'Get', ?DBUS_PROPERTIES_GET},
						   {'GetAll', ?DBUS_PROPERTIES_GET_ALL},
						   {'Set', ?DBUS_PROPERTIES_SET}
						  ]),
		    signals=gb_trees:from_orddict([
						   {'PropertiesChanged', ?DBUS_PROPERTIES_PROPERTIES_CHANGED}
						  ])}).

-endif.
