%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_dbus_hrl).
-define(dbus_dbus_hrl, true).

-include("dbus.hrl").


-define(DBUS_SERVICE, 'org.freedesktop.DBus').
-define(DBUS_IFACE, 'org.freedesktop.DBus').
-define(DBUS_PATH, <<"/org/freedesktop/DBus">>).

-define(DBUS_DBUS_HELLO, 
	#dbus_method{name= 'Hello', 
		     args=[], 
		     result=#dbus_arg{direction=out, type = <<"s">>}, 
		     in_sig = <<>>, in_types=[]}).

-define(DBUS_DBUS_LIST_NAMES, 
	#dbus_method{name= 'ListNames', 
		     args=[], 
		     result=#dbus_arg{direction=out, type = <<"as">>}, 
		     in_sig = <<>>, in_types=[]}).

-define(DBUS_DBUS_LIST_ACTIVATABLE_NAMES, 
	#dbus_method{name= 'ListActivatableNames', 
		     args=[], 
		     result=#dbus_arg{direction=out, type = <<"as">>}, 
		     in_sig = <<>>, in_types=[]}).

-define(DBUS_DBUS_NAME_HAS_OWNER, 
	#dbus_method{name= 'NameHasOwner', 
		     args=[#dbus_arg{name='name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type = <<"b">>}, 
		     in_sig = <<"s">>, in_types=[string]}).

-define(DBUS_DBUS_NAME_OWNER_CHANGED, 
	#dbus_signal{name= 'NameOwnerChanged', 
		     args=[#dbus_arg{name='name', type= <<"s">>},
			   #dbus_arg{name='old_owner', type= <<"s">>},
			   #dbus_arg{name='new_owner', type= <<"s">>}], 
		     out_sig = <<"sss">>, out_types=[string, string, string]}).

-define(DBUS_DBUS_NAME_LOST, 
	#dbus_signal{name= 'NameLost', 
		     args=[#dbus_arg{name='name', type= <<"s">>}], 
		     out_sig = <<"s">>, out_types=[string]}).

-define(DBUS_DBUS_NAME_ACQUIRED, 
	#dbus_signal{name= 'NameAcquired', 
		     args=[#dbus_arg{name='name', type= <<"s">>}], 
		     out_sig = <<"s">>, out_types=[string]}).

-define(DBUS_DBUS_START_SERVICE_BY_NAME, 
	#dbus_method{name= 'StartServiceByName', 
		     args=[#dbus_arg{name='name', direction=in, type= <<"s">>},
			   #dbus_arg{name='flags', direction=in, type= <<"u">>}], 
		     result=#dbus_arg{direction=out, type= <<"u">>},
		     in_sig = <<"su">>, 
		     in_types=[string, uint32]}).

-define(DBUS_DBUS_UPDATE_ACTIVATION_ENVIRONMENT,
	#dbus_method{name= 'UpdateActivationEnvironment', 
		     args=[#dbus_arg{name='environment', direction=in, type= <<"a{ss}">>}], 
		     in_sig = <<"a{ss}">>, 
		     in_types=[{array, {dict, string, string}}]}).

-define(DBUS_DBUS_GET_NAME_OWNER,
	#dbus_method{name= 'GetNameOwner', 
		     args=[#dbus_arg{name='name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"s">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_CONNECTION_UNIX_USER,
	#dbus_method{name= 'GetConnectionUnixUser', 
		     args=[#dbus_arg{name='bus_name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"u">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_CONNECTION_PROCESS_ID,
	#dbus_method{name= 'GetConnectionProcessID', 
		     args=[#dbus_arg{name='bus_name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"u">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_CONNECTION_CREDENTIALS,
	#dbus_method{name= 'GetConnectionCredentials', 
		     args=[#dbus_arg{name='bus_name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"{sv}">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_ADT_AUDIT_SESSION_DATA,
	#dbus_method{name= 'GetAdtAuditSessionData', 
		     args=[#dbus_arg{name='bus_name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"ay">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_CONNECTION_SELINUX_SECURITY_CONTEXT,
	#dbus_method{name= 'GetConnectionSELinuxSecurityContext',
		     args=[#dbus_arg{name='bus_name', direction=in, type= <<"s">>}], 
		     result=#dbus_arg{direction=out, type= <<"ay">>},
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_ADD_MATCH, 
	#dbus_method{name= 'AddMatch', 
		     args=[#dbus_arg{name='rule', direction=in, type= <<"s">>}], 
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_REMOVE_MATCH, 
	#dbus_method{name= 'RemoveMatch', 
		     args=[#dbus_arg{name='rule', direction=in, type= <<"s">>}], 
		     in_sig = <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS_GET_ID, 
	#dbus_method{name= 'GetId', 
		     args=[], 
		     result=#dbus_arg{direction=out, type= <<"s">>},
		     in_sig = <<>>, 
		     in_types=[]}).

-define(DBUS_DBUS_REQUEST_NAME, 
	#dbus_method{name = 'RequestName', 
		     args=[#dbus_arg{direction=in, type = <<"s">>}, 
			   #dbus_arg{direction=in, type = <<"u">>}, 
			   #dbus_arg{direction=out, type = <<"u">>}], 
		     in_sig = <<"su">>, 
		     in_types=[string,uint32]}).

-define(DBUS_DBUS_RELEASE_NAME, 
	#dbus_method{name= 'ReleaseName', 
		     args=[#dbus_arg{direction=in, type = <<"s">>}, 
			   #dbus_arg{direction=out, type = <<"u">>}], 
		     in_sig= <<"s">>, 
		     in_types=[string]}).

-define(DBUS_DBUS, 
	#dbus_iface{name=?DBUS_SERVICE,
		    methods=gb_trees:from_orddict([
						   {'AddMatch', ?DBUS_DBUS_ADD_MATCH},
						   {'GetAdtAuditSessionData',  ?DBUS_DBUS_GET_ADT_AUDIT_SESSION_DATA},
						   {'GetConnectionCredentials', ?DBUS_DBUS_GET_CONNECTION_CREDENTIALS},
						   {'GetConnectionProcessID', ?DBUS_DBUS_GET_CONNECTION_PROCESS_ID},
						   {'GetConnectionSELinuxSecurityContext', ?DBUS_DBUS_GET_CONNECTION_SELINUX_SECURITY_CONTEXT},
						   {'GetConnectionUnixUser', ?DBUS_DBUS_GET_CONNECTION_UNIX_USER},
						   {'GetId', ?DBUS_DBUS_GET_ID},
						   {'GetNameOwner', ?DBUS_DBUS_GET_NAME_OWNER},
						   {'Hello', ?DBUS_DBUS_HELLO},
						   {'ListActivatableNames', ?DBUS_DBUS_LIST_ACTIVATABLE_NAMES},
						   {'ListNames', ?DBUS_DBUS_LIST_NAMES},
						   {'NameHasOwner', ?DBUS_DBUS_NAME_HAS_OWNER},
						   {'ReleaseName', ?DBUS_DBUS_RELEASE_NAME},
						   {'RemoveMatch', ?DBUS_DBUS_REMOVE_MATCH},
						   {'RequestName', ?DBUS_DBUS_REQUEST_NAME},
						   {'StartServiceByName', ?DBUS_DBUS_START_SERVICE_BY_NAME},
						   {'UpdateActivationEnvironment', ?DBUS_DBUS_UPDATE_ACTIVATION_ENVIRONMENT}
						  ]),
		    signals=gb_trees:from_orddict([
						   {'NameAcquired', ?DBUS_DBUS_NAME_ACQUIRED},
						   {'NameLost', ?DBUS_DBUS_NAME_LOST},
						   {'NameOwnerChanged', ?DBUS_DBUS_NAME_OWNER_CHANGED}
						  ])}).

-define(DBUS_NODE, #dbus_node{elements=[], 
			      interfaces=gb_trees:from_orddict(
					  [
					   {?DBUS_IFACE, ?DBUS_DBUS},
					   {'org.freedesktop.DBus.Introspectable', ?DBUS_INTROSPECTABLE}
					  ])}
       ).

-endif.
