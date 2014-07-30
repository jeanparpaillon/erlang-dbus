%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-ifndef(dbus_peer_hrl).
-define(dbus_peer_hrl, true).

-include("dbus.hrl").

-define(DBUS_PEER_PING, 
	#dbus_method{name= 'Ping', 
		     args=[], 
		     in_sig = <<>>, in_types=[]}).

-define(DBUS_PEER_GET_MACHINE_ID, 
	#dbus_method{name= 'GetMachineId', 
		     args=[], 
		     result=#dbus_arg{name='machine_uuid', direction=out, type = <<"s">>}, 
		     in_sig = <<>>, in_types=[]}).

-define(DBUS_PEER, 
	#dbus_iface{name='org.freedesktop.DBus.Peer',
		    methods=gb_trees:from_orddict([
						   {'GetMachineId', ?DBUS_PEER_GET_MACHINE_ID},
						   {'Ping', ?DBUS_PEER_PING}
						  ])}).

-endif.
