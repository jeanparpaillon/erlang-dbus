%%%
%%% @doc       D-BUS application module
%%% @author    Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright 2014 Jean Parpaillon
%%%
-module(dbus_names).

-include("dbus.hrl").

-export([list_to_iface/1,
	 list_to_method/1,
	 list_to_signal/1]).

-export([bin_to_iface/1,
	 bin_to_method/1,
	 bin_to_signal/1,
	 bin_to_member/1,
	 bin_to_error/1]).

-spec list_to_iface(string()) -> dbus_name().
list_to_iface(Str) when is_list(Str) ->
    bin_to_iface(list_to_binary(Str)).


-spec list_to_method(string()) -> dbus_name().
list_to_method(Str) when is_list(Str) ->
    bin_to_method(list_to_binary(Str)).


-spec list_to_signal(string()) -> dbus_name().
list_to_signal(Str) when is_list(Str) ->
    bin_to_signal(list_to_binary(Str)).


-spec bin_to_iface(binary()) -> dbus_name().
bin_to_iface(<<"org.freedesktop.DBus">>) -> 'org.freedesktop.DBus';
bin_to_iface(<<"org.freedesktop.DBus.Peer">>) -> 'org.freedesktop.DBus.Peer';
bin_to_iface(<<"org.freedesktop.DBus.Introspectable">>) -> 'org.freedesktop.DBus.Introspectable';
bin_to_iface(<<"org.freedesktop.DBus.Properties">>) -> 'org.freedesktop.DBus.Properties';
bin_to_iface(<<"org.freedesktop.DBus.ObjectManager">>) -> 'org.freedesktop.DBus.ObjectManager';
bin_to_iface(Bin) when is_binary(Bin) -> Bin.


-spec bin_to_method(binary()) -> dbus_name().
bin_to_method(<<"AddMatch">>) -> 'AddMatch';
bin_to_method(<<"GetAdtAuditSessionData">>) -> 'GetAdtAuditSessionData';
bin_to_method(<<"GetConnectionCredentials">>) -> 'GetConnectionCredentials';
bin_to_method(<<"GetConnectionProcessID">>) -> 'GetConnectionProcessID';
bin_to_method(<<"GetConnectionSELinuxSecurityContext">>) -> 'GetConnectionSELinuxSecurityContext';
bin_to_method(<<"GetConnectionUnixUser">>) -> 'GetConnectionUnixUser';
bin_to_method(<<"GetId">>) -> 'GetId';
bin_to_method(<<"GetNameOwner">>) -> 'GetNameOwner';
bin_to_method(<<"Hello">>) -> 'Hello';
bin_to_method(<<"ListActivatableNames">>) -> 'ListActivatableNames';
bin_to_method(<<"ListNames">>) -> 'ListNames';
bin_to_method(<<"NameHasOwner">>) -> 'NameHasOwner';
bin_to_method(<<"ReleaseName">>) -> 'ReleaseName';
bin_to_method(<<"RemoveMatch">>) -> 'RemoveMatch';
bin_to_method(<<"RequestName">>) -> 'RequestName';
bin_to_method(<<"StartServiceByName">>) -> 'StartServiceByName';
bin_to_method(<<"UpdateActivationEnvironment">>) -> 'UpdateActivationEnvironment';
bin_to_method(<<"Ping">>) -> 'Ping';
bin_to_method(<<"GetMachineId">>) -> 'GetmachineId';
bin_to_method(<<"Introspect">>) -> 'Introspect';
bin_to_method(<<"Get">>) -> 'Get';
bin_to_method(<<"Set">>) -> 'Set';
bin_to_method(<<"GetAll">>) -> 'GetAll';
bin_to_method(<<"GetManagedObjects">>) -> 'GetManagedObjects';
bin_to_method(Bin) when is_binary(Bin) -> Bin.


-spec bin_to_signal(binary()) -> dbus_name().
bin_to_signal(<<"NameAcquired">>) -> 'NameAcquired';
bin_to_signal(<<"NameLost">>) -> 'NameLost';
bin_to_signal(<<"NameOwnerChanged">>) -> 'NameAcquired';
bin_to_signal(<<"PropertiesChanged">>) -> 'PropertiesChanged';
bin_to_signal(<<"InterfacesAdded">>) -> 'InterfacesAdded';
bin_to_signal(<<"InterfacesRemoved">>) -> 'InterfacesRemoved';
bin_to_signal(Bin) when is_binary(Bin) -> Bin.


-spec bin_to_member(binary()) -> dbus_name().
bin_to_member(Bin) ->
    case bin_to_method(Bin) of
	Bin2 when is_binary(Bin2) ->
	    bin_to_signal(Bin2);
	Atom ->
	    Atom
    end.

-spec bin_to_error(binary()) -> dbus_name().
bin_to_error(<<"org.freedesktop.DBus.Error.NameHasNoOwner">>) -> 'org.freedesktop.DBus.Error.NameHasNoOwner';
bin_to_error(<<"org.freedesktop.DBus.Error.OOM">>) -> 'org.freedesktop.DBus.Error.OOM';
bin_to_error(<<"org.freedesktop.DBus.Error.MatchRuleNotFound">>) -> 'org.freedesktop.DBus.Error.MatchRuleNotFound';
bin_to_error(Bin) when is_binary(Bin) -> Bin.
