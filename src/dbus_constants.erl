%%
%% @copyright 2014 Jean Parpaillon
%%
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%
%% @doc DBUS constants to atom
%% @end
-module(dbus_constants).

-export([
	 to_atom/1
	]).

-spec to_atom(binary()) -> atom() | binary().
to_atom(<<"Introspect">>)                                  -> 'Introspect';
to_atom(<<"org.freedesktop.DBus.Error.UnknownMethod">>)    -> 'org.freedesktop.DBus.Error.UnknownMethod';
to_atom(Bin) -> Bin.
