-module(dbus_constants).
-moduledoc "DBUS constants to atom.".

-export([
         to_atom/1
        ]).

-spec to_atom(binary()) -> atom() | binary().
to_atom(<<"Introspect">>)                                  -> 'Introspect';
to_atom(<<"org.freedesktop.DBus.Error.UnknownMethod">>)    -> 'org.freedesktop.DBus.Error.UnknownMethod';
to_atom(Bin) -> Bin.
