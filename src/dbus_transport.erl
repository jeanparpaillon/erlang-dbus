-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".

-export_type([
    connection/0
]).

-type connection() :: term().

-callback connect(Address :: dbus_address:t()) ->
    {ok, Connection :: connection()}
    | {error, Reason :: term()}.

-callback send(Connection :: connection(), Data :: iodata()) ->
    ok
    | {error, Reason :: term()}.

-callback recv(
    Connection :: connection(),
    Length :: non_neg_integer(),
    Timeout :: timeout()
) ->
    {ok, Data :: binary()}
    | {error, closed | timeout | term()}.

-callback close(Connection :: connection()) ->
    ok.
