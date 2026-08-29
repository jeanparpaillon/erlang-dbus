-module(dbus_transport).
-moduledoc """
Transport behaviour for a D-Bus transport.
""".

-export_type([
    address/0,
    option/0,
    connection/0
]).

-type address() :: map().
-type option() :: term().
-type connection() :: term().

-callback connect(Address :: address(), Options :: [option()]) ->
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
