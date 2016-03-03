%% @copyright (C) 2014, Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Defines callbacks for implemeting an SASL authentication
%% mechanism. Authentication state machine is implemented in 
%% @see dbus_peer_conection.
%%
%% ```-callback init() ->
%%    {ok, Resp :: binary()} |
%%    {continue, Resp :: binary(), State :: term()} |
%%    {error, term()}.'''
%%
%% Returns a binary to be sent to other side.
%% * `{ok, binary()}': state-machine waits for `OK' or `REJECT'
%% * `{continue, binary()}': state-machine waits for a challenge (`DATA ...') or `REJECT'
%% * `{error, term()}': an error occurred while initializing the mechanism
%%
%% ```-callback challenge(Chall :: binary(), State :: term()) ->
%%    {ok, Resp :: binary()} |
%%    {continue, Resp :: binary(), State :: term()} |
%%    {error, Reason :: term()}.'''
%%
%% Called when receiving a challenge from the server.
%% Answers has the same meaning as init @see init/0.
%%
%% See <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#auth-mechanisms" >D-Bus Specification</a>
%% and <a href="https://tools.ietf.org/html/rfc4422" >RFC 4422</a>.
%% for complete specification of the mechanisms.
%%
%% @end
%% Created : 5 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(dbus_auth).

-callback init() ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, term()}.

-callback challenge(Chall :: binary(), State :: term()) ->
    {ok, Resp :: binary()} |
    {continue, Resp :: binary(), State :: term()} |
    {error, Reason :: term()}.
