%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(dbus_objman).

-include("dbus_client.hrl").

-export([init/0,
	 add_objects/2]).

-record(state, {objects}).

-spec init() -> #state{}.
init() ->
    #state{objects=[]}.

-spec add_objects(Objects :: list(), #state{}) -> {ok, #state{}}.
add_objects([], State) ->
    {ok, State};
add_objects([{Path, Ifaces} | Rest], #state{objects=Objects}=State) ->
    lager:debug("New object ~p", [Path]),
    add_objects(Rest, State#state{objects=[{Path, Ifaces} | Objects]}).

%%%
%%% Priv
%%%
