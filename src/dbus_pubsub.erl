-module(dbus_pubsub).
-moduledoc """
Simple pubsub registry based on pg module
""".
-define(SCOPE, dbus_pubsub).

-export([
    start_link/0,
    subscribe/2,
    publish/2
]).

-type group() :: term().

-doc """
Ensure pubsub registry is started.

Started on demand by connection. In case it is already started, link to the
calling process anyway : a connection should not survive the `pg` scope death.
""".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case pg:start_link(?SCOPE) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            true = link(Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec subscribe(group(), pid()) -> ok.
subscribe(Group, Subscriber) ->
    case lists:member(Subscriber, pg:get_members(?SCOPE, Group)) of
        true -> ok;
        false -> pg:join(?SCOPE, Group, Subscriber)
    end.

-spec publish(group(), term()) -> ok.
publish(Group, Message) ->
    lists:foreach(
        fun(Pid) ->
            Pid ! Message
        end,
        pg:get_members(?SCOPE, Group)
    ).
