-module(hello).

-import(error_logger).
-import(dberl.gen_dbus).
-import(io).

-behaviour(dberl.gen_dbus).

%% api
-export([
	 start_link/2,
	 handle_info/2
	]).

%% dbus object callbacks
-export([
	 'HelloWorld'/3,
	 hello_world/1,
	 hello_world/2,
	 get_tuple/1,
	 get_tuple/2,
	 get_dict/1,
	 get_dict/2
	]).

%% gen_dbus callbacks
-export([init/1]).


-record(state, {
	 }).

start_link(Service, Path) ->
    gen_dbus:start_link(Service, Path, ?MODULE, [], []).


init([]) ->
    State = #state{},
    {ok, [{interface, 'org.designfu.SampleInterface'},
	  {members, [hello_world, get_tuple, get_dict]}],
     State}.


hello_world(dbus_info) ->
    [{type, method},
     {interface, 'org.designfu.SampleInterface'},
     {signature, [string], [{array, string}]}].
    

'HelloWorld'([Id, Hello_message], From, State) ->
    {dbus_error, 'org.freedesktop.DBus.Error.InvalidArgs', "Invalid args", State}.

%%     self() ! {hello, [Id, Hello_message], From},
%%     {noreply, State}.

%%     io:format("HelloWorld: ~p, ~p~n", [Id, Hello_message]),
%% %%     on_click(17, 123),
%%     {reply, ["Hello", " from Erlang service.erl"], State}.

hello_world([Hello_message], State) ->
    io:format("~p~n", [Hello_message]),
%%     on_click(17, 123),
    {reply, ["Hello", " from Erlang service.erl"], State}.


get_tuple(dbus_info) ->
    [{signature, [], [{struct, [string, string]}]}].

get_tuple([], State) ->
    {reply, {"Hello Tuple", " from Erlang service.erl"}, State}.


get_dict(dbus_info) ->
    [{interface, 'org.designfu.SampleInterface'}].

get_dict([], State) ->
    {reply, [{"first", "Hello Dict"},
	     {"second", " from Erlang service.erl"}], State}.

%% on_click([], State) ->

%%     @dbus.service.signal("org.designfu.SampleInterface")
%%     def OnClick(self, x, y):
%%         pass

handle_info({hello, [Id, Hello_message], From}, State) ->
    io:format("HelloWorld: callback ~p, ~p~n", [Id, Hello_message]),
    %%     on_click(17, 123),
%%     gen_dbus:reply(From, {ok, ["Hello callback", " from Erlang service.erl"]}),
    gen_dbus:reply(From, {dbus_error, 'org.freedesktop.DBus.Error.Timeout', "Error from Erlang service.erl"}),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.
