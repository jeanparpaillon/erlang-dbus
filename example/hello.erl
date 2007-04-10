-module(hello).

-include("dbus.hrl").

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
	 'HelloWorld'/1,
	 'HelloWorld'/3,
	 'GetTuple'/1,
	 'GetTuple'/3,
	 'GetDict'/1,
	 'GetDict'/3,
	 'OnClick'/1
	]).

%% gen_dbus callbacks
-export([init/1]).


-record(state, {
	 }).

start_link(Service, Path) ->
    gen_dbus:start_link({local, hello}, ?MODULE, [Service, Path], []).


init([Service, Path]) ->
    State = #state{},
    {ok, {Service, Path, [
			  {interface, 'org.designfu.SampleInterface'},
			  {methods, ['HelloWorld', 'GetTuple', 'GetDict']},
			  {signals, ['OnClick']}
			 ]}, State}.

'OnClick'(dbus_info) ->
    [{signature, [string, string], []}].

'HelloWorld'(dbus_info) ->
    [{interface, 'org.designfu.SampleInterface'},
     {signature, [string], [{array, string}]}].


'HelloWorld'([Id, Hello_message], From, State) when is_integer(Id) ->
%%     {dbus_error, 'org.freedesktop.DBus.Error.InvalidArgs', "Invalid args", State}.

    self() ! {hello, [Id, Hello_message], From},
    {noreply, State}.

%%     io:format("HelloWorld: ~p, ~p~n", [Id, Hello_message]),
%% %%     on_click(17, 123),
%%     {reply, ["Hello", " from Erlang service.erl"], State}.

'GetTuple'(dbus_info) ->
    [{signature, [], [{struct, [string, string]}]}].

'GetTuple'([], _From, State) ->
    {reply, {"Hello Tuple", " from Erlang service.erl"}, State}.


'GetDict'(dbus_info) ->
    [{interface, 'org.designfu.SampleInterface'}].

'GetDict'([], _From, State) ->
    List = [{1, "Hello"},
	    {2, " from Erlang service.erl"}],
    Dict = dict:from_list(List),

    {reply, #variant{type={dict, byte, string}, value=Dict}, State}.
%%     {reply, #variant{type={dict, string, string},value=List}, State}.
%%      {reply, #variant{type={array, {struct, [string, string]}},value=List}, State}.

%% on_click([], State) ->

%%     @dbus.service.signal("org.designfu.SampleInterface")
%%     def OnClick(self, x, y):
%%         pass

handle_info({hello, [Id, Hello_message], From}, State) ->
    io:format("HelloWorld: callback ~p, ~p~n", [Id, Hello_message]),
    gen_dbus:signal('OnClick', ["x", "y"]),
    gen_dbus:reply(From, {ok, ["Hello callback", " from Erlang service.erl"]}),
%%     gen_dbus:reply(From, {dbus_error, 'org.freedesktop.DBus.Error.Timeout', "Error from Erlang service.erl"}),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.
