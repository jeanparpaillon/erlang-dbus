-module(dbus_demo_hello).

-include("dbus.hrl").

-behaviour(gen_dbus).

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
	 'RaiseException'/3
	]).

%% gen_dbus callbacks
-export([init/1]).


-record(state, {
	 }).

start_link(Service, Path) ->
    gen_dbus:start_link({local, ?MODULE}, ?MODULE, [Service, Path], []).


init([Service, Path]) ->
    State = #state{},
    {ok, {Service, Path, [
			  {interface, 'com.example.SampleInterface'},
			  {methods, ['HelloWorld', 'GetTuple', 'GetDict']},
			  {signals, []}
			 ]}, State}.

'HelloWorld'(dbus_info) ->
    [{interface, 'com.example.SampleInterface'},
     {signature, [string], [{array, string}]}].

'HelloWorld'([Hello_message], From, State) ->
    self() ! {hello, [Hello_message], From},
    {noreply, State}.

'RaiseException'([], From, State) ->
    {dbus_error, "com.example.DemoException",
     "The RaiseException method does what you might expect", State}.

'GetTuple'(dbus_info) ->
    [{signature, [], [{struct, [string, string]}]}].

'GetTuple'([], _From, State) ->
    {reply, {"Hello Tuple", " from " ++ get_source_name()}, State}.


'GetDict'(dbus_info) ->
    [{interface, 'com.example.SampleInterface'}].

'GetDict'([], _From, State) ->
    List = [{"first", "Hello"},
	    {"second", " from " ++ get_source_name()}],
    Dict = dict:from_list(List),

    {reply, #variant{type={dict, string, string}, value=Dict}, State}.


handle_info({hello, [Hello_message], From}, State) ->
    io:format("HelloWorld: callback ~p~n", [Hello_message]),
    gen_dbus:reply(From, {ok, ["Hello callback", " from Erlang " ++ get_source_name(), "with unique name", "FIXME"]}),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


get_source_name() ->
    atom_to_list(?MODULE) ++ ".erl".
