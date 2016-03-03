%%
%% @copyright 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc Bus registry
%%
%% @todo Finish, test, ...
%% @end
-module(dbus_bus_reg).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/0,
	 get_bus/1,
	 release_bus/1,
	 export_service/2,
	 unexport_service/2,
	 set_service_reg/1,
	 cast/1
	]).

%% gen_server callbacks
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-record(state, {
	  busses=[],
	  service_reg
	 }).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_bus(#bus_id{}) -> {ok, pid()}.
get_bus(#bus_id{}=BusId) ->
    gen_server:call(?SERVER, {get_bus, BusId}).

-spec release_bus(pid()) -> ok | {error, not_registered}.
release_bus(Bus) when is_pid(Bus) ->
    gen_server:call(?SERVER, {release_bus, Bus}).

export_service(Service, ServiceName) ->
    gen_server:call(?SERVER, {export_service, Service, ServiceName}).

unexport_service(Service, ServiceName) ->
    gen_server:call(?SERVER, {unexport_service, Service, ServiceName}).

set_service_reg(ServiceReg) ->
    gen_server:cast(?SERVER, {set_service_reg, ServiceReg}).

cast(#dbus_message{}=Msg) ->
    gen_server:cast(?SERVER, Msg).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_bus, #bus_id{}=BusId}, _From, #state{busses=Busses}=State) ->
    case proplists:get_value(BusId, Busses) of
	undefined ->
	    {ok, Bus} = dbus_bus:connect(BusId),
	    Busses1 = [{BusId, Bus} | Busses],
	    {reply, {ok, Bus}, State#state{busses=Busses1}};
	Bus ->
	    {reply, {ok, Bus}, State}
    end;

handle_call({release_bus, Bus}, _From, #state{busses=Busses}=State) when is_pid(Bus) ->
    case lists:keysearch(Bus, 2, Busses) of
	{value, {BusId, _Bus}} ->
            ?debug("Release BusId ~p~n", [BusId]),
            Busses1 = lists:keydelete(Bus, 2, Busses),
            ok = dbus_bus:stop(Bus),
            {reply, ok, State#state{busses=Busses1}};
	false ->
	    {reply, {error, not_registered}, State}
    end;

handle_call({export_service, _Service, ServiceName}, _From, #state{busses=Busses}=State) ->
    Fun = fun({_, Bus}) ->
		  ?debug("export_service bus ~p~n", [Bus]),
		  ok = dbus_bus:export_service(Bus, ServiceName)
	  end,
    ?debug("export_service name ~p~n", [ServiceName]),
    lists:foreach(Fun, Busses),
    {reply, ok, State};

handle_call({unexport_service, _Service, ServiceName}, _From, #state{busses=Busses}=State) ->
    Fun = fun({_, Bus}) ->
		  ?debug("~p unexport_service bus ~p~n", [?MODULE, Bus]),
		  ok = dbus_bus:unexport_service(Bus, ServiceName)
	  end,
    ?debug("~p unexport_service name ~p~n", [?MODULE, ServiceName]),
    lists:foreach(Fun, Busses),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({set_service_reg, ServiceReg}, State) ->
    {noreply, State#state{service_reg=ServiceReg}};

handle_cast(#dbus_message{}=Msg, #state{busses=Buses}=State) ->
    Fun = fun({_, Bus}) ->
		  dbus_bus:cast(Bus, Msg)
	  end,
    lists:foreach(Fun, Buses),    
    {noreply, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
