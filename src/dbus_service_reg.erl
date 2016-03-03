%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc Register of exported D-BUS services gen_server
%% @todo Check if the code is still working, why it is here, ...
%% @end
-module(dbus_service_reg).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/0,
	 export_service/1
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
	  service,
	  services=[],				% {Service_name}
	  connections=[]			% {Name, Conn}
	 }).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

export_service(ServiceName) ->
    gen_server:call(?SERVER, {export_service, ServiceName}).

%%
%% gen_server callbacks
%%
init([]) ->
    %%process_flag(trap_exit, true),
    dbus_bus_reg:set_service_reg(self()),
    {ok, Service} = dbus_service:start_link(dummy),
    {ok, #state{service=Service}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({export_service, ServiceName}, _From, State) ->
    Services = State#state.services,
    case lists:keysearch(ServiceName, 1, Services) of
	{value, {_, Service}} ->
	    {reply, {ok, Service}, State};
	_ ->
	    ?debug("export_service name ~p~n", [ServiceName]),
	    Service = State#state.service,
	    ok = dbus_bus_reg:export_service(undefined, ServiceName),
	    Services1 = [{ServiceName, Service}|Services],
	    {reply, {ok, Service}, State#state{services=Services1}}
    end;

handle_call(Request, _From, State) ->
    ?error("Unhandled call in: ~p~n", [Request]),
    {reply, ok, State}.


handle_cast(Request, State) ->
    ?error("Unhandled cast in: ~p~n", [Request]),
    {noreply, State}.


handle_info({dbus_method_call, Header, Conn}, State) ->
    Service = State#state.service,
    Service ! {dbus_method_call, Header, Conn},
    {noreply, State};

handle_info({new_bus, _Bus}, State) ->
    Fun = fun({ServiceName, Service}) ->
		  ok = dbus_bus_reg:export_service(Service, ServiceName)
	  end,
    lists:foreach(Fun, State#state.services),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    Services = State#state.services,
    case lists:keysearch(Pid, 2, Services) of
	{value, {ServiceName, _}} ->
	    ?debug("~p Terminated ~p~n", [Pid, Reason]),
	    ok = dbus_bus_reg:unexport_service(Pid, ServiceName),
	    Services1 =
		lists:keydelete(Pid, 2, Services),
	    {noreply, State#state{services=Services1}};
	false ->
	    if
		Reason /= normal ->
		    {stop, Reason};
		true ->
		    {noreply, State}
	    end
    end;

handle_info(Info, State) ->
    ?debug("Unhandled info in: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
