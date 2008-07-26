%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc Bus registry
%%

-module(dbus_bus_reg).

-behaviour(gen_server).

-include("dbus.hrl").

% api
-export([
	 start_link/0,
	 get_bus/1,
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_bus(BusId) when is_record(BusId, bus_id) ->
    gen_server:call(?SERVER, {get_bus, BusId}).
%%     case R of
%% 	{ok, Pid} ->
%% 	    link(Pid);
%% 	_ ->
%% 	    ignore
%%     end,
%%     R.
	    
export_service(Service, ServiceName) ->
    gen_server:call(?SERVER, {export_service, Service, ServiceName}).

unexport_service(Service, ServiceName) ->
    gen_server:call(?SERVER, {unexport_service, Service, ServiceName}).

set_service_reg(ServiceReg) ->
    gen_server:cast(?SERVER, {set_service_reg, ServiceReg}).

cast(Header) ->
    gen_server:cast(?SERVER, {cast, Header}).
    

%%
%% gen_server callbacks
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({get_bus, BusId}, _From, State) when is_record(BusId, bus_id) ->
    Busses = State#state.busses,
    case lists:keysearch(BusId, 1, Busses) of
	{value, {_, Bus}} ->
	    {reply, {ok, Bus}, State};
	false ->
	    {ok, Bus} = bus:connect(BusId),
	    Busses1 = [{BusId, Bus} | Busses],
	    {reply, {ok, Bus}, State#state{busses=Busses1}}
    end;

handle_call({export_service, _Service, ServiceName}, _From, State) ->
    Busses = State#state.busses,
    Fun = fun({_, Bus}) ->
		  io:format("export_service bus ~p~n", [Bus]),
		  ok = bus:export_service(Bus, ServiceName)
	  end,
    io:format("export_service name ~p~n", [ServiceName]),
    lists:foreach(Fun, Busses),
    {reply, ok, State};

handle_call({unexport_service, _Service, ServiceName}, _From, State) ->
    Busses = State#state.busses,
    Fun = fun({_, Bus}) ->
		  io:format("~p unexport_service bus ~p~n", [?MODULE, Bus]),
		  ok = bus:unexport_service(Bus, ServiceName)
	  end,
    io:format("~p unexport_service name ~p~n", [?MODULE, ServiceName]),
    lists:foreach(Fun, Busses),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast({set_service_reg, ServiceReg}, State) ->
    {noreply, State#state{service_reg=ServiceReg}};

handle_cast({cast, Header}, State) ->
    Fun = fun({_, Bus}) ->
		  bus:cast(Bus, Header)
	  end,
    lists:foreach(Fun, State#state.busses),
    
    {noreply, State};

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({bus_ready, Bus}, State) ->
    ServiceReg = State#state.service_reg,
    ServiceReg ! {new_bus, Bus},
    
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    Busses = State#state.busses,
    case lists:keysearch(Pid, 2, Busses) of
	{value, _} ->
	    Busses1 =
		lists:keydelete(Pid, 2, Busses),
		    {noreply, State#state{busses=Busses1}};
	false ->
	    if
		Reason /= normal ->
		    {stop, Reason};
		true ->
		    {noreply, State}
	    end
    end;

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
