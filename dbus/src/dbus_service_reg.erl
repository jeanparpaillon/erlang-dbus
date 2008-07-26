%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc Register of exported D-BUS services gen_server
%%

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
    process_flag(trap_exit, true),
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
	    io:format("~p: export_service name ~p~n", [?MODULE, ServiceName]),
	    Service = State#state.service,
	    ok = dbus_bus_reg:export_service(undefined, ServiceName),
	    Services1 = [{ServiceName, Service}|Services],
	    {reply, {ok, Service}, State#state{services=Services1}}
    end;

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({dbus_method_call, Header, Conn}, State) ->
%%     {_, ServiceNameVar} = dbus_message:header_fetch(?HEADER_DESTINATION, Header),
%%     ServiceName = list_to_atom(ServiceNameVar#variant.value),
    Service = State#state.service,

%%     io:format("Handle call ~p ~p~n", [Header, ServiceName]),
%%     case lists:keysearch(ServiceName, 1, State#state.services) of
%% 	{value, {ServiceName, Service}} ->
    Service ! {dbus_method_call, Header, Conn},

%% 	_ ->
%% 	    ErrorName = "org.freedesktop.DBus.Error.ServiceUnknown",
%% 	    ErrorText = "Erlang: Service '" ++ [atom_to_list(ServiceName)] ++ "' not found.",
%% 	    {ok, Reply} = dbus_message:build_error(Header, ErrorName, ErrorText),
%% 	    io:format("Reply ~p~n", [Reply]),
%% 	    ok = dbus_connection:reply(Conn, Reply)
%%     end,
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
	    error_logger:info_msg("~p ~p Terminated ~p~n", [?MODULE, Pid, Reason]),
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
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.
