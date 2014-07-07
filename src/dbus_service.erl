%%
%% @copyright 2006-2007 Mikael Magnusson
%% @copyright 2014 Jean Parpaillon <jean.parpaillon@free.fr>
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr.
%%
%% @doc Exported D-BUS service gen_server
%%
-module(dbus_service).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/1,
	 register_object/3,
	 unregister_object/2
	]).

%% gen_server callback2
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-record(state, {
	  name,
	  xml_body,
	  objects=[]
	 }).

start_link(ServiceName) ->
    gen_server:start_link(?MODULE, [ServiceName], []).

register_object(Service, Path, Object) ->
    gen_server:call(Service, {register_object, Path, Object}).

unregister_object(Service, Object) ->
    gen_server:call(Service, {unregister_object, Object}).

%%
%% gen_server callbacks
%%
init([ServiceName]) ->
    %%process_flag(trap_exit, true),
    State = #state{name=ServiceName},
    self() ! setup,
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({register_object, Path, Object}, _From, State) ->
    Objects = State#state.objects,
    case lists:keysearch(Path, 1, Objects) of
	{value, _} ->
	    {reply, {already_registered, Path}, State};
	false ->
	    true = link(Object),
	    Objects1 = [{Path, Object} | Objects],
	    {reply, ok, State#state{objects=Objects1}}
    end;

handle_call({unregister_object, Object}, _From, State) ->
    case handle_unregister_object(Object, State) of
	{ok, State1} ->
	    {reply, ok, State1};
	{error, Reason, State1} ->
	    {reply, Reason, State1};
	{stop, State1} ->
	    {stop, normal, State1}
    end;

handle_call(Request, _From, State) ->
    lager:debug("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    lager:debug("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(setup, State) ->
    {noreply, State};

handle_info({dbus_method_call, Msg, Conn}, State) ->
    Path = dbus_message:get_field_value(?HEADER_PATH, Msg),
    handle_method_call(Path, Msg, Conn, State);

handle_info({'EXIT', Pid, Reason}, State) ->
    case handle_unregister_object(Pid, State) of
	{ok, State1} ->
	    {noreply, State1};
	{stop, State1} ->
	    {stop, normal, State1};
	{error, not_registered, State1} ->
	    if
		Reason /= normal ->
		    {stop, Reason, State1};
		true ->
		    {noreply, State1}
	    end
    end;

handle_info(Info, State) ->
    lager:debug("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.

%%
%% Return {ok, State}|{error, Reason, State}|{stop, State}
%%
handle_unregister_object(Object, State) ->
    Objects = State#state.objects,
    case lists:keysearch(Object, 2, Objects) of
	{value, {Path, _}} ->
	    true = unlink(Object),
	    lager:debug("~p: Object terminated ~p ~p~n", [?MODULE, Object, Path]),
	    Objects1 = lists:keydelete(Object, 2, Objects),
	    if
		Objects1 == [] ->
		    lager:debug("~p: No more objects stopping ~p service~n", [?MODULE, State#state.name]),
		    {stop, State};
		true ->
		    {ok, State#state{objects=Objects1}}
	    end;
	false ->
	    {error, not_registered, State}
    end.


handle_method_call(<<"/">>, #dbus_message{}=Msg, Conn,
		   #state{objects=Objects}=State) ->
    Member = dbus_message:get_field_value(?HEADER_MEMBER, Msg),
    case dbus_constants:to_atom(Member) of
	'Introspect' ->
	    Elements = lists:foldl(fun({Path, _}, Res) ->
					   [$/ | PathStr] = atom_to_list(Path),
					   [#dbus_node{name=PathStr} | Res]
				   end, [], Objects),
	    Node = #dbus_node{name="/", elements=Elements},
	    ReplyBody = dbus_introspect:to_xml(Node),
	    lager:debug("Introspect ~p~n", [ReplyBody]),
	    {ok, Reply} = dbus_message:return(Msg, [string], [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {noreply, State};
	_ ->
	    ErrorName = 'org.freedesktop.DBus.Error.UnknownMethod',
	    ErrorText = <<"Erlang: Function not found: ", Member/binary>>,
	    Reply = dbus_message:error(Msg, ErrorName, ErrorText),
	    ok = dbus_connection:cast(Conn, Reply),
	    {noreply, State}
    end;

handle_method_call(Path, #dbus_message{}=Msg, Conn, #state{objects=Objects}=State) 
  when is_binary(Path) ->
    case proplists:get_value(Path, Objects) of
	undefined ->
	    ErrorName = 'org.freedesktop.DBus.Error.UnknownObject',
	    ErrorText = <<"Erlang: Object not found: ", Path/binary>>,
	    Reply = dbus_message:error(Msg, ErrorName, ErrorText),
	    lager:debug("Reply ~p~n", [Reply]),
	    ok = dbus_connection:cast(Conn, Reply);
	Object ->
	    Object ! {dbus_method_call, Msg, Conn}
    end,
    {noreply, State}.
