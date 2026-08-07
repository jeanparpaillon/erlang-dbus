%% @copyright 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc API Introspection support module
%%
%% See <a href="https://dbus.freedesktop.org/doc/dbus-specification.html#introspection-format" >D-Bus specifications</a>
%% for introspection XML schema.
%%
%% @end
-module(dbus_introspect).

-include_lib("xmerl/include/xmerl.hrl").
-include("dbus.hrl").
-include("dbus_errors.hrl").

-export([
         find_interface/2,
         find_method/3,
         find_signal/3,
         to_xml/1,
         from_xml/1,
         from_xml_string/2
        ]).

-record(state, {node         :: dbus_node(),
                child        :: dbus_node(),
                iface        :: dbus_iface(),
                method       :: dbus_method(),
                signal       :: dbus_signal(),
                property     :: dbus_property(),
                s     = root :: atom()}).

%%%
%%% API
%%%
%% @doc Find an interface definition.
%%
%% @end
-spec find_interface(dbus_node(), Iface :: dbus_name()) ->
                            {ok, dbus_iface()} | {error, dbus_err()}.
find_interface(#dbus_node{interfaces=Ifaces}, IfaceName) ->
    case find_name(IfaceName, Ifaces, fun dbus_names:bin_to_iface/1) of
        {value, #dbus_iface{}=I} ->
            {ok, I};
        none ->
            {error, {'org.freedesktop.DBus.UnknownInterface', [IfaceName]}}
    end.


%% @doc Find a method definition.
%% @end
-spec find_method(dbus_node(), Iface :: dbus_name(), Method :: dbus_name()) ->
                         {ok, dbus_method()} | {error, dbus_err()}.
find_method(#dbus_node{interfaces=Ifaces}=Node, IfaceName, MethodName) ->
    case find_name(IfaceName, Ifaces, fun dbus_names:bin_to_iface/1) of
        {value, #dbus_iface{methods=Methods}=_I} ->
            case find_name(MethodName, Methods, fun dbus_names:bin_to_method/1) of
                {value, #dbus_method{}=Method} ->
                    {ok, Method};
                none ->
                    {error, {'org.freedesktop.DBus.UnknownMethod', {[MethodName], IfaceName, Node}}}
            end;
        none ->
            {error, {'org.freedesktop.DBus.UnknownInterface', [IfaceName]}}
    end.


%% @doc Find a signal definition
%% @end
-spec find_signal(dbus_node(), Iface :: dbus_name(), Signal :: dbus_name()) ->
                         {ok, dbus_signal()} | {error, dbus_err()}.
find_signal(#dbus_node{interfaces=Ifaces}=Node, IfaceName, SignalName) ->
    case find_name(IfaceName, Ifaces, fun dbus_names:bin_to_iface/1) of
        {value, #dbus_iface{signals=Signals}} ->
            case find_name(SignalName, Signals, fun dbus_names:bin_to_signal/1) of
                {value, #dbus_signal{}=Signal} ->
                    {ok, Signal};
                none ->
                    {error, {'org.freedesktop.DBus.UnknownSignal', {[SignalName], IfaceName, Node}}}
            end;
        none ->
            {error, {'org.freedesktop.DBus.UnknownInterface', [IfaceName]}}
    end.


%% @doc Export a `dbus_node()' into an XML introspection document.
%% @end
-spec to_xml(dbus_node()) -> list().
to_xml(#dbus_node{}=Node) ->
    Prolog = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">",
    lists:flatten(xmerl:export_simple([to_xmerl(Node)], xmerl_xml, [{prolog, Prolog}])).


%% @doc Parse a `dbus_node() ' from an XML string.
%%
%% @throws {error, parse_error}
%% @end
-spec from_xml_string(binary(), binary()) -> dbus_node().
from_xml_string(Data, RootPath) when is_binary(Data) ->
    Opts = [{event_fun, fun xml_event/3},
            {event_state, #state{}},
            skip_external_dtd],
    case xmerl_sax_parser:stream(Data, Opts) of
        {ok, #dbus_node{}=Node, _Rest} ->
            case Node#dbus_node.name of
                undefined ->
                    Node#dbus_node{name=RootPath};
                _ ->
                    Node
            end;
        {_Tag, _Location, Reason, _EndTags, _State} ->
            ?error("Error parsing introspection: ~p~n", [Reason]),
            throw({error, parse_error})
    end.


%% @doc Parse a `dbus_node()' from a filename.
%%
%% @throws {error, parse_error}
%% @end
-spec from_xml(file:filename()) -> dbus_node().
from_xml(Filename) ->
    Opts = [{event_fun, fun xml_event/3},
            {event_state, #state{}},
            skip_external_dtd],
    case xmerl_sax_parser:file(Filename, Opts) of
        {ok, #dbus_node{}=Node, _Rest} ->
            Node;
        {_Tag, _Location, Reason, _EndTags, _State} ->
            ?error("Error parsing introspection: ~p~n", [Reason]),
            throw({error, parse_error})
    end.

%%%
%%% Priv
%%%
find_name(Name, Tree, _) when is_atom(Name) ->
    case gb_trees:lookup(Name, Tree) of
        {value, Iface} -> {value, Iface};
        none -> gb_trees:lookup(atom_to_binary(Name, utf8), Tree)
    end;
find_name(Name, Tree, ToKnownFun) when is_binary(Name) ->
    case gb_trees:lookup(Name, Tree) of
        {value, Iface} -> {value, Iface};
        none ->
            case ToKnownFun(Name) of
                Bin when is_binary(Bin) -> none;
                Atom when is_atom(Atom) -> gb_trees:lookup(Atom, Tree)
            end
    end.


to_binary(Item) when is_atom(Item) ->
    atom_to_binary(Item, utf8);
to_binary(Item) when is_list(Item) ->
    list_to_binary(Item);
to_binary(Item) when is_binary(Item) ->
    Item.

to_xmerl(undefined) ->
    [];
to_xmerl(List) when is_list(List) ->
    lists:map(fun(Elem) -> to_xmerl(Elem) end, List);

to_xmerl(#dbus_node{}=Elem) ->
    {node,
     case to_binary(Elem#dbus_node.name) of
         undefined ->
             [];
         Name ->
             [{name, Name}]
     end,
     to_xmerl(Elem#dbus_node.elements) ++
         to_xmerl(Elem#dbus_node.interfaces)
    };

to_xmerl(#dbus_iface{}=Elem) ->
    {interface,
     [{name, to_binary(Elem#dbus_iface.name)}],
     to_xmerl(Elem#dbus_iface.methods) ++
         to_xmerl(Elem#dbus_iface.signals) ++
         to_xmerl(Elem#dbus_iface.properties)};

to_xmerl(#dbus_method{}=Elem) ->
    Result =
        case Elem#dbus_method.result of
            none ->
                [];
            undefined ->
                [];
            Arg ->
                [to_xmerl(Arg)]
        end,
    {method,
     case to_binary(Elem#dbus_method.name) of
         undefined ->
             [];
         Name ->
             [{name, Name}]
     end,
     to_xmerl(Elem#dbus_method.args) ++ Result};

to_xmerl(#dbus_signal{}=Elem) ->
    Result =
        case Elem#dbus_signal.result of
            none ->
                [];
            undefined ->
                [];
            Arg ->
                [to_xmerl(Arg)]
        end,
    {signal,
     case list_to_binary(Elem#dbus_signal.name) of
         undefined ->
             [];
         Name ->
             [{name, Name}]
     end,
     to_xmerl(Elem#dbus_signal.args) ++ Result};

to_xmerl(#dbus_arg{}=Elem) ->
    {arg,
     case to_binary(Elem#dbus_arg.name) of
         undefined ->
             [];
         Name ->
             [{name, Name}]
     end ++
         [{direction, Elem#dbus_arg.direction}, {type, Elem#dbus_arg.type}],  []}.

xml_event(startDocument, _L, S) ->
    S;

xml_event(endDocument, _L, #state{node=Node}) ->
    Node;

xml_event({startPrefixMapping, _Prefix, _Uri}, _L, S) ->
    S;

xml_event({endPrefixMapping, _Prefix}, _L, S) ->
    S;

xml_event({startElement, _, "node", _, Attrs}, _L, #state{s=root}=S) ->
    S#state{s=node, node=build_root_node(Attrs, #dbus_node{interfaces=gb_trees:empty()})};

xml_event({startElement, _, "node", _, Attrs}, _L, #state{s=node}=S) ->
    S#state{s=child_node, child=build_node(Attrs, #dbus_node{})};

xml_event({startElement, _, "interface", _, Attrs}, _L, #state{s=node}=S) ->
    S#state{s=iface, iface=build_iface(Attrs,
                                       #dbus_iface{methods=gb_trees:empty(),
                                                   signals=gb_trees:empty(),
                                                   properties=gb_trees:empty()})};

xml_event({startElement, _, "method", _, Attrs}, _L, #state{s=iface}=S) ->
    S#state{s=method, method=build_method(Attrs, #dbus_method{})};

xml_event({startElement, _, "signal", _, Attrs}, _L, #state{s=iface}=S) ->
    S#state{s=signal, signal=build_signal(Attrs, #dbus_signal{})};

xml_event({startElement, _, "property", _, Attrs}, _L, #state{s=iface}=S) ->
    S#state{s=property, property=build_property(Attrs, #dbus_property{})};

xml_event({startElement, _, "arg", _, Attrs}, _L, #state{s=method, method=#dbus_method{args=Args}=Method}=S) ->
    Arg = build_arg(Attrs, #dbus_arg{}),
    S#state{s=method_arg, method=Method#dbus_method{args=[Arg | Args]}};

xml_event({startElement, _, "arg", _, Attrs}, _L, #state{s=signal, signal=#dbus_signal{args=Args}=Signal}=S) ->
    Arg = build_arg(Attrs, #dbus_arg{}),
    S#state{s=signal_arg, signal=Signal#dbus_signal{args=[Arg | Args]}};

xml_event({startElement, _, "annotation", _, Attrs}, _L, #state{s=method, method=#dbus_method{annotations=Annotations}=M}=S) ->
    S#state{s=method_annotation,
            method=M#dbus_method{annotations=[ build_annotation(Attrs, {undefined, undefined}) | Annotations]}};

xml_event({startElement, _, "annotation", _, Attrs}, _L, #state{s=iface, iface=#dbus_iface{annotations=Annotations}=I}=S) ->
    S#state{s=iface_annotation,
            iface=I#dbus_iface{annotations=[ build_annotation(Attrs, {undefined, undefined}) | Annotations]}};

xml_event({startElement, _, "annotation", _, Attrs}, _L, #state{s=property, property=#dbus_property{annotations=Annotations}=P}=S) ->
    S#state{s=property_annotation,
            property=P#dbus_property{annotations=[ build_annotation(Attrs, {undefined, undefined}) | Annotations]}};

xml_event({startElement, _, "annotation", _, Attrs}, _L, #state{s=signal, signal=#dbus_signal{annotations=Annotations}=Sig}=S) ->
    S#state{s=signal_annotation,
            signal=Sig#dbus_signal{annotations=[ build_annotation(Attrs, {undefined, undefined}) | Annotations]}};

xml_event({endElement, _, "node", _}, _L, #state{s=node}=S) ->
    S#state{s=root};

xml_event({endElement, _, "node", _}, _L, #state{s=child_node, node=#dbus_node{elements=E}=N, child=C}=S) ->
    S#state{s=node, node=N#dbus_node{elements=[C | E]}, child=undefined};

xml_event({endElement, _, "interface", _}, _L, #state{s=iface, node=#dbus_node{interfaces=Ifaces}=N, iface=I}=S) ->
    NewNode = N#dbus_node{interfaces=gb_trees:enter(I#dbus_iface.name, I, Ifaces)},
    S#state{s=node, node=NewNode, iface=undefined, method=undefined};

xml_event({endElement, _, "method", _}, _L, #state{s=method, iface=#dbus_iface{methods=Methods}=I, method=M}=S) ->
    NewIface = I#dbus_iface{methods=gb_trees:enter(M#dbus_method.name, end_method(M), Methods)},
    S#state{s=iface, iface=NewIface, method=undefined};

xml_event({endElement, _, "signal", _}, _L, #state{s=signal, iface=#dbus_iface{signals=Signals}=I, signal=Sig}=S) ->
    NewIface = I#dbus_iface{signals=gb_trees:enter(Sig#dbus_signal.name, end_signal(Sig), Signals)},
    S#state{s=iface, iface=NewIface, signal=undefined};

xml_event({endElement, _, "property", _}, _L, #state{s=property, iface=#dbus_iface{properties=Props}=I, property=P}=S) ->
    NewIface = I#dbus_iface{properties=gb_trees:enter(P#dbus_property.name, P, Props)},
    S#state{s=iface, iface=NewIface, property=undefined};

xml_event({endElement, _, "arg", _}, _L, #state{s=method_arg}=S) ->
    S#state{s=method};

xml_event({endElement, _, "arg", _}, _L, #state{s=signal_arg}=S) ->
    S#state{s=signal};

xml_event({endElement, _, "annotation", _}, _L, #state{s=method_annotation}=S) ->
    S#state{s=method};

xml_event({endElement, _, "annotation", _}, _L, #state{s=iface_annotation}=S) ->
    S#state{s=iface};

xml_event({endElement, _, "annotation", _}, _L, #state{s=property_annotation}=S) ->
    S#state{s=property};

xml_event({endElement, _, "annotation", _}, _L, #state{s=signal_annotation}=S) ->
    S#state{s=signal};

xml_event({characters, _Str}, _L, S) ->
    S;

xml_event({ignorableWhitespace, _Str}, _L, S) ->
    S;

xml_event({processingInstruction, _Target, _Data}, _L, S) ->
    S;

xml_event({comment, _Str}, _L, S) ->
    S;

xml_event(startCDATA, _L, S) ->
    S;

xml_event(endCDATA, _L, S) ->
    S;

xml_event({startDTD, "node", "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN", _SysID}, _L, S) ->
    S;

xml_event({startDTD, _Name, PublicID, _SysID}, {_, _, L}, _S) ->
    throw({error, io_lib:format("(line ~p) Invalid DTD: ~p", [L, PublicID])});

xml_event(endDTD, _L, S) ->
    S;

xml_event({startEntity, _SysID}, _L, S) ->
    S;

xml_event({endEntity, _SysID}, _L, S) ->
    S;

xml_event({elementDecl, _Name, _Model}, _L, S) ->
    S;

xml_event({attributeDecl, _ElementName, _AttributeName, _Type, _Mode, _Value}, _L, S) ->
    S;

xml_event({internalEntityDecl, _Name, _Value}, _L, S) ->
    S;

xml_event({externalEntityDecl, _Name, _PublicId, _SystemId}, _L, S) ->
    S;

xml_event({unparsedEntityDecl, _Name, _PublicId, _SystemId, _Ndata}, _L, S) ->
    S;

xml_event({notationDecl, _Name, _PublicId, _SystemId}, _L, S) ->
    S;

xml_event(E, {_,_,L}, S) ->
    throw({error, io_lib:format("(line ~p) Unhandled event ~p (state=~p)", [L, E, S])}).


build_root_node([], Node) ->
    Node;
build_root_node([{_, _, "name", Path} | Attrs], Node) ->
    build_root_node(Attrs, Node#dbus_node{name=list_to_binary(Path)});
build_root_node([{_, _, Attr, _}, _], _) ->
    throw({error, {invalid_node_attribute, Attr}}).


%% TODO: check child node path: must be relative
build_node([], Node) ->
    Node;
build_node([{_, _, "name", Path} | Attrs], Node) ->
    build_node(Attrs, Node#dbus_node{name=list_to_binary(Path)});
build_node([{_, _, Attr, _}, _], _) ->
    throw({error, {invalid_node_attribute, Attr}}).


build_iface([], Iface) ->
    Iface;
build_iface([{_, _, "name", Name} | Attrs], Iface) ->
    build_iface(Attrs, Iface#dbus_iface{name=dbus_names:list_to_iface(Name)});
build_iface([{_, _, Attr, _}, _], _) ->
    throw({error, {invalid_interface_attribute, Attr}}).


build_method([], Method) ->
    Method;
build_method([{_, _, "name", Name} | Attrs], Method) ->
    build_method(Attrs, Method#dbus_method{name=dbus_names:list_to_method(Name)});
build_method([{_, _, Attr, _}, _], _) ->
    throw({error, {invalid_method_attribute, Attr}}).


end_method(#dbus_method{args=Args}=Method) ->
    InArgs = lists:filter(fun (#dbus_arg{direction=in}) ->
                                  true;
                              (#dbus_arg{direction=undefined}) ->
                                  true;
                              (_) ->
                                  false
                          end, Args),
    Signature = lists:foldl(fun (#dbus_arg{type=Type}, Acc) ->
                                    << Type/binary, Acc/binary >>
                            end, <<>>, InArgs),
    case dbus_marshaller:unmarshal_signature(Signature) of
        {ok, Types} ->
            Method#dbus_method{args=lists:reverse(Args), in_sig=Signature, in_types=Types};
        more ->
            throw({error, incomplete_signature})
    end.


%% TODO: Deal with omission of direction attribute
build_arg([], Arg) ->
    Arg;
build_arg([{_, _, "name", Name} | Attrs], Arg) ->
    build_arg(Attrs, Arg#dbus_arg{name=list_to_binary(Name)});
build_arg([{_, _, "type", Type} | Attrs], Arg) ->
    build_arg(Attrs, Arg#dbus_arg{type=list_to_binary(Type)});
build_arg([{_, _, "direction", "in"} | Attrs], Arg) ->
    build_arg(Attrs, Arg#dbus_arg{direction=in});
build_arg([{_, _, "direction", "out"} | Attrs], Arg) ->
    build_arg(Attrs, Arg#dbus_arg{direction=out});
build_arg([{_, _, Attr, _}, _], _) ->
    throw({error, {invalid_arg_attribute, Attr}}).


build_signal([], Signal) ->
    Signal;
build_signal([{_, _, "name", Name} | Attrs], Signal) ->
    build_signal(Attrs, Signal#dbus_signal{name=dbus_names:list_to_signal(Name)});
build_signal([{_, _, Attr, _} | _], _) ->
    throw({error, {invalid_signal_attribute, Attr}}).


end_signal(#dbus_signal{args=Args}=Signal) ->
    OutArgs = lists:filter(fun (#dbus_arg{direction=out}) ->
                                   true;
                               (#dbus_arg{direction=undefined}) ->
                                   true;
                               (_) ->
                                   false
                           end, Args),
    Signature = lists:foldl(fun (#dbus_arg{type=Type}, Acc) ->
                                    << Type/binary, Acc/binary >>
                            end, <<>>, OutArgs),
    case dbus_marshaller:unmarshal_signature(Signature) of
        {ok, Types} ->
            Signal#dbus_signal{args=lists:reverse(Args), out_sig=Signature, out_types=Types};
        more ->
            throw({error, incomplete_signature})
    end.


build_property([], Prop) ->
    Prop;
build_property([{_, _, "name", Name} | Attrs], Prop) ->
    build_property(Attrs, Prop#dbus_property{name=list_to_binary(Name)});
build_property([{_, _, "type", Type} | Attrs], Prop) ->
    build_property(Attrs, Prop#dbus_property{type=list_to_binary(Type)});
build_property([{_, _, "access", "read"} | Attrs], Prop) ->
    build_property(Attrs, Prop#dbus_property{access=read});
build_property([{_, _, "access", "write"} | Attrs], Prop) ->
    build_property(Attrs, Prop#dbus_property{access=write});
build_property([{_, _, "access", "readwrite"} | Attrs], Prop) ->
    build_property(Attrs, Prop#dbus_property{access=readwrite});
build_property([{_, _, Attr, _} | _], _) ->
    throw({error, {invalid_signal_attribute, Attr}}).


build_annotation([], Acc) ->
    Acc;
build_annotation([{_, _, "name", "org.freedesktop.DBus.Deprecated"} | Attrs], {undefined, V}) ->
    build_annotation(Attrs, {'org.freedesktop.DBus.Deprecated', V});
build_annotation([{_, _, "name", "org.freedesktop.DBus.GLib.CSymbol"} | Attrs], {undefined, V}) ->
    build_annotation(Attrs, {'org.freedesktop.DBus.GLib.CSymbol', V});
build_annotation([{_, _, "name", "org.freedesktop.DBus.Method.NoReply"} | Attrs], {undefined, V}) ->
    build_annotation(Attrs, {'org.freedesktop.DBus.Method.NoReply', V});
build_annotation([{_, _, "name", "org.freedesktop.DBus.Property.EmitsChangedSignal"} | Attrs], {undefined, V}) ->
    build_annotation(Attrs, {'org.freedesktop.DBus.Property.EmitsChangedSignal', V});
build_annotation([{_, _, "name", Name} | Attrs], {undefined, V}) ->
    build_annotation(Attrs, {list_to_binary(Name), V});
build_annotation([{_, _, "value", "true"} | Attrs], {N, undefined}) ->
    build_annotation(Attrs, {N, true});
build_annotation([{_, _, "value", "false"} | Attrs], {N, undefined}) ->
    build_annotation(Attrs, {N, false});
build_annotation([{_, _, "value", "invalidates"} | Attrs], {N, undefined}) ->
    build_annotation(Attrs, {N, invalidates});
build_annotation([{_, _, "value", V} | Attrs], {N, undefined}) ->
    build_annotation(Attrs, {N, list_to_binary(V)});
build_annotation([{_, _, Attr, _} | _], _) ->
    throw({error, {invalid_annotation_attribute, Attr}}).
