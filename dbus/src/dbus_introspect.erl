%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc introspect support module
%%

-module(dberl.introspect).

-import(io).
-import(lists).
-import(xmerl).
-import(xmerl_scan).

-include("xmerl.hrl").
-include("dbus.hrl").

-export([
	 build_introspect/2,
	 from_xml/1,
	 from_xml_string/1,
	 to_xmerl/1,
	 fetch_interface/2,
	 find_interface/2,
	 fetch_method/2,
	 find_method/2,
	 fetch_signal/2,
	 find_signal/2,
	 test/0
	]).

%% api

to_xmerl(List) when is_list(List) ->
    lists:map(fun(Elem) -> to_xmerl(Elem) end, List);

to_xmerl(Elem) when is_record(Elem, node) ->
    io:format("node~n"),
    {node,
     case Elem#node.name of
	 undefined ->
	     [];
	 Name ->
	     [{name, Name}]
     end,
     to_xmerl(Elem#node.elements) ++
     to_xmerl(Elem#node.interfaces)
    };

to_xmerl(Elem) when is_record(Elem, interface) ->
    io:format("interface ~p~n", [Elem]),
    {interface,
     [{name, Elem#interface.name}],
     to_xmerl(Elem#interface.methods) ++
     to_xmerl(Elem#interface.signals) ++
     to_xmerl(Elem#interface.properties)};

to_xmerl(Elem) when is_record(Elem, method) ->
    io:format("method ~p~n", [Elem]),
    Result =
	case Elem#method.result of
	    none ->
		[];
	    undefined ->
		[];
	    Arg ->
		[to_xmerl(Arg)]
	end,
%%     Fun = fun(Arg) ->
%% 		  {arg, [{direction, Arg#

    {method,
     case Elem#method.name of
	 undefined ->
	     [];
	 Name ->
	     [{name, Name}]
     end,
     to_xmerl(Elem#method.args) ++ 
     Result};

to_xmerl(Elem) when is_record(Elem, arg) ->
    io:format("arg ~p~n", [Elem]),
    {arg, 
     case Elem#arg.name of
	 undefined ->
	     [];
	 Name ->
	     [{name, Name}]
     end ++
     [{direction, Elem#arg.direction},
      {type, Elem#arg.type}], []}.

build_introspect(Service, Path) ->
    Headers = [
	       {?HEADER_PATH, #variant{type=object_path, value=Path}},
	       {?HEADER_DESTINATION, #variant{type=string, value=Service}},
	       {?HEADER_INTERFACE, #variant{type=string, value="org.freedesktop.DBus.Introspectable"}},
	       {?HEADER_MEMBER, #variant{type=string, value="Introspect"}}
	      ],

    #header{type=?TYPE_METHOD_CALL,
	    headers=Headers}.



from_xml_string(Data) when is_list(Data) ->
    {Xml, _Misc} = xmerl_scan:string(Data),
    xml(Xml).

from_xml(FileName) ->
    {Xml, _Misc} = xmerl_scan:file(FileName, []),
    xml(Xml).

xml(Content) when is_list(Content) ->
    xml(Content, []);

xml(#xmlElement{name=node}=Element) ->
    Name =
	case find_attribute(name, Element) of
	    {ok, Attrib} -> Attrib#xmlAttribute.value;
	    error -> undefined
	end,
    Content = xml(Element#xmlElement.content),
    Interfaces = filter_content(interface, Content),

    %% FIXME subnodes
    #node{name=Name,
%% 	  elements=Content,
	  interfaces=Interfaces};

xml(#xmlElement{name=interface}=Element) ->
    Name = fetch_attribute(name, Element),
    Content = xml(Element#xmlElement.content),
    Methods = filter_content(method, Content),
    Signals = filter_content(signal, Content),
    Properties = filter_content(property, Content),

    #interface{name=list_to_atom(Name#xmlAttribute.value),
	       methods=Methods,
	       signals=Signals,
%% 	       signals=Content,
	       properties=Properties};

xml(#xmlElement{name=method}=Element) ->
    Name = fetch_attribute(name, Element),
    Content = xml(Element#xmlElement.content),
    Args = filter_content(arg, Content),
    CmpDir = fun(A) ->
		  case A of
		      #arg{direction=in} -> true;
		      #arg{direction=undefined} -> true;
		      _ -> false
		  end
	  end,
		  
    OutArgs = lists:filter(CmpDir, Args),

    BuildSig = fun(Arg, Acc) ->
		       Arg#arg.type ++ Acc
	       end,

    Signature = lists:foldr(BuildSig, "", OutArgs),
    Types = marshaller:unmarshal_signature(Signature),

    #method{name=list_to_atom(Name#xmlAttribute.value),
	    args=Args,
	    in_sig=Signature, 
	    in_types=Types};

xml(#xmlElement{name=signal}=Element) ->
    Name = fetch_attribute(name, Element),
    Content = xml(Element#xmlElement.content),
    Args = filter_content(arg, Content),
    CmpDir = fun(A) ->
		  case A of
		      #arg{direction=out} -> true;
		      #arg{direction=undefined} -> true
		  end
	  end,
		  
    OutArgs = lists:filter(CmpDir, Args),

    BuildSig = fun(Arg, Acc) ->
		       Arg#arg.type ++ Acc
	       end,

    Signature = lists:foldr(BuildSig, "", OutArgs),
    Types = marshaller:unmarshal_signature(Signature),

    #signal{name=list_to_atom(Name#xmlAttribute.value),
	    args=Args,
	    out_sig=Signature, 
	    out_types=Types};

xml(#xmlElement{name=arg}=Element) ->
    Name =
	case find_attribute(name, Element) of
	    {ok, Attribute} ->
		list_to_atom(Attribute#xmlAttribute.value);
	    error ->
		none
	end,

    Direction =
	    case find_attribute(direction, Element) of
		{ok, #xmlAttribute{value="in"}} ->
		    in;
		{ok, #xmlAttribute{value="out"}} ->
		    out;
		error ->
		    undefined
	    end,

    Type = fetch_attribute(type, Element),

    #arg{name=Name, direction=Direction, type=Type#xmlAttribute.value};

xml(Element) when is_record(Element, xmlText) ->
    ignore;
xml(Element) ->
    throw({badarg, Element}).

xml([], Res) ->
    Res;

xml([Item | R], Res) ->
    xml(R, Res ++ [xml(Item)]).

fetch_attribute(Name, Element) ->
    case find_attribute(Name, Element) of
	{ok, Attribute} ->
	    Attribute;
	error ->
	    exit({error, {not_found, Name, Element}})
    end.

find_attribute(Name, Element) ->
    Fun = fun(E) ->
		  case E of
		      #xmlAttribute{name=Name} ->
			  true;
		      _ ->
			  false
		  end
	  end,
			  
    case lists:filter(Fun, Element#xmlElement.attributes) of
	[Attribute] ->
	    {ok, Attribute};
	[_Attribute|_] ->
	    error;
	[] ->
	    error
    end.

filter_content(Name, Content) ->
    Fun = fun(E) ->
		  is_record(E, Name)
	  end,
    lists:filter(Fun, Content).

fetch_interface(IfaceName, Node) when is_record(Node, node) ->
    {ok, Iface} = find_interface(IfaceName, Node),
    Iface.

find_interface(IfaceName, Node) when is_record(Node, node) ->
    Fun = fun(E) ->
		  case E of
		      #interface{name=IfaceName} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Node#node.interfaces) of
	[Iface|_] ->
	    {ok, Iface};
	[] ->
	    error
    end.

fetch_method(MethodName, Node) when is_record(Node, interface) ->
    {ok, Method} = find_method(MethodName, Node),
    Method.

find_method(MethodName, Iface) when is_record(Iface, interface) ->
    Fun = fun(E) ->
		  case E of
		      #method{name=MethodName} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Iface#interface.methods) of
	[Method|_] ->
	    {ok, Method};
	[] ->
	    error
    end.

fetch_signal(Signal_name, Node) when is_record(Node, interface) ->
    {ok, Signal} = find_signal(Signal_name, Node),
    Signal.

find_signal(Signal_name, Iface) when is_record(Iface, interface) ->
    Fun = fun(E) ->
		  case E of
		      #signal{name=Signal_name} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Iface#interface.signals) of
	[Signal|_] ->
	    {ok, Signal};
	[] ->
	    error
    end.

%% Tests
test() ->
    Data = default_dbus_node(),
    Data1 = Data,
    io:format("Data ~p~n", [Data1]),
    lists:flatten(xmerl:export_simple([to_xmerl(Data1)], xmerl_xml)).

default_dbus_node() ->
    HelloMethod = #method{name='Hello', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    AddMatch = #method{name='AddMatch', args=[#arg{direction=in, type="s"}], in_sig="s", in_types=[string]},
    RequestName = #method{name='RequestName', args=[#arg{direction=in, type="s"}, #arg{direction=in, type="u"}, #arg{direction=out, type="u"}], in_sig="su", in_types=[string,uint32]},
    ReleaseName = #method{name='ReleaseName', args=[#arg{direction=in, type="s"}, #arg{direction=out, type="u"}], in_sig="s", in_types=[string]},
    DBusIface = #interface{name='org.freedesktop.DBus', methods=[HelloMethod, AddMatch, RequestName, ReleaseName]},

    IntrospectMethod = #method{name='Introspect', args=[], result=#arg{direction=out, type="s"}, in_sig="", in_types=[]},
    DBusIntrospectableIface = #interface{name='org.freedesktop.DBus.Introspectable', methods=[IntrospectMethod]},

    DBusRootNode = #node{elements=[], interfaces=[DBusIface, DBusIntrospectableIface]},
    DBusRootNode.

