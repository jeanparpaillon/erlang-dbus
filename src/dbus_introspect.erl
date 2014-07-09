%%
%% @copyright 2006-2007 Mikael Magnusson
%% @Copyright 2014 Jean Parpaillon
%%
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr.
%% @doc introspect support module
%%
-module(dbus_introspect).

-include_lib("xmerl/include/xmerl.hrl").
-include("dbus.hrl").

-export([
	 to_xml/1,
	 from_xml/1,
	 from_xml_string/1,
	 to_xmerl/1,
	 fetch_interface/2,
	 find_interface/2,
	 fetch_method/2,
	 find_method/2,
	 fetch_signal/2,
	 find_signal/2
	]).

%%%
%%% API
%%%
to_xml(#dbus_node{}=Node) ->
    Prolog = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">",
    lists:flatten(xmerl:export_simple([to_xmerl(Node)], xmerl_xml, [{prolog, Prolog}])).

to_xmerl(undefined) ->
    [];
to_xmerl(List) when is_list(List) ->
    lists:map(fun(Elem) -> to_xmerl(Elem) end, List);

to_xmerl(#dbus_node{}=Elem) ->
    {node,
     case Elem#dbus_node.name of
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
     [{name, Elem#dbus_iface.name}],
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
     case Elem#dbus_method.name of
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
     case Elem#dbus_signal.name of
	 undefined ->
	     [];
	 Name ->
	     [{name, Name}]
     end,
     to_xmerl(Elem#dbus_signal.args) ++ Result};

to_xmerl(#dbus_arg{}=Elem) ->
    {arg, 
     case Elem#dbus_arg.name of
	 undefined ->
	     [];
	 Name ->
	     [{name, Name}]
     end ++
     [{direction, Elem#dbus_arg.direction}, {type, Elem#dbus_arg.type}], []}.

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
    #dbus_node{name=Name,
	       %% 	  elements=Content,
	       interfaces=Interfaces};

xml(#xmlElement{name=interface}=Element) ->
    Name = fetch_attribute(name, Element),
    Content = xml(Element#xmlElement.content),
    Methods = filter_content(method, Content),
    Signals = filter_content(signal, Content),
    Properties = filter_content(property, Content),

    #dbus_iface{name=Name#xmlAttribute.value,
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
		      #dbus_arg{direction=in} -> true;
		      #dbus_arg{direction=undefined} -> true;
		      _ -> false
		  end
	  end,
    OutArgs = lists:filter(CmpDir, Args),
    BuildSig = fun(Arg, Acc) ->
		       Arg#dbus_arg.type ++ Acc
	       end,
    Signature = lists:foldr(BuildSig, "", OutArgs),
    Types = dbus_marshaller:unmarshal_signature(list_to_binary(Signature)),
    #dbus_method{name=Name#xmlAttribute.value,
		 args=Args,
		 in_sig=Signature, 
		 in_types=Types};

xml(#xmlElement{name=signal}=Element) ->
    Name = fetch_attribute(name, Element),
    Content = xml(Element#xmlElement.content),
    Args = filter_content(arg, Content),
    CmpDir = fun(A) ->
		  case A of
		      #dbus_arg{direction=out} -> true;
		      #dbus_arg{direction=undefined} -> true
		  end
	  end,
    OutArgs = lists:filter(CmpDir, Args),
    BuildSig = fun(Arg, Acc) ->
		       Arg#dbus_arg.type ++ Acc
	       end,
    Signature = lists:foldr(BuildSig, "", OutArgs),
    Types = dbus_marshaller:unmarshal_signature(list_to_binary(Signature)),
    #dbus_signal{name=Name#xmlAttribute.value,
		 args=Args,
		 out_sig=Signature, 
		 out_types=Types};

xml(#xmlElement{name=arg}=Element) ->
    Name =
	case find_attribute(name, Element) of
	    {ok, Attribute} ->
		Attribute#xmlAttribute.value;
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
    #dbus_arg{name=Name, direction=Direction, type=Type#xmlAttribute.value};

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

fetch_interface(IfaceName, #dbus_node{}=Node) ->
    {ok, Iface} = find_interface(IfaceName, Node),
    Iface.

find_interface(IfaceName, #dbus_node{}=Node) ->
    Fun = fun(E) ->
		  case E of
		      #dbus_iface{name=IfaceName} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Node#dbus_node.interfaces) of
	[Iface|_] ->
	    {ok, Iface};
	[] ->
	    error
    end.

fetch_method(MethodName, #dbus_iface{}=Node) ->
    {ok, Method} = find_method(MethodName, Node),
    Method.

find_method(MethodName, #dbus_iface{}=Iface) ->
    Fun = fun(E) ->
		  case E of
		      #dbus_method{name=MethodName} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Iface#dbus_iface.methods) of
	[Method|_] ->
	    {ok, Method};
	[] ->
	    error
    end.

fetch_signal(Signal_name, #dbus_iface{}=Node) ->
    {ok, Signal} = find_signal(Signal_name, Node),
    Signal.

find_signal(Signal_name, Iface) when is_atom(Signal_name) ->
    find_signal(atom_to_list(Signal_name), Iface);
find_signal(Signal_name, #dbus_iface{}=Iface) ->
    Fun = fun(E) ->
		  case E of
		      #dbus_signal{name=Signal_name} -> true;
		      _ -> false
		  end
	  end,
    case lists:filter(Fun, Iface#dbus_iface.signals) of
	[Signal|_] ->
	    {ok, Signal};
	[] ->
	    error
    end.

%%%
%%% Priv
%%%
