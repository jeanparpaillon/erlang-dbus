-module(dberl.introspect).

-import(lists).
-import(xmerl_scan).

-include("xmerl.hrl").
-include("dbus.hrl").

-export([
	 build_introspect/2,
	 from_xml/1,
	 from_xml_string/1
	]).

%% api


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
