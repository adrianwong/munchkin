%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module dom.
:- interface.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module string.

:- type node
    --->    node(
                children  :: list(node),
                node_type :: node_type
            ).

:- type node_type
    --->    text(string)
    ;       element(element).

:- type element
    --->    element(
                tag_name   :: string,
                attributes :: attributes
            ).

:- type attributes == map(string, string).

:- func text(string) = node.

:- func element(string, attributes, list(node)) = node.

:- pred print(node::in, io::di, io::uo) is det.

:- pred print_nl(node::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module pair.

%------------------------------------------------------------------------------%

text(Data) =
    node([], text(Data)).

element(Name, Attrs, Children) = Node :-
    Elem = element(Name, Attrs),
    Node = node(Children, element(Elem)).

%------------------------------------------------------------------------------%

print_nl(Node, !IO) :-
    dom.print(Node, !IO),
    io.nl(!IO).

print(Node, !IO) :-
    Type = Node ^ node_type,
    (
        Type = text(Text),
        io.write_string(Text, !IO),
        list.foldl(dom.print, Node ^ children, !IO)
    ;
        Type = element(Elem),
        TagName = Elem ^ tag_name,
        io.write_string("<" ++ TagName, !IO),
        ( if map.is_empty(Elem ^ attributes) then
            io.write_string(">", !IO)
        else
            Attrs = stringify_attrs(Elem ^ attributes),
            io.write_string(format(" %s>", [s(Attrs)]), !IO)
        ),
        list.foldl(dom.print, Node ^ children, !IO),
        io.write_string(format("</%s>", [s(TagName)]), !IO)
    ).

:- func stringify_attrs(attributes) = string.

stringify_attrs(Attrs) = Str :-
    AttrsList0 = map.to_sorted_assoc_list(Attrs),
    AttrsList = list.map(stringify_attr, AttrsList0),
    Str = string.join_list(" ", AttrsList).

:- func stringify_attr(pair(string, string)) = string.

stringify_attr(Name - Value) = format("%s=\"%s\"", [s(Name), s(Value)]).
