%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module dom.
:- interface.

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

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

text(Data) =
    node([], text(Data)).

element(Name, Attrs, Children) = Node :-
    Elem = element(Name, Attrs),
    Node = node(Children, element(Elem)).
