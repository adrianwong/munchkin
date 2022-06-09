%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module html.
:- interface.

:- import_module dom.

:- pred parse(string::in, node::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

:- import_module parser.

%------------------------------------------------------------------------------%

parse(Input, Root) :-
    P = parser(0, Input, string.length(Input)),
    parse_nodes(Nodes, P, _),
    ( if Nodes = [Node] then
        Root = Node
    else
        Root = element("html", map.init, Nodes)
    ).

%------------------------------------------------------------------------------%

:- pred parse_nodes(list(node)::out, parser::in, parser::out) is det.

parse_nodes(reverse(Nodes), !P) :- parse_nodes([], Nodes, !P).

:- pred parse_nodes(list(node)::in, list(node)::out,
    parser::in, parser::out) is det.

parse_nodes(!Nodes, !P) :-
    consume_whitespace(!P),
    ( if eof(!.P) ; starts_with("</", !.P) then
        true
    else
        parse_node(Node, !P),
        list.cons(Node, !Nodes),
        parse_nodes(!Nodes, !P)
    ).

:- pred parse_node(node::out, parser::in, parser::out) is det.

parse_node(Node, !P) :-
    ( if get_char(!.P) = ('<') then
        parse_element(Node, !P)
    else
        parse_text(Node, !P)
    ).

%------------------------------------------------------------------------------%

:- pred parse_element(node::out, parser::in, parser::out) is det.

parse_element(Node, !P) :-
    % Opening tag.
    expect_char(('<'), !P),
    parse_tag_name(OpenTag, !P),
    parse_attrs(map.init, Attrs, !P),
    expect_char(('>'), !P),
    % Contents.
    parse_nodes(Children, !P),
    % Closing tag.
    expect_char(('<'), !P),
    expect_char(('/'), !P),
    parse_tag_name(CloseTag, !P),
    ( if OpenTag = CloseTag then
        true
    else
        error(format("Mismatch between opening tag '%s' and closing tag '%s'.",
            [s(OpenTag), s(CloseTag)]))
    ),
    expect_char(('>'), !P),
    % Construct node.
    Node = element(OpenTag, Attrs, Children).

:- pred parse_text(node::out, parser::in, parser::out) is det.

parse_text(Node, !P) :-
    consume_while(pred(C::in) is semidet :- C \= ('<'), Text, !P),
    Node = text(Text).

%------------------------------------------------------------------------------%

:- pred parse_attrs(attributes::in, attributes::out,
    parser::in, parser::out) is det.

parse_attrs(!Attrs, !P) :-
    consume_whitespace(!P),
    ( if get_char(!.P) = ('>') then
        true
    else
        parse_attr(Name, Value, !P),
        map.det_insert(Name, Value, !Attrs),
        parse_attrs(!Attrs, !P)
    ).

:- pred parse_attr(string::out, string::out, parser::in, parser::out) is det.

parse_attr(Name, Value, !P) :-
    parse_tag_name(Name, !P),
    expect_char(('='), !P),
    parse_attr_value(Value, !P).

:- pred parse_attr_value(string::out, parser::in, parser::out) is det.

parse_attr_value(Value, !P) :-
    consume_char(OpenQuote, !P),
    ( if OpenQuote = ('"') ; OpenQuote = ('\'') then
        consume_while(pred(C::in) is semidet :- C \= OpenQuote, Value, !P),
        expect_char(OpenQuote, !P)
    else
        error(format("Expected '\"' or '\'', got '%c'.", [c(OpenQuote)]))
    ).

%------------------------------------------------------------------------------%

:- pred parse_tag_name(string::out, parser::in, parser::out) is det.

parse_tag_name(Name, !P) :- consume_while(char.is_alnum, Name, !P).
