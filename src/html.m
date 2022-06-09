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
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

:- type parser
    --->    parser(
                pos    :: int,
                in     :: string,
                in_len :: int
            ).

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

%------------------------------------------------------------------------------%

:- pred consume_char(char::out, parser::in, parser::out) is det.

consume_char(C, !P) :-
    ( if string.unsafe_index_next(!.P ^ in, !.P ^ pos, NextPos, C0) then
        C = C0,
        !P ^ pos := NextPos
    else
        error("Unexpected EOF.")
    ).

:- pred expect_char(char::in, parser::in, parser::out) is det.

expect_char(Ex, !P) :-
    consume_char(C, !P),
    ( if Ex = C then
        true
    else
        error(format("Expected '%c', got '%c'.", [c(Ex), c(C)]))
    ).

:- pred consume_while(pred(char)::in(pred(in) is semidet), string::out,
    parser::in, parser::out) is det.

consume_while(Pred, Str, !P) :-
    Start = !.P ^ pos,
    consume_while(Pred, !P),
    Str = string.unsafe_between(!.P ^ in, Start, !.P ^ pos).

:- pred consume_while(pred(char)::in(pred(in) is semidet),
    parser::in, parser::out) is det.

consume_while(Pred, !P) :-
    ( if not eof(!.P), Pred(get_char(!.P)) then
        consume_char(_, !P),
        consume_while(Pred, !P)
    else
        true
    ).

:- pred consume_whitespace(parser::in, parser::out) is det.

consume_whitespace(!P) :- consume_while(char.is_whitespace, _, !P).

%------------------------------------------------------------------------------%

:- func get_char(parser) = char.

get_char(P) = string.unsafe_index(P ^ in, P ^ pos).

:- pred starts_with(string::in, parser::in) is semidet.

starts_with(Str, P) :-
    string.unsafe_sub_string_search_start(P ^ in, Str, P ^ pos, P ^ pos).

:- pred eof(parser::in) is semidet.

eof(P) :- P ^ pos >= P ^ in_len.
