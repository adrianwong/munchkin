%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module parser.
:- interface.

:- import_module char.
:- import_module string.

:- type parser
    --->    parser(
                pos    :: int,
                in     :: string,
                in_len :: int
            ).

%------------------------------------------------------------------------------%

:- pred consume_char(char::out, parser::in, parser::out) is det.

:- pred expect_char(char::in, parser::in, parser::out) is det.

:- pred consume_while(pred(char)::in(pred(in) is semidet), string::out,
    parser::in, parser::out) is det.

:- pred consume_whitespace(parser::in, parser::out) is det.

%------------------------------------------------------------------------------%

:- func get_char(parser) = char.

:- pred starts_with(string::in, parser::in) is semidet.

:- pred eof(parser::in) is semidet.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

%------------------------------------------------------------------------------%

consume_char(C, !P) :-
    ( if string.unsafe_index_next(!.P ^ in, !.P ^ pos, NextPos, C0) then
        C = C0,
        !P ^ pos := NextPos
    else
        error("Unexpected EOF.")
    ).

expect_char(Ex, !P) :-
    consume_char(C, !P),
    ( if Ex = C then
        true
    else
        error(format("Expected '%c', got '%c'.", [c(Ex), c(C)]))
    ).

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

consume_whitespace(!P) :- consume_while(char.is_whitespace, _, !P).

%------------------------------------------------------------------------------%

get_char(P) = string.unsafe_index(P ^ in, P ^ pos).

starts_with(Str, P) :-
    string.unsafe_sub_string_search_start(P ^ in, Str, P ^ pos, P ^ pos).

eof(P) :- P ^ pos >= P ^ in_len.
