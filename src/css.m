%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module css.
:- interface.

:- import_module list.
:- import_module maybe.

:- type stylesheet
    --->    stylesheet(rules).

:- type rules == list(rule).

:- type (rule)
    --->    rule(
                selectors    :: selectors,
                declarations :: declarations
            ).

:- type selectors == list(selector).

:- type selector
    --->    simple(
                tag_name :: maybe(string),
                id       :: maybe(string),
                classes  :: list(string)
            ).

:- type declarations == list(declaration).

:- type declaration
    --->    declaration(
                name  :: string,
                value :: value
            ).

:- type value
    --->    keyword(string)
    ;       length(float, unit)
    ;       colour(colour).

:- type unit
    --->    px.

:- type colour
    --->    colour(
                r :: int,
                g :: int,
                b :: int,
                a :: int
            ).

:- type specificity
    --->    specificity(int, int, int).

%------------------------------------------------------------------------------%

:- pred parse(string::in, stylesheet::out) is det.

:- func specificity(selector) = specificity.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.

:- import_module parser.

%------------------------------------------------------------------------------%

parse(Input, Stylesheet) :-
    P = parser(0, Input, string.length(Input)),
    parse_rules(Rules, P, _),
    Stylesheet = stylesheet(Rules).

%------------------------------------------------------------------------------%

:- pred parse_rules(rules::out, parser::in, parser::out) is det.

parse_rules(reverse(Rules), !P) :- parse_rules([], Rules, !P).

:- pred parse_rules(rules::in, rules::out, parser::in, parser::out) is det.

parse_rules(!Rules, !P) :-
    parser.consume_whitespace(!P),
    ( if parser.eof(!.P) then
        true
    else
        parse_rule(Rule, !P),
        list.cons(Rule, !Rules),
        parse_rules(!Rules, !P)
    ).

:- pred parse_rule((rule)::out, parser::in, parser::out) is det.

parse_rule(Rule, !P) :-
    parse_selectors(Selectors, !P),
    parse_declarations(Declarations, !P),
    Rule = rule(Selectors, Declarations).

%------------------------------------------------------------------------------%

:- pred parse_selectors(selectors::out, parser::in, parser::out) is det.

parse_selectors(Selectors, !P) :-
    parse_selectors([], Selectors0, !P),
    Selectors = list.sort(compare_selectors, Selectors0).

:- pred parse_selectors(selectors::in, selectors::out,
    parser::in, parser::out) is det.

parse_selectors(!Selectors, !P) :-
    parse_simple_selector(Simple, !P),
    list.cons(Simple, !Selectors),
    parser.consume_whitespace(!P),
    C = parser.get_char(!.P),
    ( if C = (',') then
        parser.consume_char(_, !P),
        parser.consume_whitespace(!P),
        parse_selectors(!Selectors, !P)
    else if C = ('{') then
        true
    else
        error(format("Unexpected char '%c' in selector list.", [c(C)]))
    ).

:- func compare_selectors(selector, selector) = comparison_result.

compare_selectors(S1, S2) = Res :-
    Spec1 = specificity(S1),
    Spec2 = specificity(S2),
    compare(Res, Spec2, Spec1). % Higher specificity first.

specificity(Selector) = Specificity :-
    Selector = simple(TagName, Id, Class),
    ( if Id = yes(_) then A = 1 else A = 0),
    B = list.length(Class),
    ( if TagName = yes(_) then C = 1 else C = 0),
    Specificity = specificity(A, B, C).

%------------------------------------------------------------------------------%

:- pred parse_simple_selector(selector::out, parser::in, parser::out) is det.

parse_simple_selector(Simple, !P) :-
    parse_simple_selector(simple(no, no, []), Simple, !P).

:- pred parse_simple_selector(selector::in, selector::out,
    parser::in, parser::out) is det.

parse_simple_selector(!Simple, !P) :-
    ( if parser.eof(!.P) then
        true
    else
        C = parser.get_char(!.P),
        ( if C = ('#') then
            parser.consume_char(_, !P),
            parse_identifier(Id, !P),
            !Simple ^ id := yes(Id),
            parse_simple_selector(!Simple, !P)
        else if C = ('.') then
            parser.consume_char(_, !P),
            parse_identifier(Class, !P),
            !Simple ^ classes := list.cons(Class, !.Simple ^ classes),
            parse_simple_selector(!Simple, !P)
        else if C = ('*') then
            parser.consume_char(_, !P),
            parse_simple_selector(!Simple, !P)
        else if valid_identifier_char(C) then
            parse_identifier(TagName, !P),
            !Simple ^ tag_name := yes(TagName),
            parse_simple_selector(!Simple, !P)
        else
            true
        )
    ).

%------------------------------------------------------------------------------%

:- pred parse_declarations(declarations::out, parser::in, parser::out) is det.

parse_declarations(reverse(Decls), !P) :-
    parser.expect_char(('{'), !P),
    parse_declarations([], Decls, !P).

:- pred parse_declarations(declarations::in, declarations::out,
    parser::in, parser::out) is det.

parse_declarations(!Decls, !P) :-
    parser.consume_whitespace(!P),
    ( if parser.get_char(!.P) = ('}') then
        parser.consume_char(_, !P)
    else
        parse_declaration(Decl, !P),
        list.cons(Decl, !Decls),
        parse_declarations(!Decls, !P)
    ).

:- pred parse_declaration(declaration::out, parser::in, parser::out) is det.

parse_declaration(Decl, !P) :-
    parse_identifier(Name, !P),
    parser.consume_whitespace(!P),
    parser.expect_char((':'), !P),
    parser.consume_whitespace(!P),
    parse_value(Value, !P),
    parser.consume_whitespace(!P),
    parser.expect_char((';'), !P),
    Decl = declaration(Name, Value).

%------------------------------------------------------------------------------%

:- pred parse_value(value::out, parser::in, parser::out) is det.

parse_value(Value, !P) :-
    C = parser.get_char(!.P),
    ( if char.is_decimal_digit(C) then
        parse_length(Value, !P)
    else if C = ('#') then
        parse_colour(Value, !P)
    else
        parse_identifier(Id, !P),
        Value = keyword(Id)
    ).

%------------------------------------------------------------------------------%

:- pred parse_length(value::out, parser::in, parser::out) is det.

parse_length(Length, !P) :-
    parse_float(F, !P),
    parse_unit(Unit, !P),
    Length = length(F, Unit).

:- pred parse_float(float::out, parser::in, parser::out) is det.

parse_float(F, !P) :-
    parser.consume_while(valid_float_char, FStr, !P),
    F = string.det_to_float(FStr).

:- pred parse_unit(unit::out, parser::in, parser::out) is det.

parse_unit(Unit, !P) :-
    parse_identifier(UnitStr0, !P),
    UnitStr = string.to_lower(UnitStr0),
    ( if UnitStr = "px" then
        Unit = px
    else
        error(format("Unrecognised unit '%s'.", [s(UnitStr)]))
    ).

%------------------------------------------------------------------------------%

:- pred parse_colour(value::out, parser::in, parser::out) is det.

parse_colour(Colour, !P) :-
    parser.expect_char(('#'), !P),
    parse_hex_pair(R, !P),
    parse_hex_pair(G, !P),
    parse_hex_pair(B, !P),
    A = 255,
    Colour = colour(colour(R, G, B, A)).

:- pred parse_hex_pair(int::out, parser::in, parser::out) is det.

parse_hex_pair(I, !P) :-
    parser.consume_char(FstC, !P),
    FstI = char.det_hex_digit_to_int(FstC) * 16,
    parser.consume_char(SndC, !P),
    SndI = char.det_hex_digit_to_int(SndC),
    I = FstI + SndI.

%------------------------------------------------------------------------------%

:- pred parse_identifier(string::out, parser::in, parser::out) is det.

parse_identifier(Id, !P) :-
    parser.consume_while(valid_identifier_char, Id, !P).

%------------------------------------------------------------------------------%

:- pred valid_identifier_char(char::in) is semidet.

valid_identifier_char(('-')).
valid_identifier_char(('_')).
valid_identifier_char(C) :- char.is_alnum(C).

:- pred valid_float_char(char::in) is semidet.

valid_float_char(('.')).
valid_float_char(C) :- char.is_decimal_digit(C).
