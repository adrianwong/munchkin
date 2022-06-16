%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module style.
:- interface.

:- import_module list.
:- import_module map.

:- import_module css.
:- import_module dom.

:- type property_map == map(string, value).

:- type styled_node
    --->    styled_node(
                node             :: node,
                specified_values :: property_map,
                children         :: list(styled_node)
            ).

:- type display
    --->    inline
    ;       block
    ;       none.

:- pred style_tree(stylesheet::in, node::in, styled_node::out) is det.

:- pred value(styled_node::in, string::in, value::out) is semidet.

:- func display(styled_node) = display.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%------------------------------------------------------------------------------%

style_tree(Stylesheet, Root, StyledRoot) :-
    NodeType = Root ^ node_type,
    (
        NodeType = text(_),
        SpecifiedValues = map.init
    ;
        NodeType = element(Elem),
        SpecifiedValues = specified_values(Elem, Stylesheet)
    ),
    list.map(style_tree(Stylesheet), Root ^ children, StyledChildren),
    StyledRoot = styled_node(Root, SpecifiedValues, StyledChildren).

%------------------------------------------------------------------------------%

value(StyledNode, Name, Value) :-
    SpecifiedValues = StyledNode ^ specified_values,
    Value = map.search(SpecifiedValues, Name).

display(StyledNode) = Display :-
    ( if value(StyledNode, "display", keyword(Keyword)) then
        ( if Keyword = "block" then
            Display = block
        else if Keyword = "none" then
            Display = none
        else
            Display = inline
        )
    else
        Display = inline
    ).

%------------------------------------------------------------------------------%

:- func specified_values(element, stylesheet) = property_map.

specified_values(Elem, Stylesheet) = PropertyMap :-
    MatchedRules0 = matching_rules(Elem, Stylesheet),
    MatchedRules = list.sort(compare_matched_rules, MatchedRules0),
    assoc_list.foldl_values(add_declarations_to_property_map, MatchedRules,
        map.init, PropertyMap).

:- pred add_declarations_to_property_map((rule)::in,
    property_map::in, property_map::out) is det.

add_declarations_to_property_map(Rule, !PropertyMap) :-
    Decls = Rule ^ declarations,
    list.foldl(add_declaration_to_property_map, Decls, !PropertyMap).

:- pred add_declaration_to_property_map(declaration::in,
    property_map::in, property_map::out) is det.

add_declaration_to_property_map(Decl, !PropertyMap) :-
    map.set(Decl ^ name, Decl ^ value, !PropertyMap).

:- func compare_matched_rules(matched_rule, matched_rule) = comparison_result.

compare_matched_rules(Spec1 - _, Spec2 - _) = Res :-
    compare(Res, Spec1, Spec2). % Lower specificity first.

%------------------------------------------------------------------------------%

:- type matched_rule == pair(specificity, rule).

:- func matching_rules(element, stylesheet) = list(matched_rule).

matching_rules(Elem, stylesheet(Rules)) = MatchedRules :-
    list.filter_map(match_rule(Elem), Rules, MatchedRules).

:- pred match_rule(element::in, (rule)::in, matched_rule::out) is semidet.

match_rule(Elem, Rule, MatchedRule) :-
    Selectors = Rule ^ selectors,
    list.find_first_match(matches_simple_selector(Elem), Selectors, Selector),
    MatchedRule = specificity(Selector) - Rule.

:- pred matches_simple_selector(element::in, selector::in) is semidet.

matches_simple_selector(Elem, simple(MaybeTagName, MaybeId, Classes)) :-
    MaybeTagName = yes(TagName),
    TagName = Elem ^ tag_name,
    MaybeId = yes(_),
    MaybeId = id(Elem),
    ElemClasses = classes(Elem),
    list.any_true(matches_class(ElemClasses), Classes).

:- pred matches_class(set(string)::in, string::in) is semidet.

matches_class(ElemClasses, C) :- set.member(C, ElemClasses).
