%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module painting.
:- interface.

:- import_module list.

:- import_module css.
:- import_module layout.

:- type display_command
    --->    solid_colour(colour, rect).

:- type display_list == list(display_command).

:- pred build_display_list(layout_box::in, display_list::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module style.

:- import_module float.

%------------------------------------------------------------------------------%

build_display_list(BoxRoot, reverse(DisplayList)) :-
    render_layout_box(BoxRoot, [], DisplayList).

%------------------------------------------------------------------------------%

:- pred render_layout_box(layout_box::in,
    display_list::in, display_list::out) is det.

render_layout_box(Box, !DisplayList) :-
    render_background(Box, !DisplayList),
    render_borders(Box, !DisplayList),
    list.foldl(render_layout_box, Box ^ children, !DisplayList).

:- pred render_background(layout_box::in,
    display_list::in, display_list::out) is det.

render_background(Box, !DisplayList) :-
    ( if get_colour(Box, "background", Colour) then
        Bg = solid_colour(Colour, layout.border_box(Box ^ dimensions)),
        !:DisplayList = [Bg | !.DisplayList]
    else
        true
    ).

:- pred render_borders(layout_box::in,
    display_list::in, display_list::out) is det.

render_borders(Box, !DisplayList) :-
    ( if get_colour(Box, "border-color", Colour) then
        Dim = Box ^ dimensions,
        BB = layout.border_box(Dim),
        L = solid_colour(Colour, rect(
            BB ^ x,
            BB ^ y,
            Dim ^ border ^ left,
            BB ^ height)),
        R = solid_colour(Colour, rect(
            BB ^ x + BB ^ width - Dim ^ border ^ right,
            BB ^ y,
            Dim ^ border ^ right,
            BB ^ height)),
        T = solid_colour(Colour, rect(
            BB ^ x,
            BB ^ y,
            BB ^ width,
            Dim ^ border ^ top)),
        B = solid_colour(Colour, rect(
            BB ^ x,
            BB ^ y + BB ^ height - Dim ^ border ^ bottom,
            BB ^ width,
            Dim ^ border ^ bottom)),
        !:DisplayList = [B, T, R, L | !.DisplayList]
    else
        true
    ).

:- pred get_colour(layout_box::in, string::in, colour::out) is semidet.

get_colour(Box, Name, Colour) :-
    BoxType = Box ^ box_type,
    (
        ( BoxType = block_node(Style)
        ; BoxType = inline_node(Style)
        ),
        style.value(Style, Name, colour(Colour))
    ;
        BoxType = anonymous_block,
        fail
    ).
