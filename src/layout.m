%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module layout.
:- interface.

:- import_module list.

:- import_module style.

:- type dimensions
    --->    dimensions(
                content :: rect,
                padding :: edge_sizes,
                border  :: edge_sizes,
                margin  :: edge_sizes
            ).

:- type rect
    --->    rect(
                x      :: float,
                y      :: float,
                width  :: float,
                height :: float
            ).

:- type edge_sizes
    --->    edge_sizes(
                left   :: float,
                right  :: float,
                top    :: float,
                bottom :: float
            ).

:- type layout_box
    --->    layout_box(
                dimensions :: dimensions,
                box_type   :: box_type,
                children   :: list(layout_box)
            ).

:- type box_type
    --->    block_node(styled_node)
    ;       inline_node(styled_node)
    ;       anonymous_block.

:- pred layout_tree(styled_node::in, dimensions::in, layout_box::out) is det.

:- func new_dimensions = dimensions.

:- func border_box(dimensions) = rect.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module require.

:- import_module css.

%------------------------------------------------------------------------------%

layout_tree(StyledRoot, ContainingBlock0, BoxRoot) :-
    ContainingBlock = ContainingBlock0 ^ content ^ height := 0.0,
    BoxRoot0 = build_layout_tree(StyledRoot),
    layout(ContainingBlock, BoxRoot0, BoxRoot).

%------------------------------------------------------------------------------%

:- pred layout(dimensions::in, layout_box::in, layout_box::out) is det.

layout(ContainingBlock, !Box) :-
    BoxType = !.Box ^ box_type,
    (
        BoxType = block_node(_),
        layout_block(ContainingBlock, !Box)
    ;
        BoxType = inline_node(_) % TODO
    ;
        BoxType = anonymous_block % TODO
    ).

:- pred layout_block(dimensions::in, layout_box::in, layout_box::out) is det.

layout_block(ContainingBlock, !Block) :-
    calculate_block_width(ContainingBlock, !Block),
    calculate_block_position(ContainingBlock, !Block),
    layout_block_children(!Block),
    calculate_block_height(!Block).

:- pred layout_block_children(layout_box::in, layout_box::out) is det.

layout_block_children(!Block) :-
    list.map_foldl(layout_block_children0, !.Block ^ children, Children,
        !.Block ^ dimensions, Dimensions),
    !Block ^ dimensions := Dimensions,
    !Block ^ children := Children.

:- pred layout_block_children0(layout_box::in, layout_box::out,
    dimensions::in, dimensions::out) is det.

layout_block_children0(!Child, !Dim) :-
    layout(!.Dim, !Child),
    Hi = !.Dim ^ content ^ height + margin_box(!.Child ^ dimensions) ^ height,
    !Dim ^ content ^ height := Hi.

%------------------------------------------------------------------------------%

:- pred calculate_block_width(dimensions::in,
    layout_box::in, layout_box::out) is det.

calculate_block_width(ContainingBlock, !Block) :-
    Style = get_styled_node(!.Block),
    Auto = keyword("auto"),
    ( if style.value(Style, "width", Wi0) then
        Wi1 = Wi0
    else
        Wi1 = Auto
    ),
    Zero = length(0.0, px),
    Ml0 = style.lookup(Style, "margin-left", "margin", Zero),
    Mr0 = style.lookup(Style, "margin-right", "margin", Zero),
    Bl = style.lookup(Style, "border-left-width", "border-width", Zero),
    Br = style.lookup(Style, "border-right-width", "border-width", Zero),
    Pl = style.lookup(Style, "padding-left", "padding", Zero),
    Pr = style.lookup(Style, "padding-right", "padding", Zero),
    Pxs = list.map(css.to_px, [Ml0, Mr0, Bl, Br, Pl, Pr]),
    Total = list.foldl((+), Pxs, 0.0),
    ( if
        Wi1 \= Auto,
        Total > ContainingBlock ^ content ^ width
    then
        ( if Ml0 = Auto then Ml1 = length(0.0, px) else Ml1 = Ml0),
        ( if Mr0 = Auto then Mr1 = length(0.0, px) else Mr1 = Mr0)
    else
        Ml1 = Ml0,
        Mr1 = Mr0
    ),
    Underflow = ContainingBlock ^ content ^ width - Total,
    ( if Wi1 = Auto then
        ( if Ml1 = Auto then Ml2 = length(0.0, px) else Ml2 = Ml1),
        ( if Mr1 = Auto then Mr2 = length(0.0, px) else Mr2 = Mr1),
        ( if Underflow >= 0.0 then
            Wi = length(Underflow, px),
            Ml = Ml2,
            Mr = Mr2
        else
            Wi = length(0.0, px),
            Ml = Ml2,
            Mr = length(to_px(Mr2) + Underflow, px)
        )
    else
        Wi = Wi1,
        ( if Ml1 = Auto, Mr1 = Auto then
            Ml = length(Underflow / 2.0, px),
            Mr = length(Underflow / 2.0, px)
        else if Ml1 = Auto, Mr1 \= Auto then
            Ml = length(Underflow, px),
            Mr = Mr1
        else if Ml1 \= Auto, Mr1 = Auto then
            Ml = Ml1,
            Mr = length(Underflow, px)
        else
            Ml = Ml1,
            Mr = length(to_px(Mr1) + Underflow, px)
        )
    ),
    !Block ^ dimensions ^ content ^ width := to_px(Wi),
    !Block ^ dimensions ^ padding ^ left := to_px(Pl),
    !Block ^ dimensions ^ padding ^ right := to_px(Pr),
    !Block ^ dimensions ^ border ^ left := to_px(Bl),
    !Block ^ dimensions ^ border ^ right := to_px(Br),
    !Block ^ dimensions ^ margin ^ left := to_px(Ml),
    !Block ^ dimensions ^ margin ^ right := to_px(Mr).

:- pred calculate_block_position(dimensions::in,
    layout_box::in, layout_box::out) is det.

calculate_block_position(ContainingBlock, !Block) :-
    Style = get_styled_node(!.Block),
    Zero = length(0.0, px),
    Mt = style.lookup(Style, "margin-top", "margin", Zero),
    Mb = style.lookup(Style, "margin-bottom", "margin", Zero),
    Bt = style.lookup(Style, "border-top-width", "border-width", Zero),
    Bb = style.lookup(Style, "border-bottom-width", "border-width", Zero),
    Pt = style.lookup(Style, "padding-top", "padding", Zero),
    Pb = style.lookup(Style, "padding-bottom", "padding", Zero),
    !Block ^ dimensions ^ margin ^ top := to_px(Mt),
    !Block ^ dimensions ^ margin ^ bottom := to_px(Mb),
    !Block ^ dimensions ^ border ^ top := to_px(Bt),
    !Block ^ dimensions ^ border ^ bottom := to_px(Bb),
    !Block ^ dimensions ^ padding ^ top := to_px(Pt),
    !Block ^ dimensions ^ padding ^ bottom := to_px(Pb),
    !Block ^ dimensions ^ content ^ x :=
        ContainingBlock ^ content ^ x +
        !.Block ^ dimensions ^ margin ^ left +
        !.Block ^ dimensions ^ border ^ left +
        !.Block ^ dimensions ^ padding ^ left,
    !Block ^ dimensions ^ content ^ y :=
        ContainingBlock ^ content ^ height +
        ContainingBlock ^ content ^ y +
        !.Block ^ dimensions ^ margin ^ top +
        !.Block ^ dimensions ^ border ^ top +
        !.Block ^ dimensions ^ padding ^ top.

:- pred calculate_block_height(layout_box::in, layout_box::out) is det.

calculate_block_height(!Block) :-
    Style = get_styled_node(!.Block),
    ( if
        style.value(Style, "height", Hi0),
        Hi0 = length(Hi, px)
    then
        !Block ^ dimensions ^ content ^ height := Hi
    else
        true
    ).

%------------------------------------------------------------------------------%

:- func build_layout_tree(styled_node) = layout_box.

build_layout_tree(StyledRoot) = BoxRoot :-
    Display = style.display(StyledRoot),
    (
        Display = block,
        BoxType = block_node(StyledRoot)
    ;
        Display = inline,
        BoxType = inline_node(StyledRoot)
    ;
        Display = none,
        error("Root node has 'display: none'.")
    ),
    BoxRoot0 = new(BoxType),
    list.foldl(build_layout_tree_children, StyledRoot ^ children,
        BoxRoot0, BoxRoot1),
    BoxRoot = BoxRoot1 ^ children := list.reverse(BoxRoot1 ^ children).

:- pred build_layout_tree_children(styled_node::in,
    layout_box::in, layout_box::out) is det.

build_layout_tree_children(StyledChild, !BoxRoot) :-
    Display = style.display(StyledChild),
    (
        Display = block,
        Cs = [build_layout_tree(StyledChild) | !.BoxRoot ^ children],
        !BoxRoot ^ children := Cs
    ;
        Display = inline,
        InlineContainer = get_inline_container(!.BoxRoot),
        Cs = [build_layout_tree(StyledChild) | InlineContainer ^ children],
        !BoxRoot ^ children := Cs
    ;
        Display = none
    ).

:- func get_inline_container(layout_box) = layout_box.

get_inline_container(LayoutBox0) = LayoutBox :-
    BoxType0 = LayoutBox0 ^ box_type,
    (
        ( BoxType0 = inline_node(_)
        ; BoxType0 = anonymous_block
        ),
        LayoutBox = LayoutBox0
    ;
        BoxType0 = block_node(_),
        Cs = LayoutBox0 ^ children,
        ( if Cs = [C | _], C ^ box_type = anonymous_block then
            LayoutBox = LayoutBox0
        else
            Anonymous = new(anonymous_block),
            LayoutBox = LayoutBox0 ^ children := [Anonymous | Cs]
        )
    ).

%------------------------------------------------------------------------------%

:- func padding_box(dimensions) = rect.

padding_box(D) = expanded_by(D ^ content, D ^ padding).

border_box(D) = expanded_by(padding_box(D), D ^ border).

:- func margin_box(dimensions) = rect.

margin_box(D) = expanded_by(border_box(D), D ^ margin).

:- func expanded_by(rect, edge_sizes) = rect.

expanded_by(R0, Edge) = R :-
    X = R0 ^ x - Edge ^ left,
    Y = R0 ^ y - Edge ^ top,
    W = R0 ^ width + Edge ^ left + Edge ^ right,
    H = R0 ^ height + Edge ^ top + Edge ^ bottom,
    R = rect(X, Y, W, H).

%------------------------------------------------------------------------------%

:- func new(box_type) = layout_box.

new(BoxType) = LayoutBox :-
    DefaultD = new_dimensions,
    LayoutBox = layout_box(DefaultD, BoxType, []).

new_dimensions = DefaultD :-
    DefaultR = rect(0.0, 0.0, 0.0, 0.0),
    DefaultE = edge_sizes(0.0, 0.0, 0.0, 0.0),
    DefaultD = dimensions(DefaultR, DefaultE, DefaultE, DefaultE).

:- func get_styled_node(layout_box) = styled_node.

get_styled_node(LayoutBox) = StyledNode :-
    BoxType = LayoutBox ^ box_type,
    (
        BoxType = block_node(StyledNode)
    ;
        BoxType = inline_node(StyledNode)
    ;
        BoxType = anonymous_block,
        error("Anonymous block box has no styled node.")
    ).
