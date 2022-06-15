%------------------------------------------------------------------------------%
% munchkin - a toy web rendering engine.
% Copyright (C) 2022 Adrian Wong.
%------------------------------------------------------------------------------%

:- module munchkin.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module css.
:- import_module dom.
:- import_module html.
:- import_module style.

%------------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [HtmlFile, CssFile],
        string.suffix(HtmlFile, html_file_extension),
        string.suffix(CssFile, css_file_extension)
    then
        io.read_named_file_as_string(HtmlFile, HtmlReadRes, !IO),
        io.read_named_file_as_string(CssFile, CssReadRes, !IO),
        ( if HtmlReadRes = ok(HtmlInput), CssReadRes = ok(CssInput) then
            html.parse(HtmlInput, Root),
            css.parse(CssInput, Stylesheet),
            style.style_tree(Stylesheet, Root, _StyledRoot),
            dom.print_nl(Root, !IO),
            Stylesheet = stylesheet(Rules),
            io.write_string(format("# rules: %d\n", [i(length(Rules))]), !IO)
        else
            io.format(io.stderr_stream, "I/O error.", [], !IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.progname_base("munchkin", Prog, !IO),
        io.format(io.stderr_stream, "Usage: %s file%s style%s.\n",
            [s(Prog), s(html_file_extension), s(css_file_extension)], !IO),
        io.set_exit_status(1, !IO)
    ).

%------------------------------------------------------------------------------%

:- func html_file_extension = string.

html_file_extension = ".html".

:- func css_file_extension = string.

css_file_extension = ".css".
