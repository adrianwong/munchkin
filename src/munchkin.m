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

:- import_module dom.
:- import_module html.

%------------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [InputFile],
        string.suffix(InputFile, file_extension)
    then
        io.read_named_file_as_string(InputFile, ReadRes, !IO),
        (
            ReadRes = ok(Input),
            html.parse(Input, Node),
            dom.print_nl(Node, !IO)
        ;
            ReadRes = error(Err),
            io.format(io.stderr_stream, "I/O error: %s.\n",
                [s(io.error_message(Err))], !IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.progname_base("munchkin", Prog, !IO),
        io.format(io.stderr_stream, "Usage: %s file%s.\n",
            [s(Prog), s(file_extension)], !IO),
            io.set_exit_status(1, !IO)
    ).

%------------------------------------------------------------------------------%

:- func file_extension = string.

file_extension = ".html".
