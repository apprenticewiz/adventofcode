#!/usr/bin/env swipl

:- use_module(library(readutil)).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Args),
    (   Args = [Filename] ->
	catch(
            read_file_to_string(Filename, Content, []),
	    Error,
	    ( print_message(error, Error), halt(1) )
	),
        process(Content, Result),
        format("result = ~w~n", [Result]),
        halt(0)
    ;   usage,
        halt(1)
    ).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").

process(Content, Result) :-
    split_string(Content, "\n", "\n", Lines),
    foldl(process_line, Lines, 0, Result).

process_line(Line, Acc, NewAcc) :-
    split_string(Line, "x", "", Parts),
    maplist(number_string, Dimensions, Parts),
    Dimensions = [L, W, H],
    Perim1 is 2 * (L + W),
    Perim2 is 2 * (L + H),
    Perim3 is 2 * (W + H),
    min_list([Perim1, Perim2, Perim3], PresentLen),
    BowLen is L * W * H,
    NewAcc is Acc + PresentLen + BowLen.
