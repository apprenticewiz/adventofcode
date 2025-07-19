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
        string_chars(Content, Chars),
        foldl(count, Chars, 0, Result),
        format("result = ~w~n", [Result]),
        halt(0)
    ;   usage,
        halt(1)
    ).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").

count('(', Acc, Res) :- Res is Acc + 1.
count(')', Acc, Res) :- Res is Acc - 1.
count(_, Acc, Acc).
