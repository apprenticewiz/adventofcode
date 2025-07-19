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
	process(Chars, Result),
        format("result = ~w~n", [Result]),
        halt(0)
    ;   usage,
        halt(1)
    ).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").

process(Chars, Position) :-
    find_position(Chars, 0, 0, Position).

find_position(_, Pos, Count, Pos) :-
    Count < 0, !.
find_position([], Pos, _, Pos).
find_position([Char | Rest], Pos, Count, Result) :-
    update_count(Char, Count, NewCount),
    NextPos is Pos + 1,
    find_position(Rest, NextPos, NewCount, Result).

update_count('(', C, R) :- R is C + 1.
update_count(')', C, R) :- R is C - 1.
update_count(_, C, C).
