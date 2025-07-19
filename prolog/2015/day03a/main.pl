#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(rbtrees)).

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
    string_chars(Content, Moves),
    Initial = pos(0, 0),
    rb_new(EmptySet),
    rb_insert(EmptySet, Initial, true, InitSet),
    foldl(process_move, Moves, (InitSet, Initial), (FinalSet, _)),
    rb_size(FinalSet, Result).

process_move(Char, (VisitedIn, PosIn), (VisitedOut, PosOut)) :-
    move(Char, PosIn, PosOut),
    rb_insert(VisitedIn, PosOut, true, VisitedOut).

move('^', pos(X, Y), pos(X, Y1)) :- Y1 is Y + 1.
move('v', pos(X, Y), pos(X, Y1)) :- Y1 is Y - 1.
move('<', pos(X, Y), pos(X1, Y)) :- X1 is X - 1.
move('>', pos(X, Y), pos(X1, Y)) :- X1 is X + 1.
move(_, Pos, Pos).
