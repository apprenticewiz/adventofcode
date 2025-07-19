#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).

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
    include(is_nice, Lines, NiceLines),
    length(NiceLines, Result).

is_nice(String) :-
    string_chars(String, Chars),
    prop1(Chars),
    prop2(Chars).

prop1(Chars) :- prop1(Chars, []).

prop1([A, B | Rest], Seen) :-
    Pair = [A, B],
    append(_, [A, B | _], Rest),
    \+ prefix(Pair, Seen),
    !.
prop1([_ | Rest], Seen) :- prop1(Rest, Seen).
prop1(_, _) :- fail.

prop2([A, _, A | _]) :- !.
prop2([_ | Rest]) :- prop2(Rest).
prop2(_) :- fail.
