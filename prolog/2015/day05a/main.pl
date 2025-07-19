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
    prop2(Chars),
    prop3(String).

prop1(Chars) :-
    include(is_vowel, Chars, Vowels),
    length(Vowels, Count),
    Count >= 3.

is_vowel(Char) :-
    member(Char, [a, e, i, o, u]).

prop2([A, A | _]).
prop2([_ | Rest]) :-
    prop2(Rest).

prop3(String) :-
    \+ sub_string(String, _, _, _, "ab"),
    \+ sub_string(String, _, _, _, "cd"),
    \+ sub_string(String, _, _, _, "pq"),
    \+ sub_string(String, _, _, _, "xy").
