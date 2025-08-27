#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).

:- dynamic lights_on/2.

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
    retractall(lights(_, _)),
    split_string(Content, "\n", "\r", Lines),
    maplist(process_line, Lines),
    findall((Row, Col), lights_on(Row, Col), Lights),
    length(Lights, Result).

process_line(Line) :-
    Line \= "",
    string_codes(Line, LineCodes),
    phrase(instruction(Command, R1, C1, R2, C2), LineCodes),
    perform(Command, R1, C1, R2, C2).
process_line("") :- !.

instruction(turn_on, R1, C1, R2, C2) --> "turn on ", coords(R1, C1), " through ", coords(R2, C2).
instruction(turn_off, R1, C1, R2, C2) --> "turn off ", coords(R1, C1), " through ", coords(R2, C2).
instruction(toggle, R1, C1, R2, C2) --> "toggle ", coords(R1, C1), " through ", coords(R2, C2).

coords(R, C) --> integer(R), ",", integer(C).

integer(N) --> digit(D), digits(Ds), { number_codes(N, [D|Ds]) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].
digit(D) --> [D], { code_type(D, digit) }.

perform(Command, R1, C1, R2, C2) :-
    between(R1, R2, Row),
    between(C1, C2, Col),
    apply(Command, Row, Col),
    fail.
perform(_, _, _, _, _).

apply(turn_on, Row, Col) :-
    (lights_on(Row, Col) -> true ; assertz(lights_on(Row, Col))).
apply(turn_off, Row, Col) :-
    (retract(lights_on(Row, Col)) -> true ; true).
apply(toggle, Row, Col) :-
    (retract(lights_on(Row, Col)) -> true ; assertz(lights_on(Row, Col))).
