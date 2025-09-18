#!/usr/bin/env swipl

:- use_module(library(readutil)).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Args),
    (   Args = [Filename] ->
        process(Filename, Result),
        format("result = ~w~n", [Result]),
        halt(0)
    ;   usage,
        halt(1)
    ).

process(Filename, Result) :-
    open(Filename, read, Stream),
    read_lines(Stream, 0, Result),
    close(Stream).

read_lines(Stream, Accum, Result) :-
    read_line_to_codes(Stream, Line),
    (  Line \= end_of_file
    -> length(Line, CodeLen),
       scan_line(Line, 0, EncLen),
       NewAccum is Accum + 2 + EncLen - CodeLen,
       read_lines(Stream, NewAccum, Result)
    ;  Result = Accum
    ).

scan_line([], Accum, Accum).
scan_line([Ch | Rest], Accum, FinalLen) :-
    (  member(Ch, [92, 34])
    -> NewAccum is Accum + 2,
       scan_line(Rest, NewAccum, FinalLen)
    ;  NewAccum is Accum + 1,
       scan_line(Rest, NewAccum, FinalLen)
    ).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").
