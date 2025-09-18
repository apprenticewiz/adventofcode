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
       scan_line(Line, MemLen),
       NewAccum is Accum + CodeLen - MemLen,
       read_lines(Stream, NewAccum, Result)
    ;  Result = Accum
    ).

scan_line(Line, MemLen) :-
    quoted(Line, Quoted),
    count_escaped(0, Quoted, MemLen).

quoted([_ | Rest], Middle) :-
     append(Middle, [_], Rest).

count_escaped(Count, [], Count).
count_escaped(Count, [_], FinalCount) :-
    FinalCount is Count + 1.
count_escaped(Count, [92,  Ch | Rest], FinalCount) :-
    (  member(Ch, [92, 34]) ->
       NextCount is Count + 1,
       count_escaped(NextCount, Rest, FinalCount)
    ;  Ch = 120 ->
       Rest = [_, _ | NewRest],
       NextCount is Count + 1,
       count_escaped(NextCount, NewRest, FinalCount)
    ;  NextCount is Count + 1,
       count_escaped(NextCount, Rest, FinalCount)
    ).
count_escaped(Count, [_ | Rest], FinalCount) :-
    NextCount is Count + 1,
    count_escaped(NextCount, Rest, FinalCount).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").
