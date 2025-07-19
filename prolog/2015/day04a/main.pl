#!/usr/bin/env swipl

:- use_module(library(crypto)).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Args),
    (   Args = [Key] ->
        process(Key, Result),
        format("result = ~w~n", [Result]),
        halt(0)
    ;   usage,
        halt(1)
    ).

usage :-
    tell(user_error),
    format("usage: main.pl <input file>~n").

process(Key, N) :-
    between(1, inf, N),
    string_concat(Key, N, TryKey),
    crypto_data_hash(TryKey, Hash, [algorithm(md5), encoding(utf8)]),
    sub_string(Hash, 0, 5, _, "00000"),
    !.
