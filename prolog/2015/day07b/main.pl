#!/usr/bin/env swipl

:- use_module(library(readutil)).
:- use_module(library(lists)).

:- dynamic op/2.
:- dynamic cache/2.

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
    format(user_error, "usage: main.pl <input file>~n", []).

process(Content, Result) :-
    retractall(op(_, _)),
    retractall(cache(_, _)),
    split_string(Content, "\n", "\r", Lines),
    maplist(parse_line, Lines),
    evaluate("a", A),
    retractall(op("b", _)),
    assertz(op("b", op_assign(A))),
    retractall(cache(_, _)),
    evaluate("a", Result).

parse_line(Line) :-
    Line = "", !.

parse_line(Line) :-
    split_string(Line, " ", "", Parts),
    parse_to_op(Parts, OpTerm),
    assertz(OpTerm).

parse_to_op([Src, "->", Dest], op(Dest, op_assign(Src))).
parse_to_op(["NOT", Src, "->", Dest], op(Dest, op_not(Src))).
parse_to_op([Src1, "AND", Src2, "->", Dest], op(Dest, op_and(Src1, Src2))).
parse_to_op([Src1, "OR", Src2, "->", Dest], op(Dest, op_or(Src1, Src2))).
parse_to_op([Src, "LSHIFT", Amount, "->", Dest], op(Dest, op_lshift(Src, N))) :- number_string(N, Amount).
parse_to_op([Src, "RSHIFT", Amount, "->", Dest], op(Dest, op_rshift(Src, N))) :- number_string(N, Amount).

evaluate(Value, Value) :- integer(Value), !.

evaluate(Expr, Value) :-
    number_string(Value, Expr), !.

evaluate(Expr, Value) :-
    cache(Expr, Value), !.

evaluate(Expr, Value) :-
    op(Expr, Op),
    eval_op(Op, Value),
    assertz(cache(Expr, Value)).

eval_op(op_assign(Src), Value) :-
    evaluate(Src, Value).

eval_op(op_not(Src), Value) :-
    evaluate(Src, V),
    Value is (\V) /\ 0xffff.

eval_op(op_and(Src1, Src2), Value) :-
    evaluate(Src1, V1),
    evaluate(Src2, V2),
    Value is (V1 /\ V2) /\ 0xffff.

eval_op(op_or(Src1, Src2), Value) :-
    evaluate(Src1, V1),
    evaluate(Src2, V2),
    Value is (V1 \/ V2) /\ 0xffff.

eval_op(op_lshift(Src, N), Value) :-
    evaluate(Src, V),
    Value is (V << N) /\ 0xffff.

eval_op(op_rshift(Src, N), Value) :-
    evaluate(Src, V),
    Value is (V >> N) /\ 0xffff.
