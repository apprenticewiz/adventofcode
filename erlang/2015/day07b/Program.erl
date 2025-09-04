#!/usr/bin/env escript

main(Args) ->
  case Args of
    [Filename] ->
      case process(Filename) of
        {ok, Result} ->
          io:format("result = ~p~n", [Result]);
        {error, Reason} ->
          io:format(standard_error, "error while processing file `~s': ~p~n", [Filename, Reason]),
          halt(1)
      end;
    _ ->
      usage()
  end.

usage() ->
  io:format(standard_error, "usage: Program.erl <input file>~n", []),
  halt(1).

read_lines(FileHandle, Lines) ->
  case file:read_line(FileHandle) of
    eof -> {ok, Lines};
    {ok, Line} -> read_lines(FileHandle, Lines ++ [string:trim(Line)]);
    {error, Reason} -> {error, Reason}
  end.

process(Filename) ->
  case file:open(Filename, [read]) of
    {ok, FileHandle} ->
      case read_lines(FileHandle, []) of
        {ok, Lines} ->
          Operations = lists:foldl(
            fun(Line, Acc) ->
              case re:run(Line, "^(\\d+|\\w+) -> (\\d+|\\w+)$", [unicode, {capture, all_but_first, list}]) of
                {match, [Src, Dest]} -> maps:put(Dest, {assign_op, Src}, Acc);
                nomatch ->
                  case re:run(Line, "NOT (\\d+|\\w+) -> (\\d+|\\w+)", [unicode, {capture, all_but_first, list}]) of
                    {match, [Src, Dest]} -> maps:put(Dest, {not_op, Src}, Acc);
                    nomatch ->
                      case re:run(Line, "(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\d+|\\w+)",
                                  [unicode, {capture, all_but_first, list}]) of
                        {match, [Src1, "AND", Src2, Dest]} -> maps:put(Dest, {and_op, Src1, Src2}, Acc);
                        {match, [Src1, "OR", Src2, Dest]} -> maps:put(Dest, {or_op, Src1, Src2}, Acc);
                        nomatch ->
                          case re:run(Line, "(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\d+|\\w+)",
                                 [unicode, {capture, all_but_first, list}]) of
                            {match, [Src, "LSHIFT", Amt, Dest]} ->
                               {N, _} = string:to_integer(Amt),
                               maps:put(Dest, {lshift_op, Src, N}, Acc);
                            {match, [Src, "RSHIFT", Amt, Dest]} ->
                               {N, _} = string:to_integer(Amt),
                               maps:put(Dest, {rshift_op, Src, N}, Acc);
                            nomatch ->
                                io:format(standard_error, "error: malformed input line: ~s~n", [Line]),
                                halt(1)
                          end
                      end
                  end
              end
            end,
            #{},
            Lines),
          {A, _} = eval(Operations, #{}, "a"),
          NewOps = maps:put("b", {assign_op, integer_to_list(A)}, Operations),
          {Result, _} = eval(NewOps, #{}, "a"),
          {ok, Result};
        {error, Reason} ->
          {error, Reason}
        end;
    {error, Reason} ->
      {error, Reason}
  end.

eval(Operations, Cache, Expr) ->
  case string:to_integer(Expr) of
    {error, no_integer} ->
      case maps:get(Expr, Cache, undefined) of
        undefined ->
          Operation = maps:get(Expr, Operations),
          case Operation of
            {assign_op, Src} ->
              {A, C} = eval(Operations, Cache, Src),
              {A, maps:put(Expr, A, C)};
            {not_op, Src} ->
              {A, C} = eval(Operations, Cache, Src),
              Res = (bnot A) band 65535,
              {Res, maps:put(Expr, Res, C)};
            {and_op, Src1, Src2} ->
              {A, C1} = eval(Operations, Cache, Src1),
              {B, C2} = eval(Operations, C1, Src2),
              Res = (A band B) band 65535,
              {Res, maps:put(Expr, Res, C2)};
            {or_op, Src1, Src2} ->
              {A, C1} = eval(Operations, Cache, Src1),
              {B, C2} = eval(Operations, C1, Src2),
              Res = (A bor B) band 65535,
              {Res, maps:put(Expr, Res, C2)};
            {lshift_op, Src, Amt} ->
              {A, C} = eval(Operations, Cache, Src),
              Res = (A bsl Amt) band 65535,
              {Res, maps:put(Expr, Res, C)};
            {rshift_op, Src, Amt} ->
              {A, C} = eval(Operations, Cache, Src),
              Res = (A bsr Amt) band 65535,
              {Res, maps:put(Expr, Res, C)}
          end;
        X -> {X, Cache}
      end;
    {N, _} -> {N, Cache}
  end.
