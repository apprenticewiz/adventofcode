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

process(Filename) ->
  case file:read_file(Filename) of
    {ok, Chars} ->
      Lines = string:lexemes(Chars, "\n"),
      Result = lists:foldl(
        fun
          (Line, Acc) ->
            case prop1(Line) andalso prop2(Line) of
              true -> Acc + 1;
              false -> Acc
            end
        end,
        0,
        Lines),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.

prop1(Str) ->
  case re:run(Str, "(..).*\\1", [global, {capture, none}]) of
    nomatch -> false;
    _ -> true
  end.

prop2(Str) ->
  case re:run(Str, "(.).\\1", [global, {capture, none}]) of
    nomatch -> false;
    _ -> true
  end.
