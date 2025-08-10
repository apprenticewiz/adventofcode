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
            case string:lexemes(Line, "x") of
              [LStr, WStr, HStr] ->
                {L, _} = string:to_integer(LStr),
		{W, _} = string:to_integer(WStr),
                {H, _} = string:to_integer(HStr),
                Area1 = L * W,
                Area2 = L * H,
                Area3 = W * H,
                SurfaceArea = 2 * Area1 + 2 * Area2 + 2 * Area3,
                MinArea = lists:min([Area1, Area2, Area3]),
                Acc + SurfaceArea + MinArea;
              _ ->
                Acc
            end
        end,
        0,
        Lines),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
