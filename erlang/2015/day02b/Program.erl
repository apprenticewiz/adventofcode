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
                Perim1 = 2 * (L + W),
                Perim2 = 2 * (L + H),
                Perim3 = 2 * (W + H),
                PresentLen = lists:min([Perim1, Perim2, Perim3]),
                BowLen = L * W * H,
                Acc + PresentLen + BowLen;
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
