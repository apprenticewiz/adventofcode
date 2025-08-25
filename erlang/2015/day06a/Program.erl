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

perform(Grid, Action, R1, C1, R2, C2) ->
  Indices = [{Rows, Cols} || Rows <- lists:seq(R1, R2), Cols <- lists:seq(C1, C2)],
  lists:foldl(
    fun ({Row, Col}, CurrGrid) ->
      OldValue = maps:get({Row, Col}, CurrGrid, 0),
      case Action of
        "turn on" -> maps:put({Row, Col}, 1, CurrGrid);
        "turn off" -> maps:put({Row, Col}, 0, CurrGrid);
        "toggle" -> maps:put({Row, Col}, 1 - OldValue, CurrGrid)
      end
    end,
    Grid,
    Indices).

process(Filename) ->
  case file:read_file(Filename) of
    {ok, Chars} ->
      Lines = string:lexemes(Chars, "\n"),
      Grid = lists:foldl(
        fun
          (Line, CurrGrid) ->
            case re:run(Line, "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)",
                        [unicode, {capture, all_but_first, list}]) of
              {match, [Action, R1S, C1S, R2S, C2S]} ->
                perform(CurrGrid, Action, list_to_integer(R1S), list_to_integer(C1S),
                list_to_integer(R2S), list_to_integer(C2S));
              nomatch -> skip
            end
        end,
	#{},
        Lines),
      Result = maps:fold(fun (_, Val, Acc) -> Acc + Val end, 0, Grid),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
