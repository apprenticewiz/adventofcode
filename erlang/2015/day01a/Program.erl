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
      Content = unicode:characters_to_list(Chars),
      Result = lists:foldl(
        fun
          ($(, Acc) -> Acc + 1;
          ($), Acc) -> Acc - 1;
          (_, Acc) -> Acc
        end,
        0,
        Content),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
