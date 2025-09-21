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

scan_line(Line, I) ->
  case Line of
    [] -> I;
    [Ch | Rest] ->
      case Ch of
        34 -> scan_line(Rest, I + 2);
        92 -> scan_line(Rest, I + 2);
        _ -> scan_line(Rest, I + 1)
      end
  end.

process(Filename) ->
  case file:read_file(Filename) of
    {ok, Chars} ->
      Lines = string:lexemes(Chars, "\n"),
      Result = lists:foldl(
        fun (Line, Acc) ->
          CodeLen = string:length(Line),
          EncLen = scan_line(string:to_graphemes(Line), 0),
          Acc + 2 + (EncLen - CodeLen)
        end,
        0,
        Lines),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
