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

scan_quoted(Quoted, I) ->
  case Quoted of
    [] -> I;
    [_] -> I + 1;
    [Ch1, Ch2 | Rest] ->
      case {Ch1, Ch2} of
        {92, 34} -> scan_quoted(Rest, I + 1);
        {92, 92} -> scan_quoted(Rest, I + 1);
        {92, 120} -> scan_quoted(tl(tl(Rest)), I + 1);
        {_, _} -> scan_quoted([Ch2 | Rest], I + 1)
      end
  end.

process(Filename) ->
  case file:read_file(Filename) of
    {ok, Chars} ->
      Lines = string:lexemes(Chars, "\n"),
      Result = lists:foldl(
        fun (Line, Acc) ->
          CodeLen = string:length(Line),
          Quoted = string:slice(Line, 1, string:length(Line) - 2),
          MemLen = scan_quoted(string:to_graphemes(Quoted), 0),
          Acc + CodeLen - MemLen
        end,
        0,
        Lines),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.
