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
      case find_basement(Content, 1, 0) of
        {found, Pos} -> {ok, Pos};
        not_found -> {error, "result was never negative"}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

find_basement([], _, _) ->
  not_found;
find_basement([$( | Rest], Pos, Floor) ->
  find_basement(Rest, Pos + 1, Floor + 1);
find_basement([$) | Rest], Pos, Floor) ->
  NewFloor = Floor - 1,
  if
    NewFloor < 0 -> {found, Pos};
    true -> find_basement(Rest, Pos + 1, NewFloor)
  end;
find_basement([_ | Rest], Pos, Floor) ->
  find_basement(Rest, Pos + 1, Floor).
