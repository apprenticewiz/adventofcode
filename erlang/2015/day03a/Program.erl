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
      {_, Positions} = lists:foldl(
        fun
          (Ch, {{SantaX, SantaY}, Positions}) ->
            {NewSantaX, NewSantaY} =
              case Ch of
                $^ -> {SantaX, SantaY + 1};
                $v -> {SantaX, SantaY - 1};
                $< -> {SantaX - 1, SantaY};
                $> -> {SantaX + 1, SantaY};
                _ -> {SantaX, SantaY}
            end,
            NewPositions = sets:add_element({NewSantaX, NewSantaY}, Positions),
	    {{NewSantaX, NewSantaY}, NewPositions}
        end,
	{{0, 0}, sets:new()},
        Content),
      {ok, sets:size(Positions)};
    {error, Reason} ->
      {error, Reason}
  end.
