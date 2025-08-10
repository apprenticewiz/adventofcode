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
      {_, _, _, Positions} = lists:foldl(
        fun
          (Ch, {{SantaX, SantaY}, RoboSanta, true, Positions}) ->
            {NewSantaX, NewSantaY} =
              case Ch of
                $^ -> {SantaX, SantaY + 1};
                $v -> {SantaX, SantaY - 1};
                $< -> {SantaX - 1, SantaY};
                $> -> {SantaX + 1, SantaY};
                _ -> {SantaX, SantaY}
            end,
            NewPositions = sets:add_element({NewSantaX, NewSantaY}, Positions),
	    {{NewSantaX, NewSantaY}, RoboSanta, false, NewPositions};
          (Ch, {Santa, {RoboSantaX, RoboSantaY}, false, Positions}) ->
            {NewRoboSantaX, NewRoboSantaY} =
              case Ch of
                $^ -> {RoboSantaX, RoboSantaY + 1};
                $v -> {RoboSantaX, RoboSantaY - 1};
                $< -> {RoboSantaX - 1, RoboSantaY};
                $> -> {RoboSantaX + 1, RoboSantaY};
                _ -> {RoboSantaX, RoboSantaY}
            end,
            NewPositions = sets:add_element({NewRoboSantaX, NewRoboSantaY}, Positions),
	    {Santa, {NewRoboSantaX, NewRoboSantaY}, true, NewPositions}
        end,
	{{0, 0}, {0, 0}, true, sets:new()},
        Content),
      {ok, sets:size(Positions)};
    {error, Reason} ->
      {error, Reason}
  end.
