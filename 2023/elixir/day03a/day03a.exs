defmodule Day03a do
  @progname "day03a.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def build_numbers(contents) do
    contents |> String.split("\n", trim: true)
    |> Enum.reduce({0, %{}}, fn (line, {row, number_locs}) ->
      {row + 1, Enum.reduce(String.graphemes(line), {0, false, "", {-1, -1}, number_locs}, fn (ch, {col, scanning_number, number, current_pos, inner_number_locs}) ->
        if scanning_number do
          if String.match?(ch, ~r/\d/) do
            if col + 1 == String.length(line) do
              {col + 1, false, "",  {-1, -1}, Map.put(inner_number_locs, current_pos, number <> ch)}
            else
              {col + 1, true, number <> ch, current_pos, inner_number_locs}
            end
          else
            {col + 1, false, "", {-1, -1}, Map.put(inner_number_locs, current_pos, number)}
          end
        else
          if String.match?(ch, ~r/\d/) do
            {col + 1, true, ch, {row, col}, inner_number_locs}
          else
            {col + 1, false, "", current_pos, inner_number_locs}
          end
        end
      end)
      |> elem(4)}
    end)
    |> elem(1)
  end

  def build_gears(contents) do
    contents |> String.split("\n", trim: true)
    |> Enum.reduce({0, %{}}, fn (line, {row, gear_locs}) ->
      {row + 1, Enum.reduce(String.graphemes(line), {0, gear_locs}, fn (ch, {col, inner_gear_locs}) ->
        if !String.match?(ch, ~r/\d/) && ch != "." do
          {col + 1, Map.put(inner_gear_locs, {row, col}, ch)}
        else
          {col + 1, inner_gear_locs}
        end
      end)
      |> elem(1)}
    end)
    |> elem(1)
  end

  def check_gears(number_locs, gear_locs) do
    Map.keys(number_locs) |> Enum.reduce(0, fn (number_loc, result) ->
      number_col_begin = elem(number_loc, 1)
      number_col_end = elem(number_loc, 1) + String.length(Map.get(number_locs, number_loc)) - 1
      found_adjacent = number_col_begin..number_col_end |> Enum.reduce_while(false, fn (number_col, false) ->
        found_adjacent = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}] |> Enum.reduce_while(false, fn (neighbor, _) ->
          {adjacent_row, adjacent_col} = {elem(number_loc, 0) + elem(neighbor, 0), number_col + elem(neighbor, 1)}
          found_adjacent = Map.keys(gear_locs) |> Enum.reduce_while(false, fn (gear_loc, _) ->
            if adjacent_row == elem(gear_loc, 0) && adjacent_col == elem(gear_loc, 1) do
              {:halt,  true}
            else
              {:cont, false}
            end
          end)
          if found_adjacent do
            {:halt, true}
          else
            {:cont, false}
          end
        end)
        if found_adjacent do
          {:halt, true}
        else
          {:cont, false}
        end
      end)
      if found_adjacent do
        result + String.to_integer(Map.get(number_locs, number_loc))
      else
        result
      end
    end)
  end

  def process(contents) do
    number_locs = build_numbers(contents)
    gear_locs = build_gears(contents)
    check_gears(number_locs, gear_locs)
  end

  def main do
    if length(System.argv()) < 1 do
      usage()
    end

    filename = hd(System.argv())
    {_, contents} = File.read(filename)
    result = process(contents)
    IO.puts("result = #{result}")
  end

end

Day03a.main()
