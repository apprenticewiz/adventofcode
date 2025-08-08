#!/usr/bin/env elixir

defmodule Program do
  def main(args) do
    case args do
      [filename] ->
        case process(filename) do
          {:ok, result} ->
            IO.puts("result = #{result}")
          {:error, reason} ->
            IO.puts(:stderr, "error while processing file `#{filename}': #{reason}")
            System.halt(1)
        end
      _ ->
        usage()
    end
  end

  defp usage do
    IO.puts(:stderr, "usage: Program.exs <input file>")
    System.halt(1)
  end

  defp process(filename) do
    case File.read(filename) do
      {:ok, content} ->
        {_, _, _, positions} =
          content
          |> String.graphemes()
          |> Enum.reduce({{0, 0}, {0, 0}, true, MapSet.new()}, fn
            dir, {{santa_x, santa_y}, {robo_santa_x, robo_santa_y}, santa_move, positions} -> 
              {new_santa, new_robo_santa} =
                case santa_move do
                  true ->
                    new_pos =
                      case dir do
                        "^" -> {santa_x, santa_y + 1}
                        "v" -> {santa_x, santa_y - 1}
                        "<" -> {santa_x - 1, santa_y}
                        ">" -> {santa_x + 1, santa_y}
                        _ -> {santa_x, santa_y}
                      end
                      {new_pos, {robo_santa_x, robo_santa_y}}
                  false ->
                    new_pos =
                      case dir do
                        "^" -> {robo_santa_x, robo_santa_y + 1}
                        "v" -> {robo_santa_x, robo_santa_y - 1}
                        "<" -> {robo_santa_x - 1, robo_santa_y}
                        ">" -> {robo_santa_x + 1, robo_santa_y}
                        _ -> {robo_santa_x, robo_santa_y}
                      end
                      {{santa_x, santa_y}, new_pos}
                end
              new_positions =
                case santa_move do
                  true -> MapSet.put(positions, new_santa)
                  false -> MapSet.put(positions, new_robo_santa)
                end
              {new_santa, new_robo_santa, !santa_move, new_positions}
          end)
        {:ok, MapSet.size(positions)}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
