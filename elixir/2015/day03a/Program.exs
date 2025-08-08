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
        {_, positions} =
          content
          |> String.graphemes()
          |> Enum.reduce({{0, 0}, MapSet.new()}, fn
            dir, {{santa_x, santa_y}, positions} -> 
              new_santa =
                case dir do
                  "^" -> {santa_x, santa_y + 1}
                  "v" -> {santa_x, santa_y - 1}
                  "<" -> {santa_x - 1, santa_y}
                  ">" -> {santa_x + 1, santa_y}
                  _ -> {santa_x, santa_y}
                end
              new_positions = MapSet.put(positions, new_santa)
              {new_santa, new_positions}
          end)
        {:ok, MapSet.size(positions)}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
