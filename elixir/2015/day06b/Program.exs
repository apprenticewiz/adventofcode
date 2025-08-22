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

  defp perform(grid, action, {r1, c1, r2, c2}) do
    Enum.reduce(for(row <- r1..r2, col <- c1..c2, do: {row, col}), grid, fn
      coord, curr_grid ->
        case action do
          "turn on" ->
            current = Map.get(curr_grid, coord, 0)
            Map.put(curr_grid, coord, current + 1)
          "turn off" ->
            current = Map.get(curr_grid, coord, 0)
            Map.put(curr_grid, coord, if current != 0 do current - 1 else 0 end)
          "toggle" ->
            current = Map.get(curr_grid, coord, 0)
            Map.put(curr_grid, coord, current + 2)
        end
    end)
  end

  defp process(filename) do
    case File.read(filename) do
      {:ok, content} ->
        re = ~r/(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/
        grid =
          content
          |> String.split(~r/\r?\n/)
          |> Enum.reduce(%{}, fn
            line, curr_grid ->
              case Regex.run(re, line) do
                [_, action, r1s, c1s, r2s, c2s] ->
                  r1 = String.to_integer(r1s)
                  c1 = String.to_integer(c1s)
                  r2 = String.to_integer(r2s)
                  c2 = String.to_integer(c2s)
                  perform(curr_grid, action, {r1, c1, r2, c2})
                _ ->
                  curr_grid
              end
          end)
        total = Enum.sum(Map.values(grid))
        {:ok, total}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
