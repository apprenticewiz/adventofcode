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
        total_area =
          content
          |> String.split()
          |> Enum.reduce(0, fn
            line, acc ->
              parts = String.split(line, "x")
              dims = Enum.map(parts, fn x -> String.to_integer(x) end)
              l = Enum.at(dims, 0)
              w = Enum.at(dims, 1)
              h = Enum.at(dims, 2)
              area1 = l * w
              area2 = l * h
              area3 = w * h
              surface_area = 2 * area1 + 2 * area2 + 2 * area3
              min_area = min(area1, min(area2, area3))
              acc + surface_area + min_area
          end)
        {:ok, total_area}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
