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
        total_len =
          content
          |> String.split()
          |> Enum.reduce(0, fn
            line, acc ->
              parts = String.split(line, "x")
              dims = Enum.map(parts, fn x -> String.to_integer(x) end)
              l = Enum.at(dims, 0)
              w = Enum.at(dims, 1)
              h = Enum.at(dims, 2)
              perim1 = 2 * (l + w)
              perim2 = 2 * (l + h)
              perim3 = 2 * (w + h)
              present_len = min(perim1, min(perim2, perim3))
              bow_len = l * w * h
              acc + present_len + bow_len
          end)
        {:ok, total_len}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
