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
              if prop1(line) && prop2(line) do
                acc + 1
              else
                acc
              end
          end)
        {:ok, total_area}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp prop1(str) do
    Enum.count(Regex.scan(~r/(..).*\1/, str)) > 0
  end

  defp prop2(str) do
    Enum.count(Regex.scan(~r/(.).\1/, str)) > 0
  end
end

Program.main(System.argv())
