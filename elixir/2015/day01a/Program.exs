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
        floors =
          content
          |> String.graphemes()
          |> Enum.reduce(0, fn
            "(", acc -> acc + 1
            ")", acc -> acc - 1
            _, acc -> acc
          end)

        {:ok, floors}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
