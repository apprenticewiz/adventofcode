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
        result =
          content
          |> String.graphemes()
          |> Enum.reduce_while({0, 0}, fn
            "(", {pos, floors} ->
              floors = floors + 1
              {:cont, {pos + 1, floors}}
            ")", {pos, floors} ->
              floors = floors - 1
              if floors < 0 do
                {:halt, {:found, pos + 1}}
              else
                {:cont, {pos + 1, floors}}
              end
            _, {pos, floors} ->
              {:cont, {pos + 1, floors}}
          end)
        case result do
          {:found, pos} -> {:ok, pos}
          _ -> {:error, "result was never negative"}
        end
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
