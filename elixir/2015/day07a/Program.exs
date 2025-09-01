#!/usr/bin/env elixir

defmodule Program do
  import Bitwise

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

  defp is_integer?(str) do
    case Integer.parse(str) do
      {_, ""} -> true
      _ -> false
    end
  end

  defp eval(operations, cache, expr) do
    cond do
      is_integer?(expr) ->
        {int_val, _} = Integer.parse(expr)
        {int_val, cache}
      Map.has_key?(cache, expr) ->
        {Map.fetch!(cache, expr), cache}
      true ->
        {val, new_cache} = 
          case Map.fetch!(operations, expr) do
            {:assign, src} ->
              {val1, cache1} = eval(operations, cache, src)
              {val1 &&& 0xffff, cache1}
            {:not, src} ->
              {val1, cache1} = eval(operations, cache, src)
              {~~~val1 &&& 0xffff, cache1}
            {:and, src1, src2} ->
              {val1, cache1} = eval(operations, cache, src1)
              {val2, cache2} = eval(operations, cache1, src2)
              {val1 &&& val2 &&& 0xffff, cache2}
            {:or, src1, src2} ->
              {val1, cache1} = eval(operations, cache, src1)
              {val2, cache2} = eval(operations, cache1, src2)
              {val1 ||| val2 &&& 0xffff, cache2}
            {:lshift, src, amt} ->
              {val1, cache1} = eval(operations, cache, src)
              {(val1 <<< amt) &&& 0xffff, cache1}
            {:rshift, src, amt} ->
              {val1, cache1} = eval(operations, cache, src)
              {(val1 >>> amt) &&& 0xffff, cache1}
          end
        updated_cache = Map.put(new_cache, expr, val)
        {val, updated_cache}
    end
  end

  defp process(filename) do
    case File.read(filename) do
      {:ok, content} ->
        regexes = [
          {:assign, ~r/^(\d+|\w+) -> (\w+)$/},
          {:not, ~r/^NOT (\d+|\w+) -> (\w+)$/},
          {:and, ~r/(\d+|\w+) AND (\d+|\w+) -> (\w+)$/},
          {:or, ~r/(\d+|\w+) OR (\d+|\w+) -> (\w+)$/},
          {:lshift, ~r/(\d+|\w+) LSHIFT (\d+) -> (\w+)$/},
          {:rshift, ~r/(\d+|\w+) RSHIFT (\d+) -> (\w+)$/},
        ]
        operations =
          content
          |> String.split(~r/\r?\n/)
          |> Enum.reduce(%{}, fn line, curr_ops ->
            Enum.reduce_while(regexes, curr_ops, fn {op_type, re}, acc ->
              case Regex.run(re, line) do
                nil ->
                  {:cont, acc}
                [_, src, dest] when op_type == :assign or op_type == :not ->
                  {:halt, Map.put(acc, dest, {op_type, src})}
                [_, src1, src2, dest] when op_type in [:and, :or] ->
                  {:halt, Map.put(acc, dest, {op_type, src1, src2})}
                [_, src, amtStr, dest] when op_type in [:lshift, :rshift] ->
                  {amt, _} = Integer.parse(amtStr)
                  {:halt, Map.put(acc, dest, {op_type, src, amt})}
                _ ->
                  {:cont, acc}
              end
            end)
          end)
        {total, _} = eval(operations, %{}, "a")
        {:ok, total}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

Program.main(System.argv())
