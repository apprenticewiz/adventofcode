#!/usr/bin/env elixir

defmodule Program do
  def main(args) do
    case args do
      [key] ->
        result = process(key)
        IO.puts("result = #{result}")
      _ ->
        usage()
    end
  end

  defp usage do
    IO.puts(:stderr, "usage: Program.exs <input file>")
    System.halt(1)
  end

  defp process(key) do
     find_value(key, 1)
  end

  defp find_value(key, n) do
    try_key = key <> Integer.to_string(n)
    digest = :crypto.hash(:md5, try_key) |> Base.encode16(case: :lower)
    if String.starts_with?(digest, "000000") do
      n
    else
      find_value(key, n + 1)
    end
  end
end

Program.main(System.argv())
