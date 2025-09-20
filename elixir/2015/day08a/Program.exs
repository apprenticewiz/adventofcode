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
              code_len = String.length(line)
              quoted = String.slice(line, 1..(code_len - 2))
              mem_len = scan_quoted(0, quoted)
              acc + (code_len - mem_len)
          end)
        {:ok, total_area}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp scan_quoted(i, quoted) do
    quoted_len = String.length(quoted)
    if quoted_len == 0 do
      i
    else
      case String.at(quoted, 0) do
        "\\" ->
          case String.at(quoted, 1) do
            "\\" -> scan_quoted(i + 1, String.slice(quoted, 2..quoted_len))
            "\"" -> scan_quoted(i + 1, String.slice(quoted, 2..quoted_len))
            "x" -> scan_quoted(i + 1, String.slice(quoted, 4..quoted_len))
            _ -> scan_quoted(i + 1, String.slice(quoted, 1..quoted_len))
          end
        _ -> scan_quoted(i + 1, String.slice(quoted, 1..quoted_len))
      end
    end
  end
end

Program.main(System.argv())
