defmodule Day01a do
  @progname "day01a.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    Enum.reduce(lines, 0, fn line, result ->
      {_min_index, left_digit} =
        Enum.reduce_while(digits, {nil, nil}, fn digit, {min_val, left} ->
          if String.contains?(line, digit) do
            {:ok, pattern} = Regex.compile(digit)
            {left_index, _} = hd(hd(Regex.scan(pattern, line, return: :index)))
            case min_val do
              nil -> {:cont, {left_index, digit}}
              _ when left_index < min_val -> {:cont, {left_index, digit}}
              _ -> {:cont, {min_val, left}}
            end
          else
            {:cont, {min_val, left}}
          end
        end)
      {_max_index, right_digit} =
        Enum.reduce_while(digits, {nil, nil}, fn digit, {max_val, right} ->
          if String.contains?(line, digit) do
            {:ok, pattern} = Regex.compile(digit)
            {right_index, _} = hd(List.last(Regex.scan(pattern, line, return: :index)))
            case max_val do
              nil -> {:cont, {right_index, digit}}
              _ when right_index > max_val -> {:cont, {right_index, digit}}
              _ -> {:cont, {max_val, right}}
            end
          else
            {:cont, {max_val, right}}
          end
        end)
        result + String.to_integer(left_digit) * 10 + String.to_integer(right_digit)
      end)
  end

  def main do
    if length(System.argv()) < 1 do
      usage()
    end

    filename = hd(System.argv())
    {_, contents} = File.read(filename)
    result = process(contents)
    IO.puts("result = #{result}")
  end
end

Day01a.main()
