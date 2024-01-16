defmodule Day01b do
  @progname "day01b.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    digits_map = %{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5,
      "6" => 6 , "7" => 7, "8" => 8, "9" => 9, "zero" => 0, "one" => 1,
      "two" => 2, "three" => 3, "four" => 4, "five" => 5, "six" => 6,
      "seven" => 7, "eight" => 8, "nine" => 9
    }
    Enum.reduce(lines, 0, fn line, result ->
      {_min_index, left_digit} =
        Enum.reduce_while(Map.keys(digits_map), {nil, nil}, fn digit, {min_val, left} ->
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
        Enum.reduce_while(Map.keys(digits_map), {nil, nil}, fn digit, {max_val, right} ->
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
        result + Map.get(digits_map, left_digit) * 10 + Map.get(digits_map, right_digit)
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

Day01b.main()
