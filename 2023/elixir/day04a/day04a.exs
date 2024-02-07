import Bitwise

defmodule Day04a do
  @progname "day04a.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    Enum.reduce(lines, 0, fn line, result ->
        rest = hd(tl(String.split(line, ~r{:\s+})))
        winning_str = hd(String.split(rest, ~r{\s+\|\s+}))
        winning_set = Enum.reduce(winning_str |> String.split(~r{\s+}) |> Enum.map(&String.to_integer/1),
                                  MapSet.new(),
                                  fn num, prev_set ->
                                      MapSet.put(prev_set, num)
                                  end)
        hand_str = hd(tl(String.split(rest, ~r{\s+\|\s+})))
        hand_set = Enum.reduce(hand_str |> String.split(~r{\s+}) |> Enum.map(&String.to_integer/1),
                                  MapSet.new(),
                                  fn num, prev_set ->
                                      MapSet.put(prev_set, num)
                                  end)
        count = MapSet.intersection(winning_set, hand_set) |> MapSet.size
        if count > 0 do
          result + (1 <<< (count - 1))
        else
          result
        end
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

Day04a.main()
