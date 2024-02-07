defmodule Day04b do
  @progname "day04b.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    instances = Enum.reduce(lines, %{}, fn line, prev_instances ->
        card_part = hd(String.split(line, ~r{:\s+}))
        card_num_part = hd(tl(String.split(card_part, ~r{\s+})))
        card_num = String.to_integer(card_num_part)
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
        if (card_num + 1) <= (card_num + count) do
          Enum.reduce((card_num + 1)..(card_num + count), prev_instances, fn i, prev_inst ->
            copies = Map.get(prev_inst, i, 0) + 1 + Map.get(prev_inst, card_num, 0)
            Map.put(prev_inst, i, copies)
          end)
        else
          prev_instances
        end
    end)
    Enum.reduce(Map.values(instances), 0, fn n, acc -> acc + n end) + length(lines)
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

Day04b.main()
