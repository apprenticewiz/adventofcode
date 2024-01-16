defmodule Day02a do
  @progname "day02a.exs"

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    Enum.reduce(lines, 0, fn line, result ->
      draws_part = hd(tl(String.split(line, ": ")))
      {red_needed, green_needed, blue_needed} = Enum.reduce(String.split(draws_part, "; "), {0, 0, 0}, fn draws, {red, green, blue} ->
        Enum.reduce(String.split(draws, ", "), {red, green, blue}, fn draw, {current_red, current_green, current_blue} ->
          amount = String.to_integer(hd(String.split(draw, " ")))
          color = hd(tl(String.split(draw, " ")))
          case color do
            "red" ->
              if amount > current_red do
                {amount, current_green, current_blue}
              else
                {current_red, current_green, current_blue}
              end
            "green" ->
              if amount > current_green do
                {current_red, amount, current_blue}
              else
                {current_red, current_green, current_blue}
              end
            "blue" ->
              if amount > current_blue do
                {current_red, current_green, amount}
              else
                {current_red, current_green, current_blue}
              end
          end
        end)
      end)
      result + red_needed * green_needed * blue_needed
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

Day02a.main()
