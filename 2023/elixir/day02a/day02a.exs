defmodule Day02a do
  @progname "day02a.exs"

  @total_red 12
  @total_green 13
  @total_blue 14

  def usage do
    IO.puts("usage: #{@progname} <file>")
    System.halt(1)
  end

  def process(contents) do
    lines = String.split(contents, "\n", trim: true)
    Enum.reduce(lines, 0, fn line, result ->
      game_part = hd(String.split(line, ": "))
      game_num = String.to_integer(hd(tl(String.split(game_part, " "))))
      draws_part = hd(tl(String.split(line, ": ")))
      valid = Enum.reduce(String.split(draws_part, "; "), true, fn draws, current_valid ->
        Enum.reduce(String.split(draws, ", "), current_valid, fn draw, draw_valid ->
          amount = String.to_integer(hd(String.split(draw, " ")))
          color = hd(tl(String.split(draw, " ")))
          case color do
            "red" -> draw_valid and (amount <= @total_red)
            "green" -> draw_valid and (amount <= @total_green)
            "blue" -> draw_valid and (amount <= @total_blue)
          end
        end)
      end)
      if valid do
        result + game_num
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

Day02a.main()
