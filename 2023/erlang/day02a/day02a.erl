-module (day02a).
-export ([main/1]).

total_red() -> 12.
total_green() -> 13.
total_blue() -> 14.

usage() ->
    Module = "day02a.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_lines([], Result) -> Result;
process_lines([Line | Rest], Result) ->
    {Valid, GameNum} = process_line(Line),
    case Valid of
        true -> process_lines(Rest, Result + GameNum);
        false -> process_lines(Rest, Result)
    end.

process_line(Line) ->
    GamePart = hd(string:split(Line, ": ")),
    GameNumPart = lists:last(string:split(GamePart, " ")),
    {GameNum, _} = string:to_integer(GameNumPart),
    DrawsPart = lists:last(string:split(Line, ": ")),
    Valid = lists:foldl(
        fun(Draws, CurrentValid) ->
            lists:foldl(
                fun(ColorAmount, DrawValid) ->
                    AmountStr = hd(string:split(ColorAmount, " ")),
                    Color = lists:last(string:split(ColorAmount, " ")),
                    {Amount, _} = string:to_integer(AmountStr),
                    case Color of
                        "red" -> DrawValid andalso (Amount =< total_red());
                        "green" -> DrawValid andalso (Amount =< total_green());
                        "blue" -> DrawValid andalso (Amount =< total_blue());
                        _ -> throw({error, "unexpected color"})
                    end
                end,
                CurrentValid, string:split(Draws, ", ", all)
            )
        end,
        true, string:split(DrawsPart, "; ", all)
    ),
    {Valid, GameNum}.

process_input(Contents) ->
    Trimmed = string:trim(Contents),
    Lines = string:split(Trimmed, "\n", all),
    process_lines(Lines, 0).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
