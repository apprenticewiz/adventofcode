-module (day02b).
-export ([main/1]).

usage() ->
    Module = "day02b.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_lines([], Result) -> Result;
process_lines([Line | Rest], Result) ->
    {RedNeeded, GreenNeeded, BlueNeeded} = process_line(Line),
    process_lines(Rest, Result + RedNeeded * GreenNeeded * BlueNeeded).

process_line(Line) ->
    DrawsPart = lists:last(string:split(Line, ": ")),
    {RedNeeded, GreenNeeded, BlueNeeded} = lists:foldl(
        fun(Draws, {CurrentRed, CurrentGreen, CurrentBlue}) ->
            lists:foldl(
                fun(ColorAmount, {RedNeeded, GreenNeeded, BlueNeeded}) ->
                    [AmountStr | _] = string:split(ColorAmount, " "),
                    Color = lists:last(string:split(ColorAmount, " ")),
                    {Amount, _} = string:to_integer(AmountStr),
                    case Color of
                        "red" ->
                            if Amount > RedNeeded ->
                                {Amount, GreenNeeded, BlueNeeded};
                            true ->
                                {RedNeeded, GreenNeeded, BlueNeeded}
                            end;
                        "green" ->
                            if Amount > GreenNeeded ->
                                {RedNeeded, Amount, BlueNeeded};
                            true ->
                                {RedNeeded, GreenNeeded, BlueNeeded}
                            end;
                        "blue" ->
                            if Amount > BlueNeeded ->
                                {RedNeeded, GreenNeeded, Amount};
                            true ->
                                {RedNeeded, GreenNeeded, BlueNeeded}
                            end;
                        _ -> throw({error, "unexpected color"})
                    end
                end,
                {CurrentRed, CurrentGreen, CurrentBlue}, string:split(Draws, ", ", all)
            )
        end,
        {0, 0, 0}, string:split(DrawsPart, "; ", all)
    ),
    {RedNeeded, GreenNeeded, BlueNeeded}.

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
