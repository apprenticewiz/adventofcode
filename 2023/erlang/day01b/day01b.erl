-module (day01b).
-export ([main/1]).

usage() ->
    Module = "day01b.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_lines([], _, Result) -> Result;
process_lines([Line | Rest], DigitsMap, Result) ->
    LineResult = process_line(Line, DigitsMap),
    process_lines(Rest, DigitsMap, Result + LineResult).

process_line(Line, DigitsMap) ->
    {_MinIndex, LeftDigit} = lists:foldl(
        fun(Digit, {CurrentMinIndex, CurrentLeftDigit}) ->
	    case string:find(Line, Digit) of
                nomatch ->
                    {CurrentMinIndex, CurrentLeftDigit};
                _ ->
                    LeftIndex = string:str(Line, Digit),
                    case CurrentMinIndex of
                        undefined ->
                            {LeftIndex, Digit};
                        _ ->
                            if LeftIndex < CurrentMinIndex ->
                                 {LeftIndex, Digit};
                            true ->
                                 {CurrentMinIndex, CurrentLeftDigit}
                            end
                    end
            end
        end,
        {undefined, undefined}, proplists:get_keys(DigitsMap)
    ),
    {_MaxIndex, RightDigit} = lists:foldl(
        fun(Digit, {CurrentMaxIndex, CurrentRightDigit}) ->
	    case string:find(Line, Digit) of
                nomatch ->
                    {CurrentMaxIndex, CurrentRightDigit};
		_ ->
                    RightIndex = string:rstr(Line, Digit),
                    case CurrentMaxIndex of
                        undefined ->
                            {RightIndex, Digit};
                        _ ->
                            if RightIndex > CurrentMaxIndex ->
                                {RightIndex, Digit};
                            true ->
                                {CurrentMaxIndex, CurrentRightDigit}
                            end
                    end
            end
        end,
        {undefined, undefined}, proplists:get_keys(DigitsMap)
    ),
    LeftValue = proplists:get_value(LeftDigit, DigitsMap),
    RightValue = proplists:get_value(RightDigit, DigitsMap),
    (LeftValue * 10) + RightValue.

process_input(Contents) ->
    Trimmed = string:trim(Contents),
    Lines = string:split(Trimmed, "\n", all),
    DigitsMap = [{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5},
        {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9}, {"zero", 0}, {"one", 1},
	{"two", 2}, {"three", 3}, {"four", 4}, {"five", 5}, {"six", 6},
	{"seven", 7}, {"eight", 8}, {"nine", 9}],
    process_lines(Lines, DigitsMap, 0).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
