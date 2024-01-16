-module (day01a).
-export ([main/1]).

usage() ->
    Module = "day01a.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_lines([], _, Result) -> Result;
process_lines([Line | Rest], Digits, Result) ->
    LineResult = process_line(Line, Digits),
    process_lines(Rest, Digits, Result + LineResult).

process_line(Line, Digits) ->
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
        {undefined, undefined}, Digits
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
        {undefined, undefined}, Digits
    ),
    {LeftValue, _} = string:to_integer(LeftDigit),
    {RightValue, _} = string:to_integer(RightDigit),
    (LeftValue * 10) + RightValue.

process_input(Contents) ->
    Trimmed = string:trim(Contents),
    Lines = string:split(Trimmed, "\n", all),
    Digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
    process_lines(Lines, Digits, 0).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
