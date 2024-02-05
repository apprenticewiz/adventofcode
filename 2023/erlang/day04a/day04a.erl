-module (day04a).
-export ([main/1]).

usage() ->
    Module = "day04a.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_input(Contents) ->
    lists:foldl(fun (Line, Result) ->
        Rest = hd(tl(string:split(Line, ": "))),
        WinningStr = hd(string:split(Rest, " | ")),
        WinningSet = lists:foldl(fun (NumStr, PrevSet) ->
            if NumStr /= "" ->
                sets:add_element(string:to_integer(NumStr), PrevSet)
            ; true ->
                PrevSet
            end
        end, sets:new(), string:split(WinningStr, " ", all)),
        HandStr = hd(tl(string:split(Rest, " | "))),
        HandSet = lists:foldl(fun (NumStr, PrevSet) ->
            if NumStr /= "" ->
                sets:add_element(string:to_integer(NumStr), PrevSet)
            ; true ->
                PrevSet
            end
        end, sets:new(), string:split(HandStr, " ", all)),
        Intersection = sets:intersection(WinningSet, HandSet),
        Count = sets:size(Intersection),
        if Count /= 0 ->
            Result + (1 bsl (Count - 1))
        ; true ->
            Result
        end
    end, 0, string:split(string:trim(Contents), "\n", all)).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
