-module (day04b).
-export ([main/1]).

usage() ->
    Module = "day04b.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

process_input(Contents) ->
    Instances = lists:foldl(fun (Line, PrevInstances) ->
        CardPart = hd(string:split(Line, ": ")),
        {CardNum, _} = string:to_integer(lists:last(string:split(CardPart, " ", all))),
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
        lists:foldl(fun (I, CurrentInstances) ->
            Copies = maps:get(I, CurrentInstances, 0) + 1 + maps:get(CardNum, CurrentInstances, 0),
            maps:put(I, Copies, CurrentInstances)
        end, PrevInstances, lists:seq(CardNum + 1, CardNum + Count))
    end, maps:new(), string:split(string:trim(Contents), "\n", all)),
    CardCount = length(string:split(string:trim(Contents), "\n", all)),
    lists:foldl(fun (Value, Total) ->
        Total + Value
        end, CardCount, maps:values(Instances)).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
