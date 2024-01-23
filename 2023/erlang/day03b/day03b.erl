-module (day03b).
-export ([main/1]).

usage() ->
    Module = "day03b.erl",
    io:format("usage: escript ~p <file>~n", [Module]),
    halt(1).

build_numbers(Contents) ->
    element(2, lists:foldl(fun(Line, {Row, NumberLocs}) ->
        {Row + 1, element(5, lists:foldl(fun(Ch, {Col, ScanningNumber, Number, CurrentPos, InnerNumberLocs}) ->
                case ScanningNumber of
                    true ->
                        if Ch >= $0 andalso Ch =< $9 ->
                            if Col + 1 == length(Line) ->
                                {Col + 1, false, "", {-1, -1}, maps:put(CurrentPos, Number ++ [Ch], InnerNumberLocs)}
                            ; true ->
                                {Col + 1, true, Number ++ [Ch], CurrentPos, InnerNumberLocs}
                            end
                        ; true ->
                            {Col + 1, false, "", {-1, -1}, maps:put(CurrentPos, Number, InnerNumberLocs)}
                        end;
                    false ->
                        if Ch >= $0 andalso Ch =< $9 ->
                            {Col + 1, true, [Ch], {Row, Col}, InnerNumberLocs}
                        ; true ->
                            {Col + 1, false, "", CurrentPos, InnerNumberLocs}
                        end
                end
            end, {0, false, "", {-1, -1}, NumberLocs}, Line))}
    end, {0, #{}}, string:split(string:trim(Contents), "\n", all))).

build_gears(Contents) ->
    element(2, lists:foldl(fun(Line, {Row, PartLocs}) ->
        {Row + 1, element(2, lists:foldl(fun(Ch, {Col, InnerPartLocs}) ->
            if Ch == $* ->
                {Col + 1, maps:put({Row, Col}, Ch, InnerPartLocs)}
            ; true ->
                {Col + 1, InnerPartLocs}
            end
        end, {0, PartLocs}, Line))}
    end, {0, #{}}, string:split(string:trim(Contents), "\n", all))).

check_gears(NumberLocs, GearLocs) ->
    lists:foldl(fun (GearLoc, Result) ->
            Adjacents = lists:foldl(fun (NumberLoc, AdjacentsVec) ->
                    NumberRow = element(1, NumberLoc),
                    NumberColFirst = element(2, NumberLoc),
                    NumberColLast = element(2, NumberLoc) + string:length(maps:get(NumberLoc, NumberLocs)) - 1,
                    FoundAdjacent = lists:foldl(fun (Neighbor, Found) ->
                            AdjacentPos = {element(1, GearLoc) + element(1, Neighbor),
                                           element(2, GearLoc) + element(2, Neighbor)},
                            Found orelse ((element(1, AdjacentPos) == NumberRow) andalso
                                          (element(2, AdjacentPos) >= NumberColFirst) andalso
                                          (element(2, AdjacentPos) =< NumberColLast))
                        end, false, [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]),
                    case FoundAdjacent of
                        true -> AdjacentsVec ++ [maps:get(NumberLoc, NumberLocs)];
                        false -> AdjacentsVec
                    end
                end, [], maps:keys(NumberLocs)),
            if length(Adjacents) == 2 ->
                Result + lists:foldl(fun (Adjacent, Acc) ->
                    Acc * element(1, string:to_integer(Adjacent)) end, 1, Adjacents);
            true ->
                Result
            end
        end, 0, maps:keys(GearLocs)).

process_input(Contents) ->
    NumberLocs = build_numbers(Contents),
    PartLocs = build_gears(Contents),
    check_gears(NumberLocs, PartLocs).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
