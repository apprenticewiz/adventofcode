-module (day03a).
-export ([main/1]).

usage() ->
    Module = "day03a.erl",
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

build_parts(Contents) ->
    element(2, lists:foldl(fun(Line, {Row, PartLocs}) ->
        {Row + 1, element(2, lists:foldl(fun(Ch, {Col, InnerPartLocs}) ->
            if not (Ch >= $0 andalso Ch =< $9) andalso Ch /= $. ->
                {Col + 1, maps:put({Row, Col}, Ch, InnerPartLocs)}
            ; true ->
                {Col + 1, InnerPartLocs}
            end
        end, {0, PartLocs}, Line))}
    end, {0, #{}}, string:split(string:trim(Contents), "\n", all))).

check_parts(NumberLocs, PartLocs) ->
    lists:foldl(fun (NumberLoc, Result) ->
            NumberRow = element(1, NumberLoc),
            NumberColFirst = element(2, NumberLoc),
            NumberColLast = element(2, NumberLoc) + string:length(maps:get(NumberLoc, NumberLocs)) - 1,
            FoundAdjacent = lists:foldl(fun (NumberCol, Found) ->
                    if Found ->
                        true
                    ; true ->
                        lists:foldl(fun (Neighbor, FoundAdj) ->
                                AdjacentPos = { NumberRow + element(1, Neighbor), NumberCol + element(2, Neighbor)},
                                FoundAdj or maps:is_key(AdjacentPos, PartLocs)
                            end, false, [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}])
                    end
                end, false, lists:seq(NumberColFirst, NumberColLast)),
            case FoundAdjacent of
                true -> Result + element(1, (string:to_integer(maps:get(NumberLoc, NumberLocs))));
                false -> Result
            end
        end, 0, maps:keys(NumberLocs)).

process_input(Contents) ->
    NumberLocs = build_numbers(Contents),
    PartLocs = build_parts(Contents),
    check_parts(NumberLocs, PartLocs).

main([Filename]) ->
    {ok, File} = file:read_file(Filename),
    Contents = unicode:characters_to_list(File),
    Result = process_input(Contents),
    io:format("result = ~w~n", [Result]);
main(_) ->
    usage().
