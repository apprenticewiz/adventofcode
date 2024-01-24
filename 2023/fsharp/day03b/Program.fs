open System
open System.IO

type Position = { Row : int; Col : int }

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let buildNumbers (contents: string) =
    let processLine (row, numLocs) line =
        let processChar (col, scanningNumber, number, currentPos, numLocs) ch =
            if scanningNumber then
                if Char.IsDigit ch then
                    if col + 1 = String.length line then
                        col + 1, false, "", { Row = -1; Col = -1 }, numLocs |> Map.add currentPos (number + string ch)
                    else
                        col + 1, true, number + string ch, currentPos, numLocs
                else
                    col + 1, false, "", { Row = -1; Col = -1 }, numLocs |> Map.add currentPos number
            else
                if Char.IsDigit ch then
                    col + 1, true, string ch, { Row = row; Col = col }, numLocs
                else
                    col + 1, false, "", { Row = -1; Col = -1 }, numLocs
        let _, _, _, _, updatedNumLocs =
            Seq.fold processChar (0, false, "", { Row = -1; Col = -1 }, numLocs) line
        row + 1, updatedNumLocs
    let _, numLocs =
        Seq.fold processLine (0, Map.empty) (contents.Split('\n'))
    numLocs

let buildGears (contents: string) =
    let processLine (row, gearLocs) line =
        let processChar (col, gearLocs) ch =
            if ch = '*' then
                col + 1, gearLocs |> Map.add { Row = row; Col = col } ch
            else
                col + 1, gearLocs
        let _, updatedGearLocs =
            Seq.fold processChar (0, gearLocs) line
        row + 1, updatedGearLocs
    let _, gearLocs =
        Seq.fold processLine (0, Map.empty) (contents.Split('\n'))
    gearLocs

let checkGears (numberLocs : Map<Position, string>) (gearLocs : Map<Position, char>) =
    Seq.fold
        (fun result gearLoc ->
            let adjacents =
                Seq.fold
                    (fun adjacentsVec numberLoc ->
                        let numberRow = numberLoc.Row
                        let numberColFirst = numberLoc.Col
                        let numberColLast = numberLoc.Col + (String.length (Map.find numberLoc numberLocs))
                        let foundAdjacent =
                            Seq.fold
                                (fun found neighbor ->
                                    let adjacentPos = { Row = gearLoc.Row + neighbor.Row; Col = gearLoc.Col + neighbor.Col }
                                    if
                                        (adjacentPos.Row = numberRow)
                                        && (adjacentPos.Col >= numberColFirst)
                                        && (adjacentPos.Col < numberColLast)
                                    then
                                        true
                                    else
                                        found)
                                false
                                [
                                    { Row = -1; Col = -1 }; { Row = -1; Col = 0 }; { Row = -1; Col = 1 }
                                    { Row = 0; Col = -1 }; { Row = 0; Col = 1 }
                                    { Row = 1; Col = -1 }; { Row = 1; Col = 0 }; { Row = 1; Col = 1 }
                                ]

                        if foundAdjacent then
                            adjacentsVec @ [Map.find numberLoc numberLocs]
                        else
                            adjacentsVec)
                    []
                    (Map.keys numberLocs)

            if Seq.length adjacents = 2 then
                result + (Seq.fold (fun acc n -> acc * int n) 1 (Seq.map int adjacents))
            else
                result)
        0
        (Map.keys gearLocs)

let processInput (contents: string) =
    let numberLocs = buildNumbers(contents)
    let gearLocs = buildGears(contents)
    checkGears numberLocs gearLocs

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
