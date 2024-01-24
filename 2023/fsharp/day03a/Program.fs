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

let buildParts (contents: string) =
    let processLine (row, partLocs) line =
        let processChar (col, partLocs) ch =
            if (not (Char.IsDigit ch)) && (ch <> '.') then
                col + 1, partLocs |> Map.add { Row = row; Col = col } ch
            else
                col + 1, partLocs
        let _, updatedPartLocs =
            Seq.fold processChar (0, partLocs) line
        row + 1, updatedPartLocs
    let _, partLocs =
        Seq.fold processLine (0, Map.empty) (contents.Split('\n'))
    partLocs

let checkParts numberLocs partLocs =
    Seq.fold
        (fun result numberLoc ->
            let numberRow = numberLoc.Row
            let numberColFirst = numberLoc.Col
            let numberColLast = numberLoc.Col + (String.length (Map.find numberLoc numberLocs)) - 1
            let foundAdjacent =
                Seq.fold
                    (fun found numberCol ->
                        if found then true
                        else
                            Seq.fold
                                (fun foundAdjacent neighbor ->
                                    let adjacentPos = { Row = numberRow + neighbor.Row; Col = numberCol + neighbor.Col }
                                    if Map.containsKey adjacentPos partLocs then
                                        true
                                    else
                                        foundAdjacent)
                                false
                                [
                                    { Row = -1; Col = -1 }; { Row = -1; Col = 0 }; { Row = -1; Col = 1 }
                                    { Row = 0; Col = -1 }; { Row = 0; Col = 1 }
                                    { Row = 1; Col = -1 }; { Row = 1; Col = 0 }; { Row = 1; Col = 1 }
                                ])
                    false
                    (Seq.toList (seq { numberColFirst..numberColLast }))
            if foundAdjacent then
                result + int (Map.find numberLoc numberLocs)
            else
                result)
        0
        (Map.keys numberLocs)

let processInput (contents: string) =
    let numberLocs = buildNumbers(contents)
    let partLocs = buildParts(contents)
    checkParts numberLocs partLocs

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
