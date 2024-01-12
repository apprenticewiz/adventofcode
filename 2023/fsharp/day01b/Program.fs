open System
open System.Collections.Generic
open System.IO

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    let digitsMap = dict [
        "0", 0u
        "1", 1u
        "2", 2u
        "3", 3u
        "4", 4u
        "5", 5u
        "6", 6u
        "7", 7u
        "8", 8u
        "9", 9u
        "zero", 0u
        "one", 1u
        "two", 2u
        "three", 3u
        "four", 4u
        "five", 5u
        "six", 6u
        "seven", 7u
        "eight", 8u
        "nine", 9u
    ]
    let mutable sum = 0u
    for line in contents.Split('\n') do
        let mutable minIndex = None
        let mutable maxIndex = None
        let mutable leftDigit = 0u
        let mutable rightDigit = 0u
        digitsMap.Keys |> Seq.iter (fun digit ->
            if line.Contains(digit) then
                let leftIndex = line.IndexOf(digit)
                if Option.isNone minIndex || leftIndex < Option.get minIndex then
                    minIndex <- Some leftIndex
                    leftDigit <- digitsMap.[digit]
                let rightIndex = line.LastIndexOf(digit)
                if Option.isNone maxIndex || rightIndex > Option.get maxIndex then
                    maxIndex <- Some rightIndex
                    rightDigit <- digitsMap.[digit]
        )
        sum <- sum + leftDigit * 10u + rightDigit
    sum

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
