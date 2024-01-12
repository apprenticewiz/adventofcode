open System
open System.IO

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    let mutable result = 0u
    for line in contents.Split('\n') do
        match line.Split(": ") with
        | [|_; drawsStr|] ->
            let mutable redNeeded = 0u
            let mutable greenNeeded = 0u
            let mutable blueNeeded = 0u
            for drawStr in drawsStr.Split("; ") do
                for colorAmounts in drawStr.Split(", ") do
                    match colorAmounts.Split(' ') with
                    | [|amountStr; color|] ->
                        let amount = UInt32.Parse(amountStr)
                        if color = "red" && amount > redNeeded then
                            redNeeded <- amount
                        elif color = "green" && amount > greenNeeded then
                            greenNeeded <- amount
                        elif color = "blue" && amount > blueNeeded then
                            blueNeeded <- amount
                    | _ -> ()
            result <- result + redNeeded * greenNeeded * blueNeeded
        | _ -> ()
    result

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
