open System
open System.IO

let TOTAL_RED = 12u
let TOTAL_GREEN = 13u
let TOTAL_BLUE = 14u

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    let mutable result = 0u
    for line in contents.Split('\n') do
        match line.Split(": ") with
        | [|gameStr; drawsStr|] ->
            match gameStr.Split(' ') with
            | [|_ ; gameNumStr|] ->
                let gameNum = UInt32.Parse(gameNumStr)
                let mutable valid = true
                for drawStr in drawsStr.Split("; ") do
                    for colorAmounts in drawStr.Split(", ") do
                        match colorAmounts.Split(' ') with
                        | [|amountStr; color|] ->
                            let amount = UInt32.Parse(amountStr)
                            if color = "red" && amount > TOTAL_RED then
                                valid <- false
                            elif color = "green" && amount > TOTAL_GREEN then
                                valid <- false
                            elif color = "blue" && amount > TOTAL_BLUE then
                                valid <- false
                        | _ -> ()
                if valid then
                    result <- result + gameNum
            | _ -> ()
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
