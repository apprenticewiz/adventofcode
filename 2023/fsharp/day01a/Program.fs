open System
open System.IO

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    let digits = ['0'..'9'] |> List.map string
    let mutable sum = 0u
    for line in contents.Split('\n') do
        let mutable minIndex = None
        let mutable maxIndex = None
        let mutable leftDigit = '0'
        let mutable rightDigit = '0'
        digits |> List.iter (fun digit ->
            if line.Contains(digit) then
                let leftIndex = line.IndexOf(digit)
                if Option.isNone minIndex || leftIndex < Option.get minIndex then
                    minIndex <- Some leftIndex
                    leftDigit <- digit.[0]
                let rightIndex = line.LastIndexOf(digit)
                if Option.isNone maxIndex || rightIndex > Option.get maxIndex then
                    maxIndex <- Some rightIndex
                    rightDigit <- digit.[0]
        )
        sum <- sum + (((uint leftDigit) - (uint '0')) * 10u) + ((uint rightDigit) - (uint '0'))
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
