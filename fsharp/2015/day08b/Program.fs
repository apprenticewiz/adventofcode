open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let rec scanChars(chars, i) =
    match chars with
    | [] -> i
    | (ch :: rest) ->
        (match ch with
         | '\\' | '\"' -> scanChars(rest, i + 2)
         | _           -> scanChars(rest, i + 1)
        )

let processFile(filename: string) =
    try
        let content = File.ReadAllText(filename)
        content.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.fold
           (fun acc line ->
               let codeLen = line.Length
               let encLen = scanChars(Seq.toList line, 0)
               acc + 2 + (encLen - codeLen)
           )
           0
    with
    | :? IOException as e ->
        Console.Error.WriteLine("Error reading file: " + e.Message)
        Environment.Exit(1)
        0

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        usage()

    let filename = args.[0]
    let result = processFile filename
    Console.WriteLine("result = " + result.ToString());
    0 
