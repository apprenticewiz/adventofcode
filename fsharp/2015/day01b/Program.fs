open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processFile(filename: string) =
    let f (pos, count) ch =
        if count < 0 then (pos, count)
        else
            let count' =
                match ch with
                | '(' -> count + 1
                | ')' -> count - 1
                | _ -> count
            (pos + 1, count')

    try
        File.ReadAllText(filename)
        |> Seq.fold f (0, 0)
        |> fst
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
