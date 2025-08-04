open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processFile(filename: string) =
    let f acc (line: string) =
        let dims = line.Split('x') |> Array.map int
        match dims with
        | [| l; w; h |] ->
            let area1 = l * w
            let area2 = l * h
            let area3 = w * h
            let surfaceArea = 2 * area1 + 2 * area2 + 2 * area3
            let minArea = List.min [area1; area2; area3]
            acc + surfaceArea + minArea
        | _ ->
            failwithf "unexpected input: %s" line

    try
        let content = File.ReadAllText(filename)
        content.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.fold f 0
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
