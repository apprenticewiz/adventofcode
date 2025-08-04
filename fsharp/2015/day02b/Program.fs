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
            let perim1 = 2 * (l + w)
            let perim2 = 2 * (l + h)
            let perim3 = 2 * (w + h)
            let presentLen = List.min [perim1; perim2; perim3]
            let bowLen = l * w * h
            acc + presentLen + bowLen
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
