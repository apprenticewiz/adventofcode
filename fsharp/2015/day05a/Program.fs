open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let prop1(str: string) =
    let f acc ch =
        if ch = 'a' || ch = 'e' || ch = 'i' || ch = 'o' || ch = 'u' then
            acc + 1
        else
            acc
    str |> Seq.toList |> Seq.fold f 0 |> fun vowels -> vowels >= 3

let prop2(str: string) =
    let rec f s =
        match s with
        | a :: b :: _ when a = b -> true
        | a :: b :: rest -> f (b :: rest)
        | _ -> false
    str |> Seq.toList |> f

let prop3(str: string) =
    not (str.Contains("ab")) && not (str.Contains("cd")) && not (str.Contains("pq")) && not (str.Contains("xy"))

let processFile(filename: string) =
    let f acc (line: string) =
        if (prop1 line) && (prop2 line) && (prop3 line) then
            acc + 1
        else
            acc

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
