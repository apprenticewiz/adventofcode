open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let prop1(str: string) =
    let rec f1 s =
        let rec f2 a b s =
            match s with
            | c :: d :: rest -> if (a = c) && (b = d) then
                                    true
                                else
                                    f2 a b (d :: rest)
            | _ -> false
        match s with
        | a :: b :: rest -> if f2 a b rest then
                                true
                            else f1 (b :: rest)
        | _ -> false
    str |> Seq.toList |> f1

let prop2(str: string) =
    let rec f s =
        match s with
        | a :: b :: c ::  _ when a = c -> true
        | a :: b :: c :: rest -> f (b :: c :: rest)
        | _ -> false
    str |> Seq.toList |> f

let processFile(filename: string) =
    let f acc (line: string) =
        if (prop1 line) && (prop2 line) then
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
