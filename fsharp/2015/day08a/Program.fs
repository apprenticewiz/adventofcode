open System
open System.IO

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let rec scanChars(chars, i) =
    match chars with
    | [] -> i
    | [_] -> i + 1
    | ch1 :: ch2 :: rest ->
        (match (ch1, ch2) with
         | ('\\', '\\') -> scanChars(rest, i + 1)
         | ('\\', '\"') -> scanChars(rest, i + 1)
         | ('\\', 'x')  -> scanChars(List.tail (List.tail rest), i + 1)
         | _ -> scanChars(ch2 :: rest, i + 1)
        )

let processFile(filename: string) =
    try
        let content = File.ReadAllText(filename)
        content.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.fold
           (fun acc line ->
               let codeLen = line.Length
               let quoted = line.Substring(1, line.Length - 2)
               let memLen = scanChars(Seq.toList quoted, 0)
               acc + (codeLen - memLen)
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
