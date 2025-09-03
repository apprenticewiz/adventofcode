open System
open System.IO
open System.Text.RegularExpressions

type Operation =
   | Assign of string
   | Not of string
   | And of string * string
   | Or of string * string
   | LeftShift of string * int
   | RightShift of string * int

let rec eval (operations: Map<string, Operation>) (cache: Map<string, int>) (expr: string) : int * Map<string, int> =
   match Int32.TryParse(expr) with
   | true, n -> n, cache
   | false, _ ->
       match Map.tryFind expr cache with
       | Some value -> value, cache
       | None ->
           let op = Map.find expr operations
           let eval' e c = eval operations c e
           let result, newCache =
               match op with
               | Assign src ->
                   let res, c = eval' src cache
                   res, c
               | Not src ->
                   let res, c = eval' src cache
                   ~~~res, c
               | And (src1, src2) ->
                   let r1, c1 = eval' src1 cache
                   let r2, c2 = eval' src2 c1
                   r1 &&& r2, c2
               | Or(src1, src2) ->
                   let r1, c1 = eval' src1 cache
                   let r2, c2 = eval' src2 c1
                   r1 ||| r2, c2
               | LeftShift(src, amt) ->
                   let res, c = eval' src cache
                   res <<< amt, c
               | RightShift(src, amt) ->
                   let res,c  = eval' src cache
                   res >>> amt, c
           let masked = result &&& 0xffff
           masked, Map.add expr masked newCache

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processFile(filename: string) : int =
    let regexes : (Regex * (Match -> string * Operation)) list =
        [
            Regex(@"^(\d+|\w+) -> (\w+)$"),
            fun m -> m.Groups.[2].Value, Assign m.Groups.[1].Value

            Regex(@"NOT (\d+|\w+) -> (\w+)$"),
            fun m -> m.Groups.[2].Value, Not m.Groups.[1].Value

            Regex(@"(\d+|\w+) AND (\d+|\w+) -> (\w+)$"),
            fun m -> m.Groups.[3].Value, And (m.Groups.[1].Value, m.Groups.[2].Value)

            Regex(@"(\d+|\w+) OR (\d+|\w+) -> (\w+)$"),
            fun m -> m.Groups.[3].Value, Or (m.Groups.[1].Value, m.Groups.[2].Value)

            Regex(@"(\d+|\w+) LSHIFT (\d+) -> (\w+)$"),
            fun m -> m.Groups.[3].Value, LeftShift (m.Groups.[1].Value, int m.Groups.[2].Value)

            Regex(@"(\d+|\w+) RSHIFT (\d+) -> (\w+)$"),
            fun m -> m.Groups.[3].Value, RightShift (m.Groups.[1].Value, int m.Groups.[2].Value)
        ]
    let operations : Map<string, Operation> = 
        File.ReadAllLines(filename)
        |> Array.fold (fun acc line ->
            let matched =
                regexes
                |> List.tryPick (fun (rx, handler) ->
                    let m = rx.Match(line)
                    if m.Success then Some (handler m) else None
                )
            match matched with
            | Some (key, op) -> Map.add key op acc
            | None ->
                eprintfn "error: malformed line: %s" line
                Environment.Exit(1)
                acc
        ) Map.empty
    let result, _ = eval operations Map.empty "a"
    result

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        usage()

    let filename = args.[0]
    let result = processFile filename
    Console.WriteLine("result = " + result.ToString());
    0 
