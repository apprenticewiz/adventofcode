open System
open System.Collections.Generic
open System.IO

open AOC_Utils.Geometry

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processFile(filename: string) =
    let mutable positions = HashSet<Position2D<int>>()
    let f (santa: Position2D<int>) ch =
        let santa' = match ch with
                     | '^' -> new Position2D<int>(santa.X, santa.Y + 1)
                     | 'v' -> new Position2D<int>(santa.X, santa.Y - 1)
                     | '<' -> new Position2D<int>(santa.X - 1, santa.Y)
                     | '>' -> new Position2D<int>(santa.X + 1, santa.Y)
                     | _ -> santa
        positions.Add(santa') |> ignore
        santa'

    try
        File.ReadAllText(filename)
        |> Seq.fold f (Position2D<int>(0, 0))
        |> ignore
        positions.Count
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
