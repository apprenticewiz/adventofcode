open System
open System.IO
open System.Text.RegularExpressions

open AOC_Utils.Geometry

let ROW_MAX = 1000
let COL_MAX = 1000

type Bounds(upper: Position2D<int>, lower: Position2D<int>) =
    member val UpperLeft = upper with get, set
    member val LowerRight = lower with get, set

type Grid() =
    let mutable grid : int[,] = Array2D.create ROW_MAX COL_MAX 0

    member this.Perform(action: string, bounds: Bounds) =
        for row in bounds.UpperLeft.X .. bounds.LowerRight.X do
            for col in bounds.UpperLeft.Y .. bounds.LowerRight.Y do
                match action with
                | "turn on" -> grid[row, col] <- grid[row, col] + 1
                | "turn off" -> grid[row, col] <- if grid[row, col] > 0 then grid[row, col] - 1 else 0
                | "toggle" -> grid[row, col] <- grid[row, col] + 2
                | _ -> ()

    member this.Sum() =
        let mutable sum = 0
        for row in 0 .. ROW_MAX - 1 do
            for col in 0 .. COL_MAX - 1 do
                sum <- sum + grid[row, col]
        sum

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processFile(filename: string) =
    let grid = Grid()
    let re = Regex(@"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

    try
        use reader = new StreamReader(filename)
        let mutable line = reader.ReadLine()
        while not (isNull line) do
            let matches = re.Match(line)
            if matches.Success then
                let action = matches.Groups[1].Value
                let r1 = int matches.Groups[2].Value
                let c1 = int matches.Groups[3].Value
                let upperLeft = Position2D<int>(r1, c1)
                let r2 = int matches.Groups[4].Value
                let c2 = int matches.Groups[5].Value
                let lowerRight = Position2D<int>(r2, c2)
                let bounds = Bounds(upperLeft, lowerRight)
                grid.Perform(action, bounds)
            line <- reader.ReadLine()
    with
    | :? IOException as e ->
        Console.Error.WriteLine("Error reading file: " + e.Message)
        Environment.Exit(1)

    grid.Sum()

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        usage()

    let filename = args.[0]
    let result = processFile filename
    Console.WriteLine("result = " + result.ToString());
    0 
