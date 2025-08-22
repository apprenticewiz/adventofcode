using System;
using System.IO;
using System.Text.RegularExpressions;

using AOC_Utils.Geometry;

class Bounds
{
    public Position2D<int> UpperLeft;
    public Position2D<int> LowerRight;

    public Bounds(Position2D<int> upper, Position2D<int> lower)
    {
        UpperLeft = upper;
        LowerRight = lower;
    }
}

class Grid
{
    int[,] grid;

    public const int ROW_MAX = 1000;
    public const int COL_MAX = 1000;

    public Grid()
    {
        grid = new int[ROW_MAX, COL_MAX];
        for ( int row = 0; row < ROW_MAX; row++ )
        {
            for ( int col = 0; col < COL_MAX; col++ )
            {
                grid[row, col] = 0;
            }
        }
    }

    public void Perform(string action, Bounds bounds)
    {
        for ( int row = bounds.UpperLeft.X; row <= bounds.LowerRight.X; row++ )
        {
            for ( int col = bounds.UpperLeft.Y; col <= bounds.LowerRight.Y; col++ )
            {
                switch ( action )
                {
                    case "turn on":
                        grid[row, col] += 1;
                        break;
                    case "turn off":
                        grid[row, col] = (grid[row, col] > 0) ? grid[row, col] - 1 : 0;
                        break;
                    case "toggle":
                        grid[row, col] += 2;
                        break;
                }
            }
        }
    }

    public int Sum()
    {
        int sum = 0;
        for ( int row = 0; row < COL_MAX; row++ )
        {
            for ( int col = 0; col < ROW_MAX; col++ )
            {
                sum += grid[row, col];
            }
        }
        return sum;
    }
}

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static int Process(string filename)
    {
        Grid grid = new Grid();
        Regex re = new Regex(@"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)");

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    Match match = re.Match(line);
                    if ( match.Success )
                    {
                       string action = match.Groups[1].Value;
                       int.TryParse(match.Groups[2].Value, out int x1);
                       int.TryParse(match.Groups[3].Value, out int y1);
                       Position2D<int> upperLeft = new Position2D<int>(x1, y1);
                       int.TryParse(match.Groups[4].Value, out int x2);
                       int.TryParse(match.Groups[5].Value, out int y2);
                       Position2D<int> lowerRight = new Position2D<int>(x2, y2);
                       Bounds bounds = new Bounds(upperLeft, lowerRight);
                       grid.Perform(action, bounds);
                    }
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return grid.Sum();
    }

    static void Main(string[] args)
    {
        if ( args.Length < 1 )
        {
            Usage();
        }

        string filename = args[0];
        int result = Process(filename);
        Console.WriteLine("result = " + result);
    }
}
