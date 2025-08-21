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
    bool[,] grid;

    public const int X_MAX = 1000;
    public const int Y_MAX = 1000;

    public Grid()
    {
        grid = new bool[X_MAX, Y_MAX];
        for ( int j = 0; j < Y_MAX; j++ )
        {
            for ( int i = 0; i < X_MAX; i++ )
            {
                grid[j, i] = false;
            }
        }
    }

    public void Perform(string action, Bounds bounds)
    {
        for ( int j = bounds.UpperLeft.Y; j <= bounds.LowerRight.Y; j++ )
        {
            for ( int i = bounds.UpperLeft.X; i <= bounds.LowerRight.X; i++ )
            {
                switch ( action )
                {
                    case "turn on":
                        grid[j, i] = true;
                        break;
                    case "turn off":
                        grid[j, i] = false;
                        break;
                    case "toggle":
                        grid[j, i] = !grid[j, i];
                        break;
                }
            }
        }
    }

    public int Count()
    {
        int count = 0;
        for ( int j = 0; j < Y_MAX; j++ )
        {
            for ( int i = 0; i < X_MAX; i++ )
            {
                if ( grid[j, i] )
                {
                    count++;
                }
            }
        }
        return count;
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

        return grid.Count();
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
