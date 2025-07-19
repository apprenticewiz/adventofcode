using System;
using System.Collections.Generic;
using System.IO;

using AOC_Utils.Geometry;

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static int Process(string filename)
    {
        HashSet<Position2D<int>> positions = new HashSet<Position2D<int>>();

        Position2D<int> santa = new Position2D<int>(0 , 0);
        positions.Add(santa);

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    foreach ( char ch in line )
                    {
                        switch ( ch )
                        {
                            case '^':
                                santa.Y += 1;
                                break;
                            case 'v':
                                santa.Y -= 1;
                                break;
                            case '<':
                                santa.X -= 1;
                                break;
                            case '>':
                                santa.X += 1;
                                break;
                        }
                        positions.Add(santa);
                    }
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return positions.Count;
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
