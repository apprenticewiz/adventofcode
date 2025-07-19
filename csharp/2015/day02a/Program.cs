using System;
using System.IO;

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static int ParseValue(string s)
    {
        if ( !int.TryParse(s, out int value) )
        {
            Console.Error.WriteLine("Error parsing string as number: " + s);
            Environment.Exit(1);
        }
        return value;
    }

    static int Process(string filename)
    {
        int totalArea = 0;

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    string[] parts = line.Split('x');
                    int l = ParseValue(parts[0]);
                    int w = ParseValue(parts[1]);
                    int h = ParseValue(parts[2]);
                    int area1 = l * w;
                    int area2 = l * h;
                    int area3 = w * h;
                    int surfaceArea = 2 * area1 + 2 * area2 + 2 * area3;
                    int minArea = Math.Min(area1, Math.Min(area2, area3));
                    totalArea += surfaceArea + minArea;
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return totalArea;
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
