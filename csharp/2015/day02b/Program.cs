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
        int totalLen = 0;

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
                    int perim1 = 2 * (l + w);
                    int perim2 = 2 * (l + h);
                    int perim3 = 2 * (w + h);
                    int presentLen = Math.Min(perim1, Math.Min(perim2, perim3));
                    int bowLen = l * w * h;
                    totalLen += presentLen + bowLen;
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return totalLen;
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
