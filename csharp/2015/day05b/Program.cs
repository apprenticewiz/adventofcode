using System;
using System.IO;

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static bool Prop1(string str)
    {
        for ( int i = 0; i < str.Length - 3; i++ )
        {
            string firstPair = str.Substring(i, 2);
            for ( int j = i + 2; j < str.Length - 1; j++ )
            {
                string secondPair = str.Substring(j, 2);
                if ( firstPair.Equals(secondPair) )
                {
                    return true;
                }
            }
        }
        return false;
    }

    static bool Prop2(string str)
    {
        for ( int i = 0; i < str.Length - 2; i++ )
        {
            if ( str[i] == str[i + 2] )
            {
                return true;
            }
        }
        return false;
    }

    static int Process(string filename)
    {
        int count = 0;

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    if ( Prop1(line) && Prop2(line) )
                    {
                        count++;
                    }
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return count;
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
