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
        int vowels = 0;

        foreach ( char ch in str )
        {
            if ( ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u' ) {
                vowels++;
            }
        }

        return vowels >= 3;
    }

    static bool Prop2(string str)
    {
        for ( int i = 0; i < str.Length - 1; i++ )
        {
            if ( str[i] == str[i + 1] )
            {
                return true;
            }
        }

        return false;
    }

    static bool Prop3(string str)
    {
        return !str.Contains("ab") &&
            !str.Contains("cd") &&
            !str.Contains("pq") &&
            !str.Contains("xy");
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
                    if ( Prop1(line) && Prop2(line) && Prop3(line) )
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
