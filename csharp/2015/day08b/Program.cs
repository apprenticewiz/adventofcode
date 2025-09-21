using System;
using System.IO;

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static int Process(string filename)
    {
        int result = 0;

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    int codeLen = line.Length;
                    int encLen = 0;
                    for ( int i = 0; i < line.Length; i++ )
                    {
                        switch ( line[i] )
                        {
                        case '\\':
                        case '\"':
                            encLen += 2;
                            break;
                        default:
                            encLen += 1;
                            break;
                        }
                    }
                    result += 2 + (encLen - codeLen);
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        return result;
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
