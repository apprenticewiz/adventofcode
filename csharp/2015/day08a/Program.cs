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
                    int memLen = 0;
                    int i = 1;
                    while ( i < line.Length - 1 )
                    {
                        switch ( line[i] )
                        {
                        case '\\':
                            switch ( line[i + 1] )
                            {
                            case '\\':
                            case '\"':
                                i += 2;
                                break;
                            case 'x':
                                i += 4;
                                break;
                            default:
                                i += 1;
                                break;
                            }
                            break;
                        default:
                            i += 1;
                            break;
                        }
                        memLen += 1;
                    }
                    result += codeLen - memLen;
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
