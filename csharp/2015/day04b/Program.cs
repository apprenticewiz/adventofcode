using System;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <key>");
        Environment.Exit(1);
    }

    static int Process(string key)
    {
        using MD5 md5 = MD5.Create();
        int n = 1;

        while ( true )
        {
            string tryKey = key + n.ToString();
            byte[] inputBytes = Encoding.UTF8.GetBytes(tryKey);
            byte[] hashBytes = md5.ComputeHash(inputBytes);

            StringBuilder sb = new StringBuilder();
            foreach ( byte b in hashBytes )
            {
                sb.Append(b.ToString("x2"));
            }
            string digest = sb.ToString();

            if ( digest.StartsWith("000000") )
            {
                break;
            }
            n++;
        }

        return n;
    }

    static void Main(string[] args)
    {
        if ( args.Length < 1 )
        {
            Usage();
        }

        string key = args[0];
        int result = Process(key);
        Console.WriteLine("result = " + result);
    }
}
