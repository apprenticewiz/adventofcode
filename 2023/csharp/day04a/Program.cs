using System;
using System.Collections.Generic;

class Program
{
    static void Usage()
    {
        string? progPath = Environment.ProcessPath;
        string? progName = Path.GetFileName(progPath);
        Console.WriteLine("usage: " + progName + " <file>");
        System.Environment.Exit(1);
    }

    static uint Process(string contents)
    {
        uint result = 0;
        foreach (string line in contents.Split('\n'))
        {
            string[] lineParts = line.Split(": ");
            string rest = lineParts[1];
            string[] numberParts = rest.Split(" | ");
            string winningStr = numberParts[0];
            HashSet<uint> winningSet = new HashSet<uint>();
            foreach (string numStr in winningStr.Split(' '))
            {
                if (numStr != "")
                {
                    uint num = UInt32.Parse(numStr);
                    winningSet.Add(num);
                }
            }
            string handStr = numberParts[1];
            HashSet<uint> handSet = new HashSet<uint>();
            foreach (string numStr in handStr.Split(' '))
            {
                if (numStr != "")
                {
                    uint num = UInt32.Parse(numStr);
                    handSet.Add(num);
                }
            }
            HashSet<uint> intersection = new HashSet<uint>(winningSet);
            intersection.IntersectWith(handSet);
            int count = intersection.Count;
            if (count > 0u)
            {
                result += (uint)(1 << (count - 1));
            }
        }
        return result;
    }

    public static void Main(string[] args)
    {
        if ( args.Length < 1 )
        {
            Program.Usage();
        }
        string fileName = args[0];
        string contents = File.ReadAllText(fileName);
        contents = contents.Remove(contents.Length - 1, 1);
        uint result = Process(contents);
        Console.WriteLine("result = " + result);
    }
}
