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
        Dictionary<uint, uint> instances = new Dictionary<uint, uint>();
        foreach (string line in contents.Split('\n'))
        {
            string[] lineParts = line.Split(": ");
            string cardPart = lineParts[0];
            string[] cardNumParts = cardPart.Split(' ');
            uint cardNumber = UInt32.Parse(cardNumParts[cardNumParts.Length - 1]);
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
            for (uint i = cardNumber + 1; i <= cardNumber + count; i++)
            {
                uint copies = 0;
                uint value = 0;
                if (instances.TryGetValue(i, out value))
                {
                    copies += value;
                }
                copies++;
                if (instances.TryGetValue(cardNumber, out value))
                {
                    copies += value;
                }
                instances[i] = copies;
            }
            result++;
        }
        foreach (uint value in instances.Values)
        {
            result += value;
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
