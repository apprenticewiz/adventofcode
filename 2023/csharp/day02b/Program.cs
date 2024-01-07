using System;

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
            if (line.Contains(": "))
            {
                string[] parts = line.Split(": ");
                string gameStr = parts[0];
                string revealsStr = parts[1];

                if (gameStr.Contains(' '))
                {
                    string[] gameParts = gameStr.Split(' ');
                    string gameNumStr = gameParts[1];

                    if (uint.TryParse(gameNumStr, out uint gameNum))
                    {
                        uint redNeeded = 0;
                        uint greenNeeded = 0;
                        uint blueNeeded = 0;

                        foreach (string subsetStr in revealsStr.Split("; "))
                        {
                            foreach (string cubesStr in subsetStr.Split(", "))
                            {
                                if (cubesStr.Contains(' '))
                                {
                                    string[] cubeParts = cubesStr.Split(' ');
                                    string amountStr = cubeParts[0];
                                    string color = cubeParts[1];

                                    if (uint.TryParse(amountStr, out uint amount))
                                    {
                                        switch (color)
                                        {
                                            case "red":
                                                if (amount > redNeeded)
                                                {
                                                    redNeeded = amount;
                                                }
                                                break;
                                            case "green":
                                                if (amount > greenNeeded)
                                                {
                                                    greenNeeded = amount;
                                                }
                                                break;
                                            case "blue":
                                                if (amount > blueNeeded)
                                                {
                                                    blueNeeded = amount;
                                                }
                                                break;
                                            default:
                                                throw new Exception("unknown color");
                                        }
                                    }
                                }
                            }
                        }

                        result += redNeeded * greenNeeded * blueNeeded;
                    }
                }
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
