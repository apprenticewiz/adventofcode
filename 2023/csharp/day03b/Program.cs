using System;
using System.Collections.Generic;
using System.Text;

class Program
{
    struct Position {
        public int Row;
        public int Col;

        public Position(int row, int col)
        {
            Row = row;
            Col = col;
        }
    }

    static void Usage()
    {
        string? progPath = Environment.ProcessPath;
        string? progName = Path.GetFileName(progPath);
        Console.WriteLine("usage: " + progName + " <file>");
        System.Environment.Exit(1);
    }

    static Dictionary<Position, string> BuildNumbers(string contents)
    {
        Dictionary<Position, string> numberLocs = new Dictionary<Position, string>();
        bool scanningNumber = false;
        StringBuilder number = new StringBuilder();
        Position currentPos = new Position();
        int row = 0;
        foreach ( string line in contents.Split('\n') )
        {
            for ( int col = 0; col < line.Length; col++ )
            {
                char ch = line[col];
                if ( scanningNumber ) {
                    if ( Char.IsDigit(ch) ) {
                        number.Append(ch);
                    } else {
                        numberLocs[currentPos] = number.ToString();
                        currentPos = new Position();
                        number = new StringBuilder();
                        scanningNumber = false;
                    }
                } else {
                    if ( Char.IsDigit(ch) ) {
                        number.Append(ch);
                        currentPos.Row = row;
                        currentPos.Col = col;
                        scanningNumber = true;
                    }
                }
            }
            if ( scanningNumber ) {
                numberLocs[currentPos] = number.ToString();
                currentPos = new Position();
                number = new StringBuilder();
                scanningNumber = false;
            }
            row++;
        }
        return numberLocs;
    }

    static Dictionary<Position, char> BuildGears(string contents)
    {
        Dictionary<Position, char> gearLocs = new Dictionary<Position, char>();
        int row = 0;
        foreach ( string line in contents.Split('\n') )
        {
            for ( int col = 0; col < line.Length; col++ )
            {
                char ch = line[col];
                if ( !Char.IsDigit(ch) && ch != '.' )
                {
                    Position currentPos = new Position(row, col);
                    gearLocs[currentPos] = ch;
                }
            }
            row++;
        }
        return gearLocs;
    }

    static uint CheckGears(Dictionary<Position, string> numberLocs, Dictionary<Position, char> gearLocs)
    {
        uint result = 0;
        foreach ( Position gearLoc in gearLocs.Keys )
        {
            List<uint> adjacents = new List<uint>();
            foreach ( Position numberLoc in numberLocs.Keys )
            {
                bool skip = false;
                for ( int deltaRow = -1; deltaRow <= 1; deltaRow++ )
                {
                    int adjacentRow = gearLoc.Row + deltaRow;
                    for ( int deltaCol = -1; deltaCol <= 1; deltaCol++ )
                    {
                        int adjacentCol = gearLoc.Col + deltaCol;
                        for ( int numberCol = numberLoc.Col; numberCol < numberLoc.Col + numberLocs[numberLoc].Length; numberCol++ )
                        {
                            if ( !skip && adjacentRow == numberLoc.Row && adjacentCol == numberCol )
                            {
                                skip = true;
                                adjacents.Add(UInt32.Parse(numberLocs[numberLoc]));
                            }
                        }
                    }
                }
            }
            if ( adjacents.Count == 2 )
            {
                uint prod = 1;
                foreach ( uint adjacent in adjacents )
                {
                    prod *= adjacent;
                }
                result += prod;
            }
        }
        return result;
    }

    static uint Process(string contents)
    {
        Dictionary<Position, string> numberLocs = BuildNumbers(contents);
        Dictionary<Position, char> gearLocs = BuildGears(contents);
        return CheckGears(numberLocs, gearLocs);
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
