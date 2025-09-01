using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

abstract class Op { }

class Assign : Op
{
    public string Value { get; }
    public Assign(string value) => Value = value;
}

class Not : Op
{
    public string Src { get; }
    public Not(string src) => Src = src;
}

class And : Op
{
    public string Src1 { get; }
    public string Src2 { get; }
    public And(string src1, string src2)
    {
        Src1 = src1;
        Src2 = src2;
    }
}

class Or : Op
{
    public string Src1 { get; }
    public string Src2 { get; }
    public Or(string src1, string src2)
    {
        Src1 = src1;
        Src2 = src2;
    }
}

class LeftShift : Op
{
    public string Src { get; }
    public int Amt { get; }
    public LeftShift(string src, int amt)
    {
        Src = src;
        Amt = amt;
    }
}

class RightShift : Op
{
    public string Src { get; }
    public int Amt { get; }
    public RightShift(string src, int amt)
    {
        Src = src;
        Amt = amt;
    }
}

class Program {
    static void Usage()
    {
        Console.Error.WriteLine("usage: dotnet run <input file>");
        Environment.Exit(1);
    }

    static int Eval(Dictionary<string, Op> operations, Dictionary<string, int> cache, string val)
    {
        if (int.TryParse(val, out int n))
        {
            return n;
        }
        else if (cache.ContainsKey(val))
        {
            return cache[val];
        }
        else
        {
            int result = 0;
            var op = operations[val];
            switch (op)
            {
                case Assign assign:
                    result = Eval(operations, cache, assign.Value);
                    break;
                case Not not:
                    result = ~Eval(operations, cache, not.Src);
                    break;
                case And and:
                    result = Eval(operations, cache, and.Src1) & Eval(operations, cache, and.Src2);
                    break;
                case Or or:
                    result = Eval(operations, cache, or.Src1) | Eval(operations, cache, or.Src2);
                    break;
                case LeftShift leftShift:
                    result = Eval(operations, cache, leftShift.Src) << leftShift.Amt;
                    break;
                case RightShift rightShift:
                    result = Eval(operations, cache, rightShift.Src) >> rightShift.Amt;
                    break;
            }

            result &= 0xffff;
            cache[val] = result;
            return result;
        }
    }

    static int Process(string filename)
    {
        Dictionary<string, Op> operations = new Dictionary<string, Op>();
        Regex assignRegex = new Regex(@"^(\d+|\w+) -> (\w+)$");
        Regex notRegex = new Regex(@"^NOT (\d+|\w+) -> (\w+)$");
        Regex andOrRegex = new Regex(@"^(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)$");
        Regex shiftRegex = new Regex(@"^(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)$");

        try
        {
            using ( StreamReader reader = new StreamReader(filename) )
            {
                string? line;
                while ( (line = reader.ReadLine()) != null )
                {
                    if ( assignRegex.IsMatch(line) )
                    {
                        Match match = assignRegex.Match(line);
                        string val = match.Groups[1].Value;
                        string dest = match.Groups[2].Value;
                        operations[dest] = new Assign(val);
                    }
                    else if ( notRegex.IsMatch(line) )
                    {
                        Match match = notRegex.Match(line);
                        string src = match.Groups[1].Value;
                        string dest = match.Groups[2].Value;
                        operations[dest] = new Not(src);
                    }
                    else if ( andOrRegex.IsMatch(line) )
                    {
                        Match match = andOrRegex.Match(line);
                        string src1 = match.Groups[1].Value;
                        string op = match.Groups[2].Value;
                        string src2 = match.Groups[3].Value;
                        string dest = match.Groups[4].Value;
                        operations[dest] = op == "AND" ? new And(src1, src2) : new Or(src1, src2);
                    }
                    else if ( shiftRegex.IsMatch(line) )
                    {
                        Match match = shiftRegex.Match(line);
                        string src = match.Groups[1].Value;
                        string op = match.Groups[2].Value;
                        int.TryParse(match.Groups[3].Value, out int shiftAmt);
                        string dest = match.Groups[4].Value;
                        operations[dest] = op == "LSHIFT"
                            ? new LeftShift(src, shiftAmt)
                            : new RightShift(src, shiftAmt);
                    }
                    else
                    {
                        Console.Error.WriteLine("error: malformed input line: " + line);
                        Environment.Exit(1);
                    }
                }
            }
        }
        catch ( IOException e )
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
            Environment.Exit(1);
        }

        int a = Eval(operations, new Dictionary<string, int>(), "a");
        operations["b"] = new Assign(a.ToString());
        return Eval(operations, new Dictionary<string, int>(), "a");
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
