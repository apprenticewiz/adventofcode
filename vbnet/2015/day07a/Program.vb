Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Text.RegularExpressions

Public Enum Operators
    AssignOp
    NotOp
    AndOp
    OrOp
    LeftShiftOp
    RightShiftOp
End Enum

Public Structure Operation
    Public Op As Operators
    Public Source1 As String
    Public Source2 As String
    Public Amount As Integer

    Public Sub New(op As Operators, src1 As String, src2 As String, amt As Integer)
        Me.Op = op
        Me.Source1 = src1
        Me.Source2 = src2
        Me.Amount = amt
    End Sub
End Structure

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <input file>")
        Environment.Exit(1)
    End Sub

    Function Eval(Ops As Dictionary(Of String, Operation), Cache As Dictionary(Of String, Integer), Expr As String) As Integer
        Dim digitsRE As New Regex("^\d+$")
        Dim digitsMatch As Match = digitsRE.Match(expr)
        If digitsMatch.Success Then
            Return Integer.Parse(expr)
        ElseIf Cache.ContainsKey(expr) Then
            Return Cache(expr)
        Else
            Dim op As Operation = Ops(expr)
	    Dim r As Integer = 0
	    Dim a As Integer = 0
	    Dim b As Integer = 0
	    Dim masked As Integer = 0
            Select Case op.Op
		Case Operators.AssignOp
		    r = Eval(Ops, Cache, Op.Source1)
		Case Operators.NotOp
		    a = Eval(Ops, Cache, Op.Source1)
		    r = NOT a
		Case Operators.AndOp
	            a = Eval(Ops, Cache, Op.Source1)
		    b = Eval(Ops, Cache, Op.Source2)
		    r = a AND b
		Case Operators.OrOp
		    a = Eval(Ops, Cache, Op.Source1)
		    b = Eval(Ops, Cache, Op.Source2)
		    r = a OR b
		Case Operators.LeftShiftOp
		    a = Eval(Ops, Cache, Op.Source1)
		    r = a << Op.Amount
		Case Operators.RightShiftOp
		    a = Eval(Ops, Cache, Op.Source1)
		    r = a >> Op.Amount
            End Select
	    masked = r AND 65535
	    Cache(expr) = masked
	    Return masked
        End If
    End Function

    Function Process(filename As String) As Integer
        Dim operations As New Dictionary(Of String, Operation)
        Dim cache As New Dictionary(Of String, Integer)
        Dim re1 As New Regex("^(\d+|\w+) -> (\w+)$")
        Dim re2 As New Regex("NOT (\d+|\w+) -> (\w+)")
        Dim re3 As New Regex("(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)")
        Dim re4 As New Regex("(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)")
        Try
            Using reader As New StreamReader(filename)
                Dim line As String
                line = reader.ReadLine()
                While line IsNot Nothing
                    Dim m1 As Match = re1.Match(line)
                    If m1.Success Then
                        Dim src As String = m1.Groups(1).Value
                        Dim dest As String = m1.Groups(2).Value
                        operations.Add(dest, new Operation(Operators.AssignOp, src, Nothing, 0))
                    End If
                    Dim m2 As Match = re2.Match(line)
                    If m2.Success Then
                        Dim src As String = m2.Groups(1).Value
                        Dim dest As String = m2.Groups(2).Value
                        operations.Add(dest, new Operation(Operators.NotOp, src, Nothing, 0))
                    End If
                    Dim m3 As Match = re3.Match(line)
                    If m3.Success Then
                        Dim src1 As String = m3.Groups(1).Value
                        Dim op As String = m3.Groups(2).Value
                        Dim src2 As String = m3.Groups(3).Value
                        Dim dest As String = m3.Groups(4).Value
                        If op = "AND" Then
                            operations.Add(dest, new Operation(Operators.AndOp, src1, src2, 0))
                        Else
                            operations.Add(dest, new Operation(Operators.OrOp, src1, src2, 0))
                        End If
                    End If
                    Dim m4 As Match = re4.Match(line)
                    If m4.Success Then
                        Dim src As String = m4.Groups(1).Value
                        Dim op As String = m4.Groups(2).Value
                        Dim amt As Integer = Integer.Parse(m4.Groups(3).Value)
                        Dim dest As String = m4.Groups(4).Value
                        If op = "LSHIFT" Then
                            operations.Add(dest, new Operation(Operators.LeftShiftOp, src, Nothing, amt))
                        Else
                            operations.Add(dest, new Operation(Operators.RightShiftOp, src, Nothing, amt))
                        End If
                    End If
                    line = reader.ReadLine()
                End While
            End Using
        Catch e As Exception
            Console.Error.WriteLine("Error reading file: " & e.Message)
            Environment.Exit(1)
        End Try
        Return Eval(Operations, Cache, "a")
    End Function

    Sub Main(args As String())
        If args.Length < 1 Then
            Usage()
        End If
        Dim filename As String = args(0)
        Dim result As Integer = Process(filename)
        Console.WriteLine("result = " & result)
    End Sub
End Module
