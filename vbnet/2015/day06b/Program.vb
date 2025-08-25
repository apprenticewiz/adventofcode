Imports System
Imports System.IO
Imports System.Text.RegularExpressions

Imports AOC_Utils.Geometry

Public Class Bounds
    Public UpperLeft As Position2D(Of Integer)
    Public LowerRight As Position2D(Of Integer)

    Public Sub New(upper As Position2D(Of Integer), lower As Position2D(Of Integer))
        UpperLeft = upper
    LowerRight = lower
    End Sub
End Class

Public Class Grid
    Private grid(,) As Integer
    Public Const ROW_MAX As Integer = 1000
    Public Const COL_MAX As Integer = 1000

    Public Sub New()
        grid = New Integer(ROW_MAX - 1, COL_MAX - 1) {}
        For row As Integer = 0 To ROW_MAX - 1
            For col As Integer = 0 To COL_MAX - 1
                grid(row, col) = 0
            Next
        Next
    End Sub

    Public Sub Perform(action As String, bounds As Bounds)
        For row As Integer = bounds.UpperLeft.X To bounds.LowerRight.X
            For col As Integer = bounds.UpperLeft.Y To bounds.LowerRight.Y
                Select Case action
                    Case "turn on"
                        grid(row, col) += 1
                    Case "turn off"
                        grid(row, col) = Math.max(0, grid(row, col) - 1)
                    Case "toggle"
                        grid(row, col) += 2
                End Select
            Next
        Next
    End Sub

    Public Function Count() As Integer
        Dim total As Integer = 0
        For row As Integer = 0 To ROW_MAX - 1
            For col As Integer = 0 To COL_MAX - 1
                total += grid(row, col)
            Next
        Next
    Return total
    End Function
End Class

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <input file>")
        Environment.Exit(1)
    End Sub

    Function Process(filename As String) As Integer
        Dim grid As New Grid()
        Dim re As New Regex("(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")
        Try
            Using reader As New StreamReader(filename)
                Dim line As String
                line = reader.ReadLine()
                While line IsNot Nothing
                    Dim match As Match = re.Match(line)
                    If match.Success Then
                        Dim action As String = match.Groups(1).Value
                        Dim r1 As Integer = Integer.Parse(match.Groups(2).Value)
                        Dim c1 As Integer = Integer.Parse(match.Groups(3).Value)
                        Dim upperLeft As New Position2D(Of Integer)(r1, c1)
                        Dim r2 As Integer = Integer.Parse(match.Groups(4).Value)
                        Dim c2 As Integer = Integer.Parse(match.Groups(5).Value)
                        Dim lowerRight As New Position2D(Of Integer)(r2, c2)
                        Dim bounds As New Bounds(upperLeft, lowerRight)
                        grid.Perform(action, bounds)
                    End If
                    line = reader.ReadLine()
                End While
            End Using
        Catch e As Exception
            Console.Error.WriteLine("Error reading file: " & e.Message)
            Environment.Exit(1)
        End Try
        Return Grid.Count()
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
