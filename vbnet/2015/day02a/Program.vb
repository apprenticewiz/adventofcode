Imports System
Imports System.IO

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <input file>")
        Environment.Exit(1)
    End Sub

    Function ParseValue(s As String) As Integer
        Dim value As Integer
        If Not Integer.TryParse(s, value) Then
            Console.Error.WriteLine("Error parsing string at number: " & s)
            Environment.Exit(1)
        End If
        Return value
    End Function

    Function Process(filename As String) As Integer
        Dim totalArea As Integer = 0
        Try
            Using reader As New StreamReader(filename)
                Dim line As String
                line = reader.ReadLine()
                While line IsNot Nothing
                    Dim parts As String() = line.Split("x")
                    Dim l As Integer = ParseValue(parts(0))
                    Dim w As Integer = ParseValue(parts(1))
                    Dim h As Integer = ParseValue(parts(2))
                    Dim area1 As Integer = l * w
                    Dim area2 As Integer = l * h
                    Dim area3 As Integer = w * h
                    Dim surfaceArea As Integer = 2 * area1 + 2 * area2 + 2 * area3
                    Dim minArea As Integer = Math.Min(area1, Math.Min(area2, area3))
                    totalArea += surfaceArea + minArea
                    line = reader.ReadLine()
                End While
            End Using
        Catch e As IOException
            Console.Error.WriteLine("Error reading file: " & e.Message)
            Environment.Exit(1)
        End Try
        Return totalArea
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
