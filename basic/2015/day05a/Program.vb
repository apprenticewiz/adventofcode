Imports System
Imports System.IO

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <input file>")
        Environment.Exit(1)
    End Sub

    Function Prop1(line As String) As Boolean
        Dim vowels As Integer = 0
        For Each ch As Char In line
            Select Case ch
            Case "a"c, "e"c, "i"c, "o"c, "u"c
                vowels += 1
            End Select
        Next
        Return vowels >= 3
    End Function

    Function Prop2(line As String) As Boolean
        For i As Integer = 0 To line.Length - 2
            If line(i) = line(i + 1) Then
                Return True
            End If
        Next
        Return False
    End Function

    Function Prop3(line As String) As Boolean
        Return Not line.contains("ab") AndAlso
               Not line.contains("cd") AndAlso
               Not line.contains("pq") AndAlso
               Not line.contains("xy")
    End Function

    Function Process(filename As String) As Integer
        Dim n As Integer = 0
        Try
            Using reader As New StreamReader(filename)
                Dim line As String
                line = reader.ReadLine()
                While line IsNot Nothing
                    If Prop1(line) AndAlso Prop2(line) AndAlso Prop3(line)
                        n += 1
                    End If
                    line = reader.ReadLine()
                End While
            End Using
        Catch e As Exception
            Console.Error.WriteLine("Error reading file: " & e.Message)
            Environment.Exit(1)
        End Try
        Return n
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
