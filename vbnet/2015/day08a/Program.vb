Imports System
Imports System.IO

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <input file>")
        Environment.Exit(1)
    End Sub

    Function Process(filename As String) As Integer
        Dim result As Integer = 0
        Try
            Using reader As New StreamReader(filename)
                Dim line As String = reader.ReadLine()
                While line IsNot Nothing
                    Dim codeLen As Integer = line.Length
                    Dim memLen As Integer = 0
                    Dim i As Integer = 1
                    While i < (line.Length - 1)
                        Select Case line(i)
                            Case Chr(92)
                                Select Case line(i + 1)
                                    Case Chr(34), Chr(92)
                                        i += 2
                                    Case Chr(120)
                                        i += 4
                                    Case Else
                                        i += 1
                                End Select
                            Case Else
                                i += 1
                        End Select
                        memLen += 1
                    End While
                    result += (codeLen - memLen)
		    line = reader.ReadLine()
                End While
            End Using
        Catch e As Exception
            Console.Error.WriteLine("Error reading file: " & e.Message)
            Environment.Exit(1)
        End Try
        Return result
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
