Imports System
Imports System.Security.Cryptography
Imports System.Text

Module Program
    Sub Usage()
        Console.Error.WriteLine("usage: dotnet run <key>")
        Environment.Exit(1)
    End Sub

    Function Process(key As String) As Integer
        Dim n As Integer = 1
        Using md5 As MD5 = MD5.Create()
            Do While True
                Dim tryKey As String = key & n.ToString()
                Dim inputBytes As Byte() = Encoding.UTF8.GetBytes(tryKey)
                Dim hashBytes As Byte() = md5.ComputeHash(inputBytes)
                Dim sb As New StringBuilder()
                For Each b As Byte in hashBytes
                    sb.Append(b.ToString("x2"))
                Next
                Dim digest As String = sb.ToString()
                If digest.StartsWith("000000") Then
                    Exit Do
                End If
                n += 1
            Loop
        End Using
        Return n
    End Function

    Sub Main(args As String())
        If args.Length < 1 Then
            Usage()
        End If
        Dim key As String = args(0)
        Dim result As Integer = Process(key)
        Console.WriteLine("result = " & result)
    End Sub
End Module
