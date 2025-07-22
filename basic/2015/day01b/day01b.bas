#include "crt/stdlib.bi"

Sub usage(progname As String)
    Print "usage: " & progname & " <input file>"
    End 1
End Sub

Function process(filename As String) As Integer
    Dim counter As Integer = 0
    Dim lineText As String
    Dim f As Integer

    f = FreeFile()
    If Open(filename For Input As #f) <> 0 Then
        Print "Error opening file: " & filename
        End 1
    End If

    While Not Eof(f)
        Line Input #f, lineText
        For i As Integer = 1 To Len(lineText)
            Select Case Mid(lineText, i, 1)
                Case "("
                    counter += 1
                Case ")"
                    counter -= 1
            End Select
            If counter < 0 Then
                Return i
            End If
        Next i
    Wend

    Close #f
    Return 0
End Function

Dim argc As Integer = __FB_ARGC__
Dim argv As ZString Ptr Ptr = __FB_ARGV__

If argc < 2 Then
    usage(*argv[0])
End If

Dim filename As String = *argv[1]
Dim result As Integer = process(filename)
Print "result = " & result
