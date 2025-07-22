#include "crt/stdlib.bi"

Sub usage(progname As String)
    Print "usage: " & progname & " <input file>"
    End 1
End Sub

Function process(filename As String) As Integer
    Dim totalLen As Integer = 0
    Dim lineText As String
    Dim f As Integer

    f = FreeFile()
    If Open(filename For Input As #f) <> 0 Then
        Print "Error opening file: " & filename
        End 1
    End If

    Do While Not Eof(f)
        Line Input #f, lineText
        Dim x1 As Integer = Instr(lineText, "x")
        Dim x2 As Integer = Instr(x1 + 1, lineText, "x")
        Dim l As Integer
        Dim w As Integer
        Dim h As Integer
        Dim perim1 As Integer
        Dim perim2 As Integer
        Dim perim3 As Integer
        Dim presentLen as Integer
        Dim bowLen as Integer

        l = Val(Left(lineText, x1 -1))
        w = Val(Mid(lineText, x1 + 1, x2 - x1 - 1))
        h = Val(Mid(lineText, x2 + 1))

        perim1 = 2 * (l + w)
        perim2 = 2 * (l + h)
        perim3 = 2 * (w + h)

        presentLen = perim1
        If perim2 < presentLen Then
            presentLen = perim2
        End If
        If perim3 < presentLen Then
            presentLen = perim3
        End If

        bowLen = l * w * h

        totalLen += presentLen + bowLen
    Loop

    Close #f
    Return totalLen
End Function

Dim argc As Integer = __FB_ARGC__
Dim argv As ZString Ptr Ptr = __FB_ARGV__

If argc < 2 Then
    usage(*argv[0])
End If

Dim filename As String = *argv[1]
Dim result As Integer = process(filename)
Print "result = " & result
