#include "crt/stdlib.bi"

Sub usage(progname As String)
    Print "usage: " & progname & " <input file>"
    End 1
End Sub

Function process(filename As String) As Integer
    Dim totalArea As Integer = 0
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
        Dim area1 As Integer
        Dim area2 As Integer
        Dim area3 As Integer
        Dim surfaceArea as Integer
        Dim minArea as Integer

        l = Val(Left(lineText, x1 -1))
        w = Val(Mid(lineText, x1 + 1, x2 - x1 - 1))
        h = Val(Mid(lineText, x2 + 1))

        area1 = l * w
        area2 = l * h
        area3 = w * h

        surfaceArea = (2 * area1) + (2 * area2) + (2 * area3)
        minArea = area1
        If area2 < minArea Then
            minArea = area2
        End If
        If area3 < minArea Then
            minArea = area3
        End If

        totalArea += surfaceArea + minArea
    Loop

    Close #f
    Return totalArea
End Function

Dim argc As Integer = __FB_ARGC__
Dim argv As ZString Ptr Ptr = __FB_ARGV__

If argc < 2 Then
    usage(*argv[0])
End If

Dim filename As String = *argv[1]
Dim result As Integer = process(filename)
Print "result = " & result
