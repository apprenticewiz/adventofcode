Imports System
Imports System.IO

Module Program
	Sub Usage()
		Console.Error.WriteLine("usage: dotnet run <input file>")
		Environment.Exit(1)
	End Sub

	Function Process(filename As String) As Integer
		Dim counter As Integer = 0
		Dim content As String = File.ReadAllText(filename)
		For Each ch As Char In content
			If ch = "("c Then
				counter += 1
			ElseIf ch = ")"c Then
				counter -= 1
			End If
		Next
		Return counter
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
