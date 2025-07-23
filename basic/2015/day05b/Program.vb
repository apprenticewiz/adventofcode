Imports System
Imports System.IO

Module Program
	Sub Usage()
		Console.Error.WriteLine("usage: dotnet run <input file>")
		Environment.Exit(1)
	End Sub

	Function Prop1(line As String) As Boolean
		For i As Integer = 0 To line.Length - 4
			Dim firstPair As String = line.Substring(i, 2)
			For j As Integer = i + 2 To line.length - 2
				Dim secondPair As String = line.Substring(j, 2)
				If firstPair.equals(secondPair) Then
					Return True
				End If
			Next
		Next
		Return False
	End Function

	Function Prop2(line As String) As Boolean
		For i As Integer = 0 To line.Length - 3
			If line(i) = line(i + 2) Then
				Return True
			End If
		Next
		Return False
	End Function

	Function Process(filename As String) As Integer
		Dim n As Integer = 0
		Try
			Using reader As New StreamReader(filename)
				Dim line As String
				line = reader.ReadLine()
				While line IsNot Nothing
					If Prop1(line) AndAlso Prop2(line)
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
