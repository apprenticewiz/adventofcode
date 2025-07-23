Imports System
Imports System.Collections.Generic
Imports System.IO

Imports AOC_Utils.Geometry

Module Program
	Sub Usage()
		Console.Error.WriteLine("usage: dotnet run <input file>")
		Environment.Exit(1)
	End Sub

	Function Process(filename As String) As Integer
		Dim positions As New HashSet(Of Position2D(Of Integer))()
		Dim santa As New Position2D(Of Integer)(0, 0)
		Dim roboSanta As New Position2D(Of Integer)(0, 0)
		Dim i As Integer = 0
		positions.Add(santa)
		Try
			Using reader As New StreamReader(filename)
				Dim line As String
				line = reader.ReadLine()
				While line IsNot Nothing
					For Each ch As Char In line
						If i Mod 2 = 0 Then
							Select Case ch
								Case "^"c
									santa.Y += 1
								Case "v"c
									santa.Y -= 1
								Case "<"c
									santa.X -= 1
								Case ">"c
									santa.X += 1
							End Select
							positions.Add(santa)
						Else
							Select Case ch
								Case "^"c
									roboSanta.Y += 1
								Case "v"c
									roboSanta.Y -= 1
								Case "<"c
									roboSanta.X -= 1
								Case ">"c
									roboSanta.X += 1
							End Select
							positions.Add(roboSanta)
						End If
						i += 1
					Next
					line = reader.ReadLine()
				End While
			End Using
		Catch e As IOException
			Console.Error.WriteLine("Error reading file:" & e.Message)
			Environment.Exit(1)
		End Try

		Return positions.Count
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
