SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM TotalLen AS Integer = 0
    DIM S AS String
    DIM FirstX AS Integer
    DIM SecondX AS Integer
    DIM Temp AS String
    DIM Tok AS String
    DIM L AS Integer
    DIM W AS Integer
    DIM H AS INteger
    DIM Perim1 AS Integer
    DIM Perim2 AS Integer
    DIM Perim3 AS Integer
    DIM PresentLen AS Integer
    DIM BowLen AS Integer

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, S
        FirstX = InStr(S, "x")
        Tok = Left(S, FirstX - 1)
        L = ValInt(Tok)
        Temp = Mid(S, FirstX + 1)
        SecondX = InStr(Temp, "x")
        Tok = Left(Temp, SecondX - 1)
        W = ValInt(Tok)
        Tok = Mid(Temp, SecondX + 1)
        H = ValInt(Tok)
        Perim1 = 2 * (L + W)
        Perim2 = 2 * (L + H)
        Perim3 = 2 * (W + H)
	PresentLen = Perim1
        IF Perim2 < PresentLen THEN
            PresentLen = Perim2
        END IF
        IF Perim3 < PresentLen THEN
            PresentLen = Perim3
        END IF
	BowLen = L * W * H
        TotalLen += PresentLen + BowLen
    WEND

    CLOSE #InFile
    RETURN TotalLen
END FUNCTION

DIM Argc AS Integer
DIM Argv AS ZString Ptr Ptr

DIM FileName AS String
DIM Result AS Integer

Argc = __FB_ARGC__
Argv = __FB_ARGV__

IF Argc < 2 THEN
    Usage(*Argv[0])
END IF

FileName = *Argv[1]
Result = Process(FileName)

PRINT "result ="; result

