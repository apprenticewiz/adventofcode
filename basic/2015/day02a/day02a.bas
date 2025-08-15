SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM TotalArea AS Integer = 0
    DIM S AS String
    DIM FirstX AS Integer
    DIM SecondX AS Integer
    DIM Temp AS String
    DIM Tok AS String
    DIM L AS Integer
    DIM W AS Integer
    DIM H AS INteger
    DIM Area1 AS Integer
    DIM Area2 AS Integer
    DIM Area3 AS Integer
    DIM SurfaceArea AS Integer
    DIM MinArea AS Integer

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
        Area1 = L * W
        Area2 = L * H
        Area3 = W * H
        SurfaceArea = (2 * Area1) + (2 * Area2) + (2 * Area3)
        MinArea = Area1
        IF Area2 < MinArea THEN
            MinArea = Area2
        END IF
        IF Area3 < MinArea THEN
            MinArea = Area3
        END IF
        TotalArea += SurfaceArea + MinArea
    WEND

    CLOSE #InFile
    RETURN TotalArea
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

