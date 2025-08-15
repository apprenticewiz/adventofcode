SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Prop1(S AS String) AS Boolean
    DIM I AS Integer
    DIM Pair AS String

    FOR I = 1 TO Len(S) - 3
        Pair = Mid(S, I, 2)
        IF InStr(I + 2, S, Pair) THEN
            RETURN True
        END IF
    NEXT
    RETURN False
END FUNCTION

FUNCTION Prop2(S AS String) AS Boolean
    DIM I As Integer

    FOR I = 1 TO Len(S) - 2
        IF Mid(S, I, 1) = Mid(S, I + 2, 1) THEN
            RETURN True
        END IF
    NEXT

    RETURN False
END FUNCTION

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM S AS String
    DIM Count AS Integer

    Count = 0
    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, S
        IF Prop1(S) AND Prop2(S) THEN
            Count += 1
        END IF
    WEND

    CLOSE #InFile
    RETURN Count
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

