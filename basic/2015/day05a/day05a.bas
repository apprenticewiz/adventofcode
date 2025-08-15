SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Prop1(S AS String) AS Boolean
    DIM Vowels AS Integer
    DIM I AS Integer

    Vowels = 0
    FOR I = 1 TO Len(S)
        SELECT CASE Mid(S, I, 1)
            CASE "a", "e", "i", "o", "u"
                Vowels += 1
        END SELECT
    NEXT
    RETURN Vowels >= 3
END FUNCTION

FUNCTION Prop2(S AS String) AS Boolean
    DIM I As Integer

    FOR I = 1 TO Len(S) - 1
        IF Mid(S, I, 1) = Mid(S, I + 1, 1) THEN
            RETURN True
        END IF
    NEXT

    RETURN False
END FUNCTION

FUNCTION Prop3(S AS String) AS Boolean
    RETURN InStr(S, "ab") = 0 AND InStr(S, "cd") = 0 AND InStr(S, "pq") = 0 AND InStr(S, "xy") = 0
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
        IF Prop1(S) AND Prop2(S) AND Prop3(S) THEN
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

