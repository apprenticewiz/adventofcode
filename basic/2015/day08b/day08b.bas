SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM Result AS Integer = 0
    DIM CodeLen AS Integer
    DIM EncLen AS Integer
    DIM LineStr AS String

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, LineStr
        CodeLen = Len(LineStr)
        EncLen = 0
        FOR I AS Integer = 1 TO Len(LineStr)
            SELECT CASE MID(LineStr, I, 1)
                CASE "\", """"
                    EncLen += 2
                CASE ELSE
                    EncLen += 1
            END SELECT
        NEXT
        Result = Result + 2 + (EncLen - CodeLen)
    WEND
    CLOSE #InFile
    RETURN Result
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

