SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM Result AS Integer = 0
    DIM CodeLen AS Integer
    DIM MemLen AS Integer
    DIM LineStr AS String
    DIM I AS Integer

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, LineStr
        CodeLen = Len(LineStr)
        MemLen = 0
        I = 2
        WHILE I < Len(LineStr)
            SELECT CASE MID(LineStr, I, 1)
                CASE "\"
                    SELECT CASE MID(LineStr, I + 1, 1)
                        CASE "\", """"
                            I += 2
                        CASE "x"
                            I += 4
                        CASE ELSE
                            I += 1
                    END SELECT
                CASE ELSE
                    I += 1
            END SELECT
            MemLen += 1
        WEND
        Result = Result + (CodeLen - MemLen)
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

