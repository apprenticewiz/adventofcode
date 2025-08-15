SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM Floor AS Integer = 0
    DIM Ch AS String

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        Ch = INPUT(1, #InFile)
        SELECT CASE Ch
            CASE "("
                Floor += 1
            CASE ")"
                Floor -= 1
        END SELECT
    WEND

    CLOSE #InFile
    RETURN Floor
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

