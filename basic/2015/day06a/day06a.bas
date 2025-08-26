SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

SUB Perform(Grid() AS Integer, Action AS String, R1 AS Integer, C1 AS Integer, R2 AS Integer, C2 AS Integer)
    DIM Row AS Integer
    DIM Col AS Integer

    FOR Row = R1 TO R2
        FOR Col = C1 TO C2
            SELECT CASE Action
                CASE "turn on"
                    Grid(Row, Col) = 1
                CASE "turn off"
                    Grid(Row, Col) = 0
                CASE "toggle"
                    Grid(Row, Col) = 1 - Grid(Row, Col)
            END SELECT
        NEXT
    NEXT
END SUB

FUNCTION Sum(Grid() AS Integer) AS Integer
    DIM Row AS Integer
    DIM Col AS Integer
    DIM Total AS Integer

    Total = 0
    FOR Row = 1 TO 1000
        FOR Col = 1 TO 1000
            Total += Grid(Row, Col)
        NEXT
    NEXT
    RETURN Total
END FUNCTION

FUNCTION Process(FileName AS String) AS Integer
    DIM Grid(1000, 1000) AS Integer
    DIM InFile AS Integer
    DIM LineStr AS String
    DIM Action AS String
    DIM CoordIdx AS Integer
    DIM Coords AS String
    DIM ThroughIdx AS Integer
    DIM Coord1 AS String
    DIM Coord2 AS String
    DIM CommaIdx AS Integer
    DIM R1S AS String
    DIM C1S AS String
    DIM R2S AS String
    DIM C2S AS String
    DIM R1 AS Integer
    DIM C1 AS Integer
    DIM R2 AS Integer
    DIM C2 AS Integer

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, LineStr
        IF Left(LineStr, 7) = "turn on" THEN
            Action = Left(LineStr, 7)
            CoordIdx = 9
        ELSEIF Left(LineStr, 8) = "turn off" THEN
            Action = Left(LineStr, 8)
            CoordIdx = 10
        ELSEIF Left(LineStr, 6) = "toggle" THEN
            Action = Left(LineStr, 6)
            CoordIdx = 8
        ELSE
            PRINT "error: unexpected input"
            System(1)
        END IF
        Coords = Mid(LineStr, CoordIdx)
        ThroughIdx = Instr(1, Coords, " through ")
        Coord1 = Mid(Coords, 1, ThroughIdx - 1)
        CommaIdx = Instr(1, Coord1, ",")
        R1S = Mid(Coord1, 1, CommaIdx - 1)
        C1S = Mid(Coord1, CommaIdx + 1)
        Coord2 = Mid(Coords, ThroughIdx + 9)
        CommaIdx = Instr(1, Coord2, ",")
        R2S = Mid(Coord2, 1, CommaIdx - 1)
        C2S = Mid(Coord2, CommaIdx + 1)
        R1 = ValInt(R1S) + 1
        C1 = ValInt(C1S) + 1
        R2 = ValInt(R2S) + 1
        C2 = ValInt(C2S) + 1
        Perform(Grid(), Action, R1, C1, R2, C2)
    WEND

    CLOSE #InFile
    RETURN Sum(Grid())
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

