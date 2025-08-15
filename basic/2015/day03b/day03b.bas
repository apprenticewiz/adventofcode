TYPE Position
    X AS Integer
    Y AS Integer
END TYPE

TYPE PositionSet
    Positions(16384) AS String
    Count AS Integer
END TYPE

FUNCTION SetInit() AS PositionSet
    DIM NewSet AS PositionSet

    NewSet.Count = 0
    RETURN NewSet
END FUNCTION

FUNCTION SetFind(PS AS PositionSet, P AS Position) AS Boolean
    DIM KeyString AS String
    DIM I AS Integer

    KeyString = "(" & STR(P.X) & "," & STR(P.Y) & ")"
    FOR I = 1 TO PS.Count
        IF PS.Positions(I) = KeyString THEN
            RETURN True
        END IF
    NEXT
    RETURN False
END FUNCTION

SUB SetInsert(BYREF PS AS PositionSet, P AS Position)
    DIM KeyString AS String

    KeyString = "(" & STR(P.X) & "," & STR(P.Y) & ")"
    IF PS.Count < 16384 AND NOT SetFind(PS, P) THEN
        PS.Count += 1
        PS.Positions(PS.Count) = KeyString
    END IF
END SUB

FUNCTION SetSize(PS AS PositionSet) AS Integer
    RETURN PS.Count
END FUNCTION

SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM Ch AS String
    DIM Santa AS Position
    DIM RoboSanta AS Position
    DIM Positions AS PositionSet
    DIM SantaMove AS Boolean

    Santa.X = 0
    Santa.Y = 0
    RoboSanta.X = 0
    RoboSanta.Y = 0
    Positions = SetInit()
    SetInsert(Positions, Santa)
    SantaMove = TRUE

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        Ch = INPUT(1, #InFile)
        IF SantaMove THEN
            SELECT CASE Ch
                CASE "^"
                    Santa.Y += 1
                CASE "v"
                    Santa.Y -= 1
                CASE "<"
                    Santa.X -= 1
                CASE ">"
                    Santa.X += 1
            END SELECT
            SetInsert(Positions, Santa)
        ELSE
            SELECT CASE Ch
                CASE "^"
                    RoboSanta.Y += 1
                CASE "v"
                    RoboSanta.Y -= 1
                CASE "<"
                    RoboSanta.X -= 1
                CASE ">"
                    RoboSanta.X += 1
            END SELECT
            SetInsert(Positions, RoboSanta)
        END IF
	SantaMove = NOT SantaMove
    WEND

    CLOSE #InFile
    RETURN SetSize(Positions)
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

