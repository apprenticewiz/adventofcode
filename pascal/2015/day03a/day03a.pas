PROGRAM Day03a(Input, Output);

USES
    Generics.Collections;

TYPE
    Position2D = RECORD
        X, Y : 	Integer
    END;

    PositionSet = SPECIALIZE THashSet<Position2D>;

VAR
    FileName : PChar;
    Res      : UInt32;

PROCEDURE Usage();
VAR
    ProgName : PChar;
BEGIN
    ProgName := argv[0];
    WriteLn('usage: ', ProgName, ' <input file>');
    Halt(1)
END;

FUNCTION Process(FileName : PChar) : UInt32;
VAR
    InFile    : Text;
    Ch        : Char;
    Santa     : Position2D;
    Positions : PositionSet;
    Count     : UInt32;

BEGIN
    Santa.X := 0;
    Santa.Y := 0;
    Positions := PositionSet.Create;
    Positions.Add(Santa);
    {$I-}
    Assign(InFile, FileName);
    Reset(InFile);
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'error when opening file: ', FileName);
        Halt(1)
    END;
    {$I+}
    WHILE NOT EOF(InFile) DO
    BEGIN
        Read(InFile, Ch);
        CASE Ch OF
            '^' : Santa.Y := Santa.Y + 1;
            'v' : Santa.Y := Santa.Y - 1;
            '<' : Santa.X := Santa.X - 1;
            '>' : Santa.X := Santa.X + 1;
        END;
        Positions.Add(Santa);
    END;
    Close(InFile);
    Count := Positions.GetCount;
    Positions.Free;
    Process := Count
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
