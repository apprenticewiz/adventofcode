MODULE day03b;

FROM DynamicStrings  IMPORT String;
FROM FIO             IMPORT File, Close, EOF, IsNoError, ReadChar;
FROM InOut           IMPORT WriteCard, WriteLn, WriteString;
FROM libc            IMPORT exit;
FROM SFIO            IMPORT OpenToRead;

FROM Args            IMPORT ArgCount, GetArgument;
FROM DynamicStringIO IMPORT WriteDynString;
FROM Geometry        IMPORT Position2D, Position2DToString;
FROM StringSet       IMPORT StringSet, Create, Delete, Insert, Count;

VAR
    Argc          : CARDINAL;
    ProgName      : String;
    FileName      : String;
    Result        : CARDINAL;

PROCEDURE Usage(ProgName : String);
BEGIN
    WriteString('usage: ');
    WriteDynString(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile        : File;
    Ch            : CHAR;
    Santa         : Position2D;
    RoboSanta     : Position2D;
    Key           : String;
    Positions     : StringSet;
    SantaMove     : BOOLEAN;
    Total         : CARDINAL;

BEGIN
    Santa.X := 0;
    Santa.Y := 0;
    RoboSanta.X := 0;
    RoboSanta.Y := 0;
    Positions := Create();
    Key := Position2DToString(Santa);
    Insert(Positions, Key);
    SantaMove := FALSE;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        WriteDynString(FileName);
        WriteLn;
        exit(1);
    END;
    REPEAT
        Ch := ReadChar(InFile);
        IF SantaMove THEN
            CASE Ch OF
                '^': INC(Santa.Y); |
                'v': DEC(Santa.Y); |
                '<': DEC(Santa.X); |
                '>': INC(Santa.X);
            END;
            Key := Position2DToString(Santa);
            Insert(Positions, Key);
        ELSE
            CASE Ch OF
                '^': INC(RoboSanta.Y); |
                'v': DEC(RoboSanta.Y); |
                '<': DEC(RoboSanta.X); |
                '>': INC(RoboSanta.X);
            END;
            Key := Position2DToString(RoboSanta);
            Insert(Positions, Key);
        END;
        SantaMove := NOT SantaMove;
    UNTIL EOF(InFile);
    Close(InFile);
    Total := Count(Positions);
    Delete(Positions);
    RETURN Total;
END Process;

BEGIN
    Argc := ArgCount();
    ProgName := GetArgument(0);
    IF Argc < 2 THEN
        Usage(ProgName);
    END;
    FileName := GetArgument(1);
    Result := Process(FileName);
    WriteString('result = ');
    WriteCard(Result, 1);
    WriteLn;
END day03b.
