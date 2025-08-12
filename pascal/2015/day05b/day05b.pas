PROGRAM Day05a(Input, Output);

USES
    Math;

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

FUNCTION Prop1(S : String) : Boolean;
VAR
    I, J                 : SizeInt;
    Slice1, Slice2       : String;
    Found                : Boolean;

BEGIN
    Found := False;
    I := 1;
    WHILE (I < Length(S) - 2) AND NOT Found DO
    BEGIN
        Slice1 := Copy(S, I, 2);
        J := I + 2;
        WHILE (J < Length(S)) AND NOT Found DO
        BEGIN
            Slice2 := Copy(S, J, 2);
            IF Slice1 = Slice2 THEN
                Found := True
            ELSE
                J := J + 1
        END;
        IF NOT Found THEN
            I := I + 1;
    END;
    Prop1 := Found
END;

FUNCTION Prop2(S : String) : Boolean;
VAR
    I                    : SizeInt;
    Found                : Boolean;

BEGIN
    Found := False;
    I := 1;
    WHILE (I < Length(S) - 1) AND NOT Found DO
        IF S[I] = S[I + 2] THEN
            Found := True
        ELSE
            I := I + 1;
    Prop2 := Found
END;

FUNCTION Process(FileName : PChar) : UInt32;
VAR
    InFile               : Text;
    LineStr              : String;
    Count                : UInt32;

BEGIN
    Count := 0;
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
        ReadLn(InFile, LineStr);
        IF Prop1(LineStr) AND Prop2(LineStr) THEN
            Count := Count + 1
    END;
    Close(InFile);
    Process := Count
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
