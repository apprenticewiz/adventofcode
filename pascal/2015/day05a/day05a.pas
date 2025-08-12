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
    Vowels               : SET OF Char;
    VowelCount           : UInt32;
    I                    : SizeInt;

BEGIN
    Vowels := ['a', 'e', 'i', 'o', 'u'];
    VowelCount := 0;
    FOR I := 1 TO Length(S) DO
        IF S[I] IN Vowels THEN
            VowelCount := VowelCount + 1;
    Prop1 := VowelCount >= 3
END;

FUNCTION Prop2(S : String) : Boolean;
VAR
    I                    : SizeInt;
    Found                : Boolean;

BEGIN
    Found := False;
    I := 1;
    WHILE (I < Length(S)) AND NOT Found DO
        IF S[I] = S[I + 1] THEN
            Found := True
        ELSE
            I := I + 1;
    Prop2 := Found
END;

FUNCTION Prop3(S : String) : Boolean;
VAR
    I                    : SizeInt;
    Found                : Boolean;
    Slice                : String;

BEGIN
    Found := False;
    I := 1;
    WHILE (I < Length(S) - 1) AND NOT Found DO
    BEGIN
        Slice := Copy(S, I, 2);
        IF (Slice = 'ab') OR (Slice = 'cd') OR (Slice = 'pq') OR (Slice = 'xy') THEN
            Found := True
        ELSE
            I := I + 1
    END;
    Prop3 := NOT Found
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
        IF Prop1(LineStr) AND Prop2(LineStr) AND Prop3(LineStr) THEN
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
