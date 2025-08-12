PROGRAM Day02a(Input, Output);

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

FUNCTION Process(FileName : PChar) : UInt32;
VAR
    InFile               : Text;
    LineStr              : String;
    TotalArea            : UInt32;
    FirstX, SecondX      : SizeInt;
    Tok                  : String;
    L, W, H              : UInt32;
    Area1, Area2, Area3  : UInt32;
    SurfaceArea, MinArea : UInt32;

BEGIN
    TotalArea := 0;
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
        FirstX := Pos('x', LineStr);
        Tok := Copy(LineStr, 1, FirstX - 1);
        Val(Tok, L);
        SecondX := Pos('x', LineStr, FirstX + 1);
        Tok := Copy(LineStr, FirstX + 1, SecondX - FirstX - 1);
        Val(Tok, W);
        Tok := Copy(LineStr, SecondX + 1);
        Val(Tok, H);
        Area1 := L * W;
        Area2 := L * H;
        Area3 := W * H;
        SurfaceArea := 2 * Area1 + 2 * Area2 + 2 * Area3;
        MinArea := Min(Area1, Min(Area2, Area3));
        TotalArea := TotalArea + SurfaceArea + MinArea;
    END;
    Close(InFile);
    Process := TotalArea;
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
