PROGRAM Day02b(Input, Output);

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
    InFile                 : Text;
    LineStr                : String;
    TotalLen               : UInt32;
    FirstX, SecondX        : SizeInt;
    Tok                    : String;
    L, W, H                : UInt32;
    Perim1, Perim2, Perim3 : UInt32;
    PresentLen, BowLen     : UInt32;

BEGIN
    TotalLen := 0;
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
        Perim1 := 2 * (L + W);
        Perim2 := 2 * (L + H);
        Perim3 := 2 * (W + H);
        PresentLen := Min(Perim1, Min(Perim2, Perim3));
        BowLen := L * W * H;
        TotalLen := TotalLen + PresentLen + BowLen;
    END;
    Close(InFile);
    Process := TotalLen;
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
