PROGRAM Day06b(Input, Output);

USES
    Math, RegExpr;

CONST
    ROW_MAX = 1000;
    COL_MAX = 1000;

TYPE
    GridType = ARRAY [1..ROW_MAX, 1..COL_MAX] OF Integer;

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

PROCEDURE InitGrid(VAR Grid : GridType);
VAR
    Row : Integer;
    Col : Integer;
BEGIN
    FOR Row := 1 TO ROW_MAX DO
        FOR Col := 1 TO COL_MAX DO
            Grid[Row, Col] := 0
END;

PROCEDURE Perform(VAR Grid : GridType;
                  Action   : String;
                  R1       : Integer;
                  C1       : Integer;
                  R2       : Integer;
                  C2       : Integer);
VAR
    Row : Integer;
    Col : Integer;
BEGIN
    FOR Row := R1 TO R2 DO
        FOR Col := C1 TO C2 DO
            CASE Action OF
                'turn on'  : Grid[Row, Col] := Grid[Row, Col] + 1;
                'turn off' : Grid[Row, Col] := Max(0, Grid[Row, Col] - 1);
                'toggle'   : Grid[Row, Col] := Grid[Row, Col] + 2;
            END
END;

FUNCTION Sum(Grid : GridType) : UInt32;
VAR
    Row   : Integer;
    Col   : Integer;
    Total : UInt32;
BEGIN
    Total := 0;
    FOR Row := 1 TO ROW_MAX DO
        For Col := 1 TO COL_MAX DO
           Total := Total + Grid[Row, Col];
    Sum := Total
END;

FUNCTION Process(FileName : PChar) : UInt32;
VAR
    Grid      : GridType;
    RegExp    : TRegExpr;
    InFile    : Text;
    LineStr   : String;
    Action    : String;
    Num       : Integer;
    Code      : Integer;
    R1        : Integer;
    C1        : Integer;
    R2        : Integer;
    C2        : Integer;

BEGIN
    InitGrid(Grid);
    RegExp := TRegExpr.Create;
    RegExp.Expression := '(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)';
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
        IF RegExp.Exec(LineStr) THEN
	BEGIN
	    Action := RegExp.Match[1];
	    Val(RegExp.Match[2], Num, Code);
	    R1 := Num;
	    Val(RegExp.Match[3], Num, Code);
	    C1 := Num;
	    Val(RegExp.Match[4], Num, Code);
	    R2 := Num;
	    Val(RegExp.Match[5], Num, Code);
	    C2 := Num;
	    Perform(Grid, Action, R1, C1, R2, C2)
	END
    END;
    Close(InFile);
    RegExp.Free;
    Process := Sum(Grid)
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
