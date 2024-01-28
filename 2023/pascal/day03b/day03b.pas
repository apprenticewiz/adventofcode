PROGRAM Day03a(INPUT, OUTPUT);
{$H+}

USES StrUtils, SysUtils;

TYPE
    Position = RECORD
        Row : INTEGER;
        Col : INTEGER;
    END;

    NumberLocation = RECORD
        Pos : Position;
        Number : ARRAY [1..3] OF CHAR;
    END;

    PartLocation = RECORD
        Pos : Position;
        Part : CHAR;
    END;

    NodeValueType = (Number, Part);

    NodeData = RECORD
        CASE ValueType : NodeValueType OF
            Number : (NumberLoc : NumberLocation);
            Part : (PartLoc : PartLocation); 
    END;

    Node = RECORD
        Val  : NodeData;
        Next : ^Node;
    END;

    NodePtr = ^Node;

VAR
    FileName : STRING;
    Res      : INT32;

PROCEDURE AppendList(VAR List : NodePtr; Val : NodeData);
VAR
    Curr, Temp : NodePtr;
BEGIN
    New(Temp);
    Temp^.Val := Val;
    Temp^.Next := NIL;
    Curr := List;
    IF List = NIL THEN
        List := Temp
    ELSE
    BEGIN
        WHILE Curr^.Next <> NIL DO
            Curr := Curr^.Next;
        Curr^.Next := Temp
    END
END;

PROCEDURE FreeList(List : NodePtr);
VAR
    Curr, Temp : NodePtr;
BEGIN
    Curr := List;
    WHILE Curr <> NIL DO
    BEGIN
        Temp := Curr;
        Curr := Curr^.Next;
        Dispose(Temp)
    END
END;

PROCEDURE Usage;
BEGIN
    WriteLn('usage: ', ExtractFileName(paramStr(0)), ' <file>');
    Halt(1)
END;

FUNCTION BuildNumbers(FileName : STRING) : NodePtr;
VAR
    InFile         : TEXT;
    FileLine       : STRING;
    Row            : INTEGER = 1;
    Col            : INTEGER;
    Ch             : CHAR;
    ScanningNumber : BOOLEAN = False;
    NumIdx         : INTEGER = 1;
    NumLoc         : NumberLocation;
    NodeVal        : NodeData;
    NumLocs        : NodePtr = NIL;
BEGIN
    Assign(InFile, FileName);
    Reset(InFile);
    WHILE NOT Eof(InFile) DO
    BEGIN
        ReadLn(InFile, FileLine);
        FOR Col := 1 TO Length(FileLine) DO
        BEGIN
            Ch := FileLine[Col];
            IF ScanningNumber THEN
                IF (Ch >= '0') AND (Ch <= '9') THEN
                BEGIN
                    NumLoc.Number[NumIdx] := Ch;
                    NumIdx := NumIdx + 1
                END
                ELSE
                BEGIN
                    WHILE NumIdx <= 3 DO
                    BEGIN
                        NumLoc.Number[NumIdx] := ' ';
                        NumIdx := NumIdx + 1
                    END;
                    NodeVal.ValueType := Number;
                    NodeVal.NumberLoc := NumLoc;
                    AppendList(NumLocs, NodeVal);
                    NumIdx := 1;
                    ScanningNumber := False
                END
            ELSE
                IF (Ch >= '0') AND (Ch <= '9') THEN
                BEGIN
                    NumLoc.Number[NumIdx] := Ch;
                    NumIdx := NumIdx + 1;
                    NumLoc.Pos.Row := Row;
                    NumLoc.Pos.Col := Col;
                    ScanningNumber := True
                END
        END;
        IF ScanningNumber THEN
        BEGIN
            WHILE NumIdx <= 3 DO
            BEGIN
                NumLoc.Number[NumIdx] := ' ';
                NumIdx := NumIdx + 1
            END;
            NodeVal.ValueType := Number;
            NodeVal.NumberLoc := NumLoc;
            AppendList(NumLocs, NodeVal);
            NumIdx := 1;
            ScanningNumber := False
        END;
        Row := Row + 1
    END;
    Close(InFile);
    BuildNumbers := NumLocs
END;

FUNCTION BuildParts(FileName : STRING) : NodePtr;
VAR
    InFile         : TEXT;
    FileLine       : STRING;
    Row            : INTEGER = 1;
    Col            : INTEGER;
    Ch             : CHAR;
    PartLoc        : PartLocation;
    NodeVal        : NodeData;
    PartLocs       : NodePtr = NIL;
BEGIN
    Assign(InFile, FileName);
    Reset(InFile);
    WHILE NOT Eof(InFile) DO
    BEGIN
        ReadLn(InFile, FileLine);
        FOR Col := 1 TO Length(FileLine) DO
        BEGIN
            Ch := FileLine[Col];
            IF Ch = '*' THEN
            BEGIN
                PartLoc.Pos.Row := Row;
                PartLoc.Pos.Col := Col;
                PartLoc.Part := Ch;
                NodeVal.ValueType := Part;
                NodeVal.PartLoc := PartLoc;
                AppendList(PartLocs, NodeVal)
            END
        END;
        Row := Row + 1
    END;
    Close(InFile);
    BuildParts := PartLocs
END;

FUNCTION CheckParts(NumberLocs : NodePtr; PartLocs : NodePtr) : INT32;
VAR
    Res                : INT32 = 0;
    NumLocPtr          : NodePtr;
    PartLocPtr         : NodePtr;
    NumLoc             : NumberLocation;
    PartLoc            : PartLocation;
    Found              : BOOLEAN;
    I, J               : INTEGER;
    NumCol             : INTEGER;
    DeltaRow, AdjRow   : INTEGER;
    DeltaCol, AdjCol   : INTEGER;
    AdjCount           : INTEGER;
    NumVal             : INTEGER;
    Prod               : INT32;
BEGIN
    PartLocPtr := PartLocs;
    WHILE PartLocPtr <> NIL DO
    BEGIN
        PartLoc := PartLocPtr^.Val.PartLoc;
        AdjCount := 0;
        Prod := 1;
        NumLocPtr := NumberLocs;
        WHILE NumLocPtr <> NIL DO
        BEGIN
            NumLoc := NumLocPtr^.Val.NumberLoc;
            Found := False;
            FOR DeltaRow := -1 TO 1 DO
            BEGIN
                AdjRow := PartLoc.Pos.Row + DeltaRow;
                FOR DeltaCol := -1 TO 1 DO
                BEGIN
                    AdjCol := PartLoc.Pos.Col + DeltaCol;
                    FOR I := 0 TO 2 DO
                    BEGIN
                        IF NumLoc.Number[I + 1] = ' ' THEN
                            BREAK;
                        NumCol := NumLoc.Pos.Col + I;
                        IF (AdjRow = NumLoc.Pos.Row) AND (AdjCol = NumCol) THEN
                        BEGIN
                            AdjCount := AdjCount + 1;
                            NumVal := 0;
                            FOR J := 1 TO 3 DO
                                IF (NumLoc.Number[J] >= '0') AND (NumLoc.Number[J] <= '9') THEN
                                    NumVal := NumVal * 10 + Ord(NumLoc.Number[J]) - Ord('0');
                            Prod := Prod * NumVal;
                            Found := True;
                            BREAK
                        END;
                    END;
                    IF Found THEN
                        BREAK
                END;
                IF Found THEN
                    BREAK
            END;
            NumLocPtr := NumLocPtr^.Next
        END;
        IF AdjCount = 2 THEN
            Res := Res + Prod;
        PartLocPtr := PartLocPtr^.Next
    END;
    CheckParts := Res
END;

FUNCTION Process(FileName : STRING) : INT32;
VAR
    NumberLocs : NodePtr;
    PartLocs   : NodePtr;
    Res        : INT32;
BEGIN
    NumberLocs := BuildNumbers(FileName);
    PartLocs := BuildParts(FileName);
    Res := CheckParts(NumberLocs, PartLocs);
    FreeList(NumberLocs);
    FreeList(PartLocs);
    Process := Res
END;

BEGIN
    IF paramCount() < 1 THEN
        Usage;
    FileName := paramStr(1);
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
