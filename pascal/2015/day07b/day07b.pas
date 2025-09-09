PROGRAM Day07a(Input, Output);

USES
    Generics.Collections, RegExpr;

TYPE
    OperType = (AssignOp, NotOp, AndOp, OrOp, LeftShiftOp, RightShiftOp);

    Operation = RECORD
        Oper     : OperType;
        Source1  : String;
        Source2  : String;
        Amount   : Integer;
    END;

    OperationTable = SPECIALIZE TDictionary<String, Operation>;
    CacheTable = SPECIALIZE TDictionary<String, Word>;

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

FUNCTION Eval(VAR Ops : OperationTable; VAR Cache : CacheTable; Expr : String) : Word;
VAR
    Code         : Integer;
    Op           : Operation;
    Num, R, A, B : Word;

BEGIN
    Val(Expr, Num, Code);
    IF Code = 0 THEN
        Eval := Num
    ELSE IF Cache.ContainsKey(Expr) THEN
        Eval := Cache[Expr]
    ELSE
    BEGIN
        Op := Ops[Expr];
        R := 0;
        CASE Op.Oper OF
            AssignOp :
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    R := A
                END;
            NotOp:
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    R := NOT A
                END;
            AndOp:
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    B := Eval(Ops, Cache, Op.Source2);
                    R := A AND B
                END;
            OrOp:
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    B := Eval(Ops, Cache, Op.Source2);
                    R := A OR B
                END;
            LeftShiftOp:
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    R := A SHL Op.Amount
                END;
             RightShiftOp:
                BEGIN
                    A := Eval(Ops, Cache, Op.Source1);
                    R := A SHR Op.Amount
                END
        END;
        Cache.AddOrSetValue(Expr, R);
        Eval := R
    END
END;

FUNCTION Process(FileName : PChar) : UInt32;
VAR
    Operations          : OperationTable;
    Op                  : Operation;
    OpStr, Dest         : String;
    Cache               : CacheTable;
    RE1, RE2, RE3, RE4  : TRegExpr;
    InFile              : Text;
    LineStr             : String;
    Num, Code           : Integer;
    A                   : Word;

BEGIN
    Operations := OperationTable.Create;
    Cache := CacheTable.Create;
    RE1 := TRegExpr.Create;
    RE1.Expression := '^(\d+|\w+) -> (\w+)$';
    RE2 := TRegExpr.Create;
    RE2.Expression := 'NOT (\d+|\w+) -> (\w+)';
    RE3 := TRegExpr.Create;
    RE3.Expression := '(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)';
    RE4 := TRegExpr.Create;
    RE4.Expression := '(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)';
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
        IF RE1.Exec(LineStr) THEN
        BEGIN
            Op.Oper := AssignOp;
            Op.Source1 := RE1.Match[1];
            Dest := RE1.Match[2]
        END;
        IF RE2.Exec(LineStr) THEN
        BEGIN
            Op.Oper := NotOp;
            Op.Source1 := RE2.Match[1];
            Dest := RE2.Match[2]
        END;
        IF RE3.Exec(LineStr) THEN
        BEGIN
            OpStr := RE3.Match[2];
            IF OpStr = 'AND' THEN
                Op.Oper := AndOp
            ELSE
                Op.Oper := OrOp;
            Op.Source1 := RE3.Match[1];
            Op.Source2 := RE3.Match[3];
            Dest := RE3.Match[4]
        END;
        IF RE4.Exec(LineStr) THEN
        BEGIN
            OpStr := RE4.Match[2];
            IF OpStr = 'LSHIFT' THEN
                Op.Oper := LeftShiftOp
            ELSE
                Op.Oper := RightShiftOp;
            Op.Source1 := RE4.Match[1];
            Val(RE4.Match[3], Num, Code);
            Op.Amount := Num;
            Dest := RE4.Match[4]
        END;
        Operations.Add(Dest, Op)
    END;
    Close(InFile);
    RE1.Free;
    RE2.Free;
    RE3.Free;
    RE4.Free;
    A := Eval(Operations, Cache, 'a');
    Op.Oper := AssignOp;
    Str(A, Op.Source1);
    Operations.AddOrSetvalue('b', Op);
    Cache.Clear;
    Process := Eval(Operations, Cache, 'a')
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.
