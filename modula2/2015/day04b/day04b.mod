MODULE day04b;

FROM DynamicStrings  IMPORT String, ConCat, Dup, Fin, char;
FROM InOut           IMPORT WriteInt, WriteLn, WriteString;
FROM libc            IMPORT exit;
FROM StringConvert   IMPORT CardinalToString;

FROM Args            IMPORT ArgCount, GetArgument;
FROM DynamicStringIO IMPORT WriteDynString;
FROM MD5             IMPORT Digest, CalcDigest, DigestToHexString;

CONST
    NumZeros = 6;

VAR
    Argc          : CARDINAL;
    ProgName      : String;
    FileName      : String;
    Result        : INTEGER;

PROCEDURE Usage(ProgName : String);
BEGIN
    WriteString('usage: ');
    WriteDynString(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE Process(Key : String) : CARDINAL;
VAR
    N, I, Zeros   : CARDINAL;
    TryKey, Hex   : String;
    Dig           : Digest;

BEGIN
    N := 1;
    LOOP
        TryKey := Dup(Key);
        TryKey := ConCat(TryKey, CardinalToString(N, 1, ' ', 10, FALSE));
        CalcDigest(TryKey, Dig);
        Hex := DigestToHexString(Dig);
        Zeros := 0;
        FOR I := 0 TO (NumZeros - 1) DO
            IF char(Hex, I) = '0' THEN
                INC(Zeros);
            END;
        END;
        IF Zeros = NumZeros THEN
            EXIT;
        ELSE
            INC(N);
        END;
        Fin(TryKey);
        Fin(Hex);
    END;
    RETURN N;
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
    WriteInt(Result, 1);
    WriteLn;

END day04b.
