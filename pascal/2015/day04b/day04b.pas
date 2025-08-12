PROGRAM Day04b(Input, Output);

USES
    MD5;

VAR
    Key      : PChar;
    Res      : UInt32;

PROCEDURE Usage();
VAR
    ProgName : PChar;
BEGIN
    ProgName := argv[0];
    WriteLn('usage: ', ProgName, ' <key>');
    Halt(1)
END;

FUNCTION Process(Key : PChar) : UInt32;
VAR
    TryKey, NStr, HexDigest : String;
    Digest                  : TMD5Digest;
    N                       : UInt32;
    Found                   : Boolean;

BEGIN
    N := 0;
    Found := False;
    REPEAT
        N := N + 1;
	Str(N, NStr);
        TryKey := Concat(Key, NStr);
        Digest := MD5String(RawByteString(TryKey));
	HexDigest := MD5Print(Digest);
	IF Copy(HexDigest, 1, 6) = '000000' THEN
	    Found := True
    UNTIL Found;
    Process := N
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    Key := argv[1];
    Res := Process(Key);
    WriteLn('result = ', Res)
END.
