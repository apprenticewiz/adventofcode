IMPLEMENTATION MODULE DynamicStringFIO;

FROM DynamicStrings IMPORT String, ConCatChar, InitStringChar;
FROM FIO            IMPORT File, EOF, ReadChar, WasEOLN;

PROCEDURE ReadDynString(F : File) : String;
VAR
    S           : String;
    Initialized : BOOLEAN;
    Ch           : CHAR;
BEGIN
    Initialized := FALSE;
    LOOP
       Ch := ReadChar(F);
       IF WasEOLN(F) THEN
           EXIT;
       END;
       IF NOT Initialized THEN
           S := InitStringChar(Ch);
           Initialized := TRUE;
       ELSE
           S := ConCatChar(S, Ch);
       END;
      IF EOF(F) THEN
          EXIT;
      END;
    END;
    RETURN S;
END ReadDynString;

END DynamicStringFIO.
