IMPLEMENTATION MODULE DynamicStringIO;

FROM DynamicStrings IMPORT String, Length, char;
FROM InOut IMPORT Write;

PROCEDURE WriteDynString(S : String);
VAR
    I, Last : CARDINAL;
BEGIN
    Last := Length(S) - 1;
    FOR I := 0 TO Last DO
        Write(char(S, I));
    END;
END WriteDynString;

END DynamicStringIO.
