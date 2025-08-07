(* Note that there are some ugly workarounds below due to bugs in gm2,
   at least as of version 15.1.0.  These mostly seem to involve SHIFT
   and ROTATE with LONGCARD bitsets.  I am calling the internal
   ShiftLeft, ShiftRight, and RotateLeft procedures to work around
   these for now.  Not all of these are strictly necessary, but I am
   being somewhat conservative here.
*)

IMPLEMENTATION MODULE MD5;

FROM DynamicStrings IMPORT String, ConCatChar, InitStringChar, Length, char;
FROM Storage        IMPORT ALLOCATE, DEALLOCATE;
FROM SYSTEM         IMPORT ShiftLeft, ShiftRight, RotateLeft, TSIZE;

PROCEDURE CalcDigest(S : String; VAR Dig : Digest);
VAR
    R                : ARRAY [0..63] OF CARDINAL;
    K                : ARRAY [0..63] OF CARDINAL;
    H0, H1, H2, H3   : CARDINAL;
    InputLen, PadLen : CARDINAL;
    BitLen, ByteVal  : LONGCARD;
    PaddedLen        : CARDINAL;
    Msg              : POINTER TO ARRAY [0..0] OF BYTE;
    I, J, Offset     : CARDINAL;
    W                : ARRAY [0..15] OF CARDINAL;
    A, B, C, D       : CARDINAL;
    F, G             : CARDINAL;
    Temp             : CARDINAL;
    BS1, BS2         : ARRAY [0..0] OF BITSET;
    BSAcc            : BITSET;

BEGIN
    R[ 0] :=  7; R[ 1] := 12; R[ 2] := 17; R[ 3] := 22;
    R[ 4] :=  7; R[ 5] := 12; R[ 6] := 17; R[ 7] := 22;
    R[ 8] :=  7; R[ 9] := 12; R[10] := 17; R[11] := 22;
    R[12] :=  7; R[13] := 12; R[14] := 17; R[15] := 22;
    R[16] :=  5; R[17] :=  9; R[18] := 14; R[19] := 20;
    R[20] :=  5; R[21] :=  9; R[22] := 14; R[23] := 20;
    R[24] :=  5; R[25] :=  9; R[26] := 14; R[27] := 20;
    R[28] :=  5; R[29] :=  9; R[30] := 14; R[31] := 20;
    R[32] :=  4; R[33] := 11; R[34] := 16; R[35] := 23;
    R[36] :=  4; R[37] := 11; R[38] := 16; R[39] := 23;
    R[40] :=  4; R[41] := 11; R[42] := 16; R[43] := 23;
    R[44] :=  4; R[45] := 11; R[46] := 16; R[47] := 23;
    R[48] :=  6; R[49] := 10; R[50] := 15; R[51] := 21;
    R[52] :=  6; R[53] := 10; R[54] := 15; R[55] := 21;
    R[56] :=  6; R[57] := 10; R[58] := 15; R[59] := 21;
    R[60] :=  6; R[61] := 10; R[62] := 15; R[63] := 21;

    K[0] := 3614090360; K[1] := 3905402710; K[2] := 606105819; K[3] := 3250441966; 
    K[4] := 4118548399; K[5] := 1200080426; K[6] := 2821735955; K[7] := 4249261313; 
    K[8] := 1770035416; K[9] := 2336552879; K[10] := 4294925233; K[11] := 2304563134; 
    K[12] := 1804603682; K[13] := 4254626195; K[14] := 2792965006; K[15] := 1236535329; 
    K[16] := 4129170786; K[17] := 3225465664; K[18] := 643717713; K[19] := 3921069994; 
    K[20] := 3593408605; K[21] := 38016083; K[22] := 3634488961; K[23] := 3889429448; 
    K[24] := 568446438; K[25] := 3275163606; K[26] := 4107603335; K[27] := 1163531501; 
    K[28] := 2850285829; K[29] := 4243563512; K[30] := 1735328473; K[31] := 2368359562; 
    K[32] := 4294588738; K[33] := 2272392833; K[34] := 1839030562; K[35] := 4259657740; 
    K[36] := 2763975236; K[37] := 1272893353; K[38] := 4139469664; K[39] := 3200236656; 
    K[40] := 681279174; K[41] := 3936430074; K[42] := 3572445317; K[43] := 76029189; 
    K[44] := 3654602809; K[45] := 3873151461; K[46] := 530742520; K[47] := 3299628645; 
    K[48] := 4096336452; K[49] := 1126891415; K[50] := 2878612391; K[51] := 4237533241; 
    K[52] := 1700485571; K[53] := 2399980690; K[54] := 4293915773; K[55] := 2240044497; 
    K[56] := 1873313359; K[57] := 4264355552; K[58] := 2734768916; K[59] := 1309151649; 
    K[60] := 4149444226; K[61] := 3174756917; K[62] := 718787259; K[63] := 3951481745; 

    H0 := 1732584193;
    H1 := 4023233417;
    H2 := 2562383102;
    H3 := 271733878;

    InputLen := Length(S);
    BitLen := VAL(LONGCARD, InputLen) * 8;
    PadLen := 1 + ((56 - (InputLen + 1) MOD 64) MOD 64);
    PaddedLen := InputLen + PadLen + 8;
    ALLOCATE(Msg, TSIZE(BYTE) * PaddedLen);
    FOR I := 0 TO InputLen - 1 DO
        Msg^[I] := VAL(BYTE, ORD(char(S, I)));
    END;
    Msg^[InputLen] := 128;
    FOR I := 0 TO 7 DO
        BS1[0] := VAL(BITSET, BitLen);
        ShiftRight(BS1, BS2, 64, 8 * I);
        Msg^[InputLen + PadLen + I] := VAL(BYTE, BS2[0]);
    END;
    FOR Offset := 0 TO PaddedLen - 1 BY 64 DO
        FOR I := 0 TO 15 DO
            BSAcc := VAL(BITSET, Msg^[Offset + I * 4]);
            BS1[0] := VAL(BITSET, Msg^[Offset + I * 4 + 1]);
            ShiftLeft(BS1, BS2, 32, 8);
            BSAcc := BSAcc + BS2[0];
            BS1[0] := VAL(BITSET, Msg^[Offset + I * 4 + 2]);
            ShiftLeft(BS1, BS2, 32, 16);
            BSAcc := BSAcc + BS2[0];
            BS1[0] := VAL(BITSET, Msg^[Offset + I * 4 + 3]);
            ShiftLeft(BS1, BS2, 32, 24);
            BSAcc := BSAcc + BS2[0];
            W[I] := VAL(CARDINAL, BSAcc);
        END;
        A := H0;
        B := H1;
        C := H2;
        D := H3;
        FOR I := 0 TO 63 DO
            IF I < 16 THEN
                F := VAL(CARDINAL, (VAL(BITSET, B) * VAL(BITSET, C)) + (({0..31} - VAL(BITSET, B)) * VAL(BITSET, D)));
                G := I;
            ELSIF I < 32 THEN
                F := VAL(CARDINAL, (VAL(BITSET, D) * VAL(BITSET, B)) + (({0..31} - VAL(BITSET, D)) * VAL(BITSET, C)));
                G := (5 * I + 1) MOD 16;
            ELSIF I < 48 THEN
                F := VAL(CARDINAL, VAL(BITSET, B) / VAL(BITSET, C) / VAL(BITSET, D));
                G := (3 * I + 5) MOD 16;
            ELSE
                F := VAL(CARDINAL, VAL(BITSET, C) / (VAL(BITSET, B) + ({0..31} - VAL(BITSET, D))));
                G := (7 * I) MOD 16;
            END;
            Temp := D;
            D := C;
            C := B;
            BS1[0] := VAL(BITSET, A + F + K[I] + W[G]);
            RotateLeft(BS1, BS2, 32, R[I]);
            INC(B, VAL(CARDINAL, BS2[0]));
            A := Temp;
        END;
        INC(H0, A);
        INC(H1, B);
        INC(H2, C);
        INC(H3, D);
    END;
    FOR I := 0 TO 3 DO
        BS1[0] := VAL(BITSET, H0);
        ShiftRight(BS1, BS2, 32, 8 * I);
        BS2[0] := BS2[0] - {8..31};
        Dig[I] := VAL(BYTE, BS2[0]);
        BS1[0] := VAL(BITSET, H1);
        ShiftRight(BS1, BS2, 32, 8 * I);
        BS2[0] := BS2[0] - {8..31};
        Dig[I + 4] := VAL(BYTE, BS2[0]);
        BS1[0] := VAL(BITSET, H2);
        ShiftRight(BS1, BS2, 32, 8 * I);
        BS2[0] := BS2[0] - {8..31};
        Dig[I + 8] := VAL(BYTE, BS2[0]);
        BS1[0] := VAL(BITSET, H3);
        ShiftRight(BS1, BS2, 32, 8 * I);
        BS2[0] := BS2[0] - {8..31};
        Dig[I + 12] := VAL(BYTE, BS2[0]);
    END;
    DEALLOCATE(Msg, TSIZE(BYTE) * PaddedLen);
END CalcDigest;

PROCEDURE DigestToHexString(Dig : Digest) : String;
VAR
    HexDigits : ARRAY [0..15] OF CHAR;
    I         : CARDINAL;
    S         : String;

BEGIN
    FOR I := 0 TO 15 DO
        IF I < 10 THEN
            HexDigits[I] := CHR(ORD('0') + I);
        ELSE
            HexDigits[I] := CHR(ORD('a') + I - 10);
        END;
    END;
    FOR I := 0 TO 15 DO
        IF I = 0 THEN
            S := InitStringChar(HexDigits[VAL(CARDINAL, Dig[I]) DIV 16]);
        ELSE
            S := ConCatChar(S, HexDigits[VAL(CARDINAL, Dig[I]) DIV 16]);
        END;
        S := ConCatChar(S, HexDigits[VAL(CARDINAL, Dig[I]) MOD 16]);
    END;
    RETURN S;
END DigestToHexString;

END MD5.
