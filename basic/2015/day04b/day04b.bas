FUNCTION RotateLeft(X AS ULong, C AS ULong) AS ULong
    RETURN ((X Shl C) OR (X Shr (32 - C)))
END FUNCTION

FUNCTION MD5Digest(BYREF InputStr AS String) AS String
    DIM AS String * 33 Digest = Space(32)
    DIM AS ULong R(0 To 63) = { _
        7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22, _
        5, 9,14,20, 5, 9,14,20, 5, 9,14,20, 5, 9,14,20, _
        4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23, _
        6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21 }

    DIM AS ULong K(0 To 63) = { _
        &Hd76aa478, &He8c7b756, &H242070db, &Hc1bdceee, &Hf57c0faf, &H4787c62a, _
        &Ha8304613, &Hfd469501, &H698098d8, &H8b44f7af, &Hffff5bb1, &H895cd7be, _
        &H6b901122, &Hfd987193, &Ha679438e, &H49b40821, &Hf61e2562, &Hc040b340, _
        &H265e5a51, &He9b6c7aa, &Hd62f105d, &H02441453, &Hd8a1e681, &He7d3fbc8, _
        &H21e1cde6, &Hc33707d6, &Hf4d50d87, &H455a14ed, &Ha9e3e905, &Hfcefa3f8, _
        &H676f02d9, &H8d2a4c8a, &Hfffa3942, &H8771f681, &H6d9d6122, &Hfde5380c, _
        &Ha4beea44, &H4bdecfa9, &Hf6bb4b60, &Hbebfbc70, &H289b7ec6, &Heaa127fa, _
        &Hd4ef3085, &H04881d05, &Hd9d4d039, &He6db99e5, &H1fa27cf8, &Hc4ac5665, _
        &Hf4292244, &H432aff97, &Hab9423a7, &Hfc93a039, &H655b59c3, &H8f0ccc92, _
        &Hffeff47d, &H85845dd1, &H6fa87e4f, &Hfe2ce6e0, &Ha3014314, &H4e0811a1, _
        &Hf7537e82, &Hbd3af235, &H2ad7d2bb, &Heb86d391 }

    DIM AS ULong H0 = &H67452301
    DIM AS ULong H1 = &Hefcdab89
    DIM AS ULong H2 = &H98badcfe
    DIM AS ULong H3 = &H10325476

    DIM AS Integer InputLen = Len(InputStr)
    DIM AS ULongInt BitLen = InputLen * 8ULL
    DIM AS Integer PadLen = 1 + ((56 - ((InputLen + 1) Mod 64)) Mod 64)
    DIM AS Integer PaddedLen = InputLen + PadLen + 8

    DIM AS UByte Ptr Msg = ALLOCATE(PaddedLen)
    IF Msg = 0 THEN
        PRINT "memory allocation failed"
	System(1)
    END IF
    FOR I AS Integer = 0 TO (PaddedLen - 1)
        Msg[I] = 0
    NEXT
    FOR I As Integer = 0 TO (InputLen - 1)
        Msg[I] = InputStr[I]
    NEXT
    Msg[InputLen] = &H80

    FOR I AS Integer = 0 TO 7
        Msg[InputLen + PadLen + I] = (BitLen Shr (8 * I)) And &HFF
    Next

    DIM AS ULong W(0 TO 15)
    DIM AS ULong A, B, C, D, F, G, Temp

    FOR Offset As Integer = 0 TO PaddedLen - 1 STEP 64
        FOR I AS Integer = 0 TO 15
            W(I) = Cast(ULong, Msg[Offset + I*4 + 0]) _
                 Or (Cast(ULong, Msg[Offset + I*4 + 1]) Shl 8) _
                 Or (Cast(ULong, Msg[Offset + I*4 + 2]) Shl 16) _
                 Or (Cast(ULong, Msg[Offset + I*4 + 3]) Shl 24)
        NEXT

        A = H0 : B = H1 : C = H2 : D = H3

        FOR I AS Integer = 0 TO 63
            IF I < 16 THEN
                F = (B And C) Or ((Not B) And D)
                G = I
            ELSEIF I < 32 THEN
                F = (D And B) Or ((Not D) And C)
                G = (5 * I + 1) Mod 16
            ELSEIF I < 48 THEN
                F = B Xor C Xor D
                G = (3 * I + 5) Mod 16
            ELSE
                F = C Xor (B Or (Not D))
                G = (7 * I) Mod 16
            END IF

            Temp = D
            D = C
            C = B
            B += RotateLeft((A + F + K(I) + W(G)), R(I))
            A = Temp
        NEXT

        H0 += A : H1 += B : H2 += C : H3 += F
    NEXT

    DIM AS Integer P = 1
    FOR I AS Integer = 0 TO 3
        Mid(Digest, P, 2) = Hex((H0 Shr (8 * I)) And &HFF, 2) : P += 2
    NEXT
    FOR I AS Integer = 0 TO 3
        Mid(Digest, P, 2) = Hex((H1 Shr (8 * I)) And &HFF, 2) : P += 2
    NEXT
    FOR I AS Integer = 0 TO 3
       Mid(Digest, P, 2) = Hex((H2 Shr (8 * I)) And &HFF, 2) : P += 2
    NEXT
    FOR I AS Integer = 0 TO 3
        Mid(Digest, P, 2) = Hex((H3 Shr (8 * I)) And &HFF, 2) : P += 2
    NEXT

    DEALLOCATE Msg
    RETURN LCase(Digest)
END FUNCTION

SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <key>"
    System(1)
END SUB

FUNCTION Process(KeyStr AS String) AS Integer
    DIM N AS Integer = 1
    DIM TryKey AS String
    DIM Digest AS String

    DO
        TryKey = KeyStr & LTrim(Str(N))
	Digest = MD5Digest(TryKey)
	IF Mid(Digest, 1, 6) = "000000" THEN
            EXIT DO
        ELSE
            N += 1
        END IF
    LOOP

    RETURN N
END FUNCTION

DIM Argc AS Integer
DIM Argv AS ZString Ptr Ptr

DIM KeyStr AS String
DIM Result AS Integer

Argc = __FB_ARGC__
Argv = __FB_ARGV__

IF Argc < 2 THEN
    Usage(*Argv[0])
END IF

KeyStr = *Argv[1]
Result = Process(KeyStr)

PRINT "result ="; result

