       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-REC  PIC X(65536).
       
       WORKING-STORAGE SECTION.
       77 ARGC          PIC 9(4).
       77 ARG-COUNT     PIC Z(1).
       77 PROGNAME      PIC X(256).
       77 ARG           PIC X(256) VALUE SPACES.
       77 EOF-FLAG      PIC X(1) VALUE "N".
       77 CONTENTS      PIC X(65536).
       77 CONTENTS-LEN  PIC 9(4).
       77 RESULT        PIC Z(15).
       77 WS-COMMA-PTR  PIC 9(4).
       77 WS-RANGE-PTR  PIC 9(4).
       77 WS-INDEX      PIC 9(4) VALUE 1.
       77 ONE-RANGE     PIC X(32).
       77 TABLE-IDX     PIC 9(4).
       77 LO            PIC 9(15).
       77 HI            PIC 9(15).
       77 I             PIC 9(15).
       77 J             PIC 9(15).
       77 K             PIC 9(15).
       77 DIGIT-LEN     PIC 9(4).
       77 MID           PIC 9(4).
       77 DIVISOR       PIC 9(15).
       77 QUOT          PIC S9(15).
       77 REM           PIC 9(15).
       77 PATTERN       PIC 9(15).
       77 REST          PIC 9(15).
       77 TEMP-REST     PIC 9(15).
       77 CHUNK         PIC 9(15).
       77 NUM-REPS      PIC 9(4).
       77 REP-COUNT     PIC 9(4).
       77 REST-LEN      PIC 9(4).
       77 CHUNK-DIVISOR PIC 9(15).
       77 INVALID-FLAG  PIC X(1) VALUE "N".
       77 TOTAL         PIC 9(15) VALUE 0.
       01 RANGE-TABLE.
          05 RANGE-ENTRIES OCCURS 200 TIMES.
              10 RANGE-LOW    PIC X(16).
              10 RANGE-HIGH   PIC X(16).

       PROCEDURE DIVISION.

       ACCEPT ARGC FROM ARGUMENT-NUMBER

       DISPLAY 0 UPON ARGUMENT-NUMBER
       ACCEPT PROGNAME FROM ARGUMENT-VALUE

       EVALUATE TRUE
         WHEN ARGC IS LESS THAN 1
           DISPLAY "usage: " FUNCTION TRIM(PROGNAME) " <input file>"
           STOP RUN
       END-EVALUATE

       DISPLAY 1 UPON ARGUMENT-NUMBER
       ACCEPT ARG FROM ARGUMENT-VALUE
       MOVE FUNCTION TRIM(ARG) TO FILENAME

       OPEN INPUT INPUT-FILE
       PERFORM UNTIL EOF-FLAG = "Y"
         READ INPUT-FILE INTO INPUT-REC
           AT END
             MOVE "Y" TO EOF-FLAG
           NOT AT END
             MOVE INPUT-REC TO CONTENTS
             PERFORM PARSE-RANGES
             PERFORM PROCESS-INVALID-IDS
         END-READ
       END-PERFORM
       CLOSE INPUT-FILE

       MOVE TOTAL TO RESULT
       DISPLAY "result = " RESULT

       STOP RUN.
       
       PARSE-RANGES.
         MOVE INPUT-REC TO CONTENTS
         MOVE 1 TO WS-COMMA-PTR
                   WS-INDEX
         MOVE FUNCTION LENGTH(FUNCTION TRIM(CONTENTS)) TO CONTENTS-LEN
         PERFORM UNTIL WS-COMMA-PTR > CONTENTS-LEN
           UNSTRING CONTENTS
             DELIMITED BY ","
             INTO ONE-RANGE
             WITH POINTER WS-COMMA-PTR
           END-UNSTRING
           MOVE 1 TO WS-RANGE-PTR
           UNSTRING ONE-RANGE
             DELIMITED BY "-"
             INTO RANGE-LOW(WS-INDEX)
                  RANGE-HIGH(WS-INDEX)
             WITH POINTER WS-RANGE-PTR
           END-UNSTRING
           ADD 1 TO WS-INDEX
         END-PERFORM.

       PROCESS-INVALID-IDS.
         PERFORM VARYING TABLE-IDX FROM 1 BY 1
             UNTIL (TABLE-IDX > WS-INDEX)
           MOVE FUNCTION NUMVAL(RANGE-LOW(TABLE-IDX)) TO LO
           MOVE FUNCTION NUMVAL(RANGE-HIGH(TABLE-IDX)) TO HI
           PERFORM VARYING I FROM LO BY 1 UNTIL I > HI
             MOVE "N" TO INVALID-FLAG
             MOVE 1 TO DIVISOR
             MOVE 0 TO DIGIT-LEN
             MOVE -1 TO QUOT
             PERFORM UNTIL QUOT = 0
               DIVIDE DIVISOR INTO I GIVING QUOT
               IF QUOT = 0 THEN
                 EXIT PERFORM
               END-IF
               COMPUTE DIVISOR = DIVISOR * 10
               ADD 1 TO DIGIT-LEN
             END-PERFORM
             DIVIDE 2 INTO DIGIT-LEN GIVING MID
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > MID
               DIVIDE J INTO DIGIT-LEN GIVING NUM-REPS REMAINDER REM
               IF REM = 0 AND NUM-REPS >= 2 THEN
                 MOVE 1 TO DIVISOR
                 SUBTRACT J FROM DIGIT-LEN GIVING REST-LEN
                 PERFORM VARYING K FROM 1 BY 1 UNTIL K > REST-LEN
                   COMPUTE DIVISOR = DIVISOR * 10
                 END-PERFORM
                 DIVIDE DIVISOR INTO I GIVING PATTERN REMAINDER REST
                 MOVE "Y" TO INVALID-FLAG
                 PERFORM VARYING REP-COUNT FROM 2 BY 1
                     UNTIL REP-COUNT > NUM-REPS
                   MOVE 1 TO CHUNK-DIVISOR
                   SUBTRACT J FROM REST-LEN
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > REST-LEN
                     COMPUTE CHUNK-DIVISOR = CHUNK-DIVISOR * 10
                   END-PERFORM
                   DIVIDE CHUNK-DIVISOR INTO REST
                     GIVING CHUNK REMAINDER TEMP-REST
                   IF CHUNK NOT = PATTERN THEN
                     MOVE "N" TO INVALID-FLAG
                     EXIT PERFORM
                   END-IF
                   MOVE TEMP-REST TO REST
                 END-PERFORM
                 IF INVALID-FLAG = "Y" THEN
                   EXIT PERFORM
                 END-IF
               END-IF
             END-PERFORM
             IF INVALID-FLAG = "Y" THEN
               ADD I TO TOTAL
             END-IF
           END-PERFORM
         END-PERFORM.
