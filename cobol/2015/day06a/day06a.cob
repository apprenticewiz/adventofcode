       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY06A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(32).
       
       WORKING-STORAGE SECTION.
       77 ARGC                 PIC 9(4).
       77 PROGNAME             PIC X(256).
       77 ARG                  PIC X(256) VALUE SPACES.
       77 EOF-FLAG             PIC X(1) VALUE "N".
       77 INPUT-LEN            PIC 9(6).
       77 ROW-NUM              PIC 9(6).
       77 COL-NUM              PIC 9(6).
       77 TOTAL                PIC 9(6) VALUE 0.
       77 RESULT               PIC Z(6).
       77 ACTION1              PIC X(7).
       77 ACTION2              PIC X(3).
       77 ACTION               PIC X(8).
       77 COORD1               PIC X(8).
       77 THR-STR              PIC X(8).
       77 COORD2               PIC X(8).
       77 R1-STR               PIC X(4).
       77 C1-STR               PIC X(4).
       77 R2-STR               PIC X(4).
       77 C2-STR               PIC X(4).
       77 R1                   PIC 9(4).
       77 C1                   PIC 9(4).
       77 R2                   PIC 9(4).
       77 C2                   PIC 9(4).
       01 GRID.
           05 GRID-ROWS OCCURS 1000 TIMES
              INDEXED BY ROW-INDEX.
              10 GRID-COLS OCCURS 1000 TIMES
                 INDEXED BY COL-INDEX.
                  15 LIGHT-ON  PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-ROUTINE.

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
           READ INPUT-FILE
               AT END
                   MOVE "Y" TO EOF-FLAG
               NOT AT END
                   MOVE INPUT-RECORD(1:4) TO ACTION1
                   IF ACTION1 = "turn"
                       UNSTRING INPUT-RECORD
                           DELIMITED BY SPACE
                           INTO ACTION1 ACTION2 COORD1 THR-STR COORD2
                       END-UNSTRING
                       STRING ACTION1 DELIMITED BY SPACE
                              " " DELIMITED BY SIZE
                              ACTION2 DELIMITED BY SPACE
                              INTO ACTION
                       END-STRING
                   ELSE
                       UNSTRING INPUT-RECORD
                           DELIMITED BY SPACE
                           INTO ACTION COORD1 THR-STR COORD2
                       END-UNSTRING
                   END-IF
                   UNSTRING COORD1
                       DELIMITED BY ","
                       INTO R1-STR C1-STR
                   END-UNSTRING
                   UNSTRING COORD2
                       DELIMITED BY ","
                       INTO R2-STR C2-STR
                   END-UNSTRING
                   MOVE FUNCTION NUMVAL(R1-STR) TO R1
                   MOVE FUNCTION NUMVAL(C1-STR) TO C1
                   MOVE FUNCTION NUMVAL(R2-STR) TO R2
                   MOVE FUNCTION NUMVAL(C2-STR) TO C2
                   PERFORM PERFORM-ACTION
           END-READ
       END-PERFORM
       CLOSE INPUT-FILE

       PERFORM COUNT-LIGHTS

       MOVE TOTAL TO RESULT
       DISPLAY "result = " RESULT

       STOP RUN.

       PERFORM-ACTION.
           PERFORM VARYING ROW-NUM FROM R1 BY 1 UNTIL ROW-NUM > R2
               PERFORM VARYING COL-NUM FROM C1 BY 1 UNTIL COL-NUM > C2
                   MOVE ROW-NUM TO ROW-INDEX
                   MOVE COL-NUM TO COL-INDEX
                   IF ACTION(1:7) = "turn on"
                       MOVE "Y" TO LIGHT-ON(ROW-INDEX, COL-INDEX)
                   ELSE IF ACTION(1:8) = "turn off"
                       MOVE "N" TO LIGHT-ON(ROW-INDEX, COL-INDEX)
                   ELSE IF ACTION(1:6) = "toggle"
                       IF LIGHT-ON(ROW-INDEX, COL-INDEX) = "Y"
                           MOVE "N" TO LIGHT-ON(ROW-INDEX, COL-INDEX)
                       ELSE
                           MOVE "Y" TO LIGHT-ON(ROW-INDEX, COL-INDEX)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       COUNT-LIGHTS.
           PERFORM VARYING ROW-NUM FROM 1 BY 1 UNTIL ROW-NUM > 1000
               PERFORM VARYING COL-NUM FROM 1 BY 1 UNTIL COL-NUM > 1000
                   MOVE ROW-NUM TO ROW-INDEX
                   MOVE COL-NUM TO COL-INDEX
                   IF LIGHT-ON(ROW-INDEX, COL-INDEX) = "Y"
                       ADD 1 TO TOTAL
                   END-IF
               END-PERFORM
           END-PERFORM.
