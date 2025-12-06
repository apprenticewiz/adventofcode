       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY04B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-REC  PIC X(150).

       WORKING-STORAGE SECTION.
       77 ARGC         PIC 9(4).
       77 ARG-COUNT    PIC Z(1).
       77 PROGNAME     PIC X(256).
       77 ARG          PIC X(256) VALUE SPACES.
       77 EOF-FLAG     PIC X(1) VALUE "N".
       77 NUM-ROLLS    PIC 9(6) VALUE 0.
       77 RESULT       PIC Z(6).
       77 ROW-NUM      PIC 9(4) VALUE 1.
       77 COL-NUM      PIC 9(4) VALUE 1.
       77 NUM-ROWS     PIC 9(4).
       77 NUM-COLS     PIC 9(4).
       77 ROW-DELTA    PIC S9(4).
       77 COL-DELTA    PIC S9(4).
       77 NUM-NBRS     PIC 9(4).
       77 NBR-ROW      PIC 9(4).
       77 NBR-COL      PIC 9(4).
       77 STABLE       PIC X(1) VALUE "N".
       77 REMOVED      PIC 9(4) VALUE 0.
       01 GRID.
         05 GRID-ROWS OCCURS 150 TIMES INDEXED BY ROW-INDEX.
           10 GRID-COLS OCCURS 150 TIMES INDEXED BY COL-INDEX.
             15 GRID-CELL PIC X.
       01 NEXT-GRID.
         05 NEXT-ROWS OCCURS 150 TIMES INDEXED BY ROW-INDEX.
           10 NEXT-COLS OCCURS 150 TIMES INDEXED BY COL-INDEX.
             15 NEXT-CELL PIC X.

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
             MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-REC)) TO NUM-COLS
             PERFORM VARYING COL-NUM FROM 1 BY 1
                 UNTIL COL-NUM > NUM-COLS
               MOVE INPUT-REC(COL-NUM:1) TO GRID-CELL(ROW-NUM, COL-NUM)
             END-PERFORM
             ADD 1 TO ROW-NUM
         END-READ
       END-PERFORM
       COMPUTE NUM-ROWS = ROW-NUM - 1
       PERFORM COUNT-ACCESSIBLE
       PERFORM STABILIZE UNTIL STABLE = "Y"
       CLOSE INPUT-FILE
       MOVE REMOVED TO RESULT
       DISPLAY "result = " FUNCTION TRIM(RESULT)

       STOP RUN.

       COUNT-ACCESSIBLE.
         MOVE 0 TO NUM-ROLLS
         PERFORM VARYING ROW-NUM FROM 1 BY 1 UNTIL ROW-NUM > NUM-ROWS
           PERFORM VARYING COL-NUM FROM 1 BY 1 UNTIL COL-NUM > NUM-COLS
             IF GRID-CELL(ROW-NUM, COL-NUM) = "@" THEN
               MOVE 0 TO NUM-NBRS
               PERFORM VARYING ROW-DELTA FROM -1 BY 1 UNTIL
                   ROW-DELTA > 1
                 COMPUTE NBR-ROW = ROW-NUM + ROW-DELTA
                 PERFORM VARYING COL-DELTA FROM -1 BY 1 UNTIL
                     COL-DELTA > 1
                   COMPUTE NBR-COL = COL-NUM + COL-DELTA
                   IF NBR-ROW >= 1 AND NBR-ROW <= NUM-ROWS AND
                       NBR-COL >= 1 AND NBR-COL <= NUM-COLS AND
                       NOT (ROW-DELTA = 0 AND COL-DELTA = 0) THEN
                     IF GRID-CELL(NBR-ROW, NBR-COL) = "@" THEN
                       ADD 1 TO NUM-NBRS
                     END-IF
                   END-IF
                 END-PERFORM
               END-PERFORM
               IF NUM-NBRS <= 3 THEN
                 ADD 1 TO NUM-ROLLS
               END-IF
             END-IF
           END-PERFORM
         END-PERFORM.

       STABILIZE.
         PERFORM VARYING ROW-NUM FROM 1 BY 1 UNTIL ROW-NUM > NUM-ROWS
           PERFORM VARYING COL-NUM FROM 1 BY 1 UNTIL COL-NUM > NUM-COLS
             IF GRID-CELL(ROW-NUM, COL-NUM) = "@" THEN
               MOVE 0 TO NUM-NBRS
               PERFORM VARYING ROW-DELTA FROM -1 BY 1 UNTIL
                   ROW-DELTA > 1
                 COMPUTE NBR-ROW = ROW-NUM + ROW-DELTA
                 PERFORM VARYING COL-DELTA FROM -1 BY 1 UNTIL
                     COL-DELTA > 1
                   COMPUTE NBR-COL = COL-NUM + COL-DELTA
                   IF NBR-ROW >= 1 AND NBR-ROW <= NUM-ROWS AND
                       NBR-COL >= 1 AND NBR-COL <= NUM-COLS AND
                       NOT (ROW-DELTA = 0 AND COL-DELTA = 0) THEN
                     IF GRID-CELL(NBR-ROW, NBR-COL) = "@" THEN
                       ADD 1 TO NUM-NBRS
                     END-IF
                   END-IF
                 END-PERFORM
               END-PERFORM
               IF NUM-NBRS <= 3 THEN
                 MOVE "." TO NEXT-CELL(ROW-NUM, COL-NUM)
                 ADD 1 TO REMOVED
               ELSE
                 MOVE "@" TO NEXT-CELL(ROW-NUM, COL-NUM)
               END-IF
             ELSE
               MOVE GRID-CELL(ROW-NUM, COL-NUM) TO
                   NEXT-CELL(ROW-NUM, COL-NUM)
             END-IF
           END-PERFORM
         END-PERFORM
         PERFORM VARYING ROW-NUM FROM 1 BY 1 UNTIL ROW-NUM > NUM-ROWS
           PERFORM VARYING COL-NUM FROM 1 BY 1 UNTIL COL-NUM > NUM-COLS
             MOVE NEXT-CELL(ROW-NUM, COL-NUM) TO
                 GRID-CELL(ROW-NUM, COL-NUM)
           END-PERFORM
         END-PERFORM
         PERFORM COUNT-ACCESSIBLE
         IF NUM-ROLLS = 0 THEN
           MOVE "Y" TO STABLE
         END-IF.
