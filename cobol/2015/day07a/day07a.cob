       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY07A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(80).

       WORKING-STORAGE SECTION.
       77 EOF-FLAG             PIC X VALUE "N".
       77 ARGC                 PIC 9(4).
       77 PROGNAME             PIC X(256).
       77 ARG                  PIC X(256).
       77 FILENAME             PIC X(256).
       77 OPS-LAST-PTR         PIC 9(4) VALUE 0.
       77 WIRES-LAST-PTR       PIC 9(4) VALUE 0.
       77 LOOP-STOP            PIC X VALUE "N".
       77 WIRE-TO-SOLVE        PIC X VALUE "a".
       77 TOTAL                PIC 9(8).
       77 RESULT               PIC Z(8).

       01 OPERATIONS.
           05 OP-DEST OCCURS 1000 TIMES PIC X(8).
           05 OP-OPER OCCURS 1000 TIMES PIC X(8).
           05 OP-SRC1 OCCURS 1000 TIMES PIC X(8).
           05 OP-SRC2 OCCURS 1000 TIMES PIC X(8).
           05 OP-AMT OCCURS 1000 TIMES PIC 9(8).
           05 OP-SOLVED OCCURS 1000 TIMES PIC X VALUE "N".

       01 WIRES.
           05 WIRE-NAME OCCURS 1000 TIMES PIC X(8).
           05 WIRE-VAL OCCURS 1000 TIMES PIC 9(8) COMP-5.

       01 WORK.
           05 PART1            PIC X(8).
           05 PART2            PIC X(8).
           05 PART3            PIC X(8).
           05 PART4            PIC X(8).
           05 PART5            PIC X(8).
           05 I                PIC 9(4).
           05 UNMASKED         PIC 9(8) COMP-5.
           05 MASKED           PIC 9(8) COMP-5.

       01 GET-ONE-ARG-LOCALS.
           05 S1-ARG           PIC X(8).
           05 S1-SOLVED        PIC X.
           05 S1-RESULT        PIC 9(8) COMP-5.
           05 I1               PIC 9(4).

       01 GET-TWO-ARGS-LOCALS.
           05 S2-ARG           PIC X(8).
           05 S2-SOLVED        PIC X.
           05 S2-RESULT        PIC 9(8) COMP-5.

       01 CHECK-RESULT-LOCALS.
           05 I2               PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-ROUTINE.

         ACCEPT ARGC FROM ARGUMENT-NUMBER
         DISPLAY 0 UPON ARGUMENT-NUMBER
         ACCEPT PROGNAME FROM ARGUMENT-VALUE

         IF ARGC < 1 THEN
           DISPLAY "Usage: " FUNCTION TRIM(PROGNAME) " <input file>"
           STOP RUN
         END-IF

         DISPLAY 1 UPON ARGUMENT-NUMBER
         ACCEPT ARG FROM ARGUMENT-VALUE
         MOVE FUNCTION TRIM(ARG) TO FILENAME

         OPEN INPUT INPUT-FILE
         PERFORM UNTIL EOF-FLAG = "Y"
           READ INPUT-FILE
             AT END
               MOVE "Y" TO EOF-FLAG
             NOT AT END
               PERFORM PARSE-LINE
         END-PERFORM
         CLOSE INPUT-FILE

         PERFORM UNTIL LOOP-STOP = "Y"
           PERFORM SOLVE-WIRES
         END-PERFORM

         PERFORM GET-RESULT
         MOVE TOTAL TO RESULT
         DISPLAY "result = " RESULT

         STOP RUN.

       PARSE-LINE.
         ADD 1 TO OPS-LAST-PTR
         UNSTRING INPUT-RECORD DELIMITED BY SPACES
           INTO PART1 PART2 PART3 PART4 PART5
         END-UNSTRING
         EVALUATE TRUE
           WHEN PART1 = "NOT"
             MOVE PART1 TO OP-OPER(OPS-LAST-PTR)
             MOVE PART2 TO OP-SRC1(OPS-LAST-PTR)
             MOVE PART4 TO OP-DEST(OPS-LAST-PTR)
           WHEN PART2 = "AND" OR PART2 = "OR"
             MOVE PART2 TO OP-OPER(OPS-LAST-PTR)
             MOVE PART1 TO OP-SRC1(OPS-LAST-PTR)
             MOVE PART3 TO OP-SRC2(OPS-LAST-PTR)
             MOVE PART5 TO OP-DEST(OPS-LAST-PTR)
           WHEN PART2 = "LSHIFT" OR PART2 = "RSHIFT"
             MOVE PART2 TO OP-OPER(OPS-LAST-PTR)
             MOVE PART1 TO OP-SRC1(OPS-LAST-PTR)
             MOVE FUNCTION NUMVAL(PART3) TO OP-AMT(OPS-LAST-PTR)
             MOVE PART5 TO OP-DEST(OPS-LAST-PTR)
           WHEN PART2 = "->"
             MOVE "ASSIGN" TO OP-OPER(OPS-LAST-PTR)
             MOVE PART1 TO OP-SRC1(OPS-LAST-PTR)
             MOVE PART3 TO OP-DEST(OPS-LAST-PTR)
         END-EVALUATE.
           
       SOLVE-WIRES.
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > OPS-LAST-PTR
           IF OP-SOLVED(I) NOT EQUAL "Y"
             EVALUATE FUNCTION TRIM(OP-OPER(I))
               WHEN "ASSIGN"
                 MOVE OP-SRC1(I) TO S1-ARG
                 PERFORM GET-ONE-ARG
                 IF S1-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   MOVE S1-RESULT TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
               WHEN "NOT"
                 MOVE OP-SRC1(I) TO S1-ARG
                 PERFORM GET-ONE-ARG
                 IF S1-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   COMPUTE UNMASKED = B-NOT S1-RESULT
                   COMPUTE MASKED = UNMASKED B-AND 65535
                   MOVE MASKED TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
               WHEN "AND"
                 MOVE OP-SRC1(I) TO S1-ARG
                 MOVE OP-SRC2(I) TO S2-ARG
                 PERFORM GET-TWO-ARGS
                 IF S1-SOLVED = "Y" AND S2-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   COMPUTE UNMASKED = S1-RESULT B-AND S2-RESULT
                   COMPUTE MASKED = UNMASKED B-AND 65535
                   MOVE MASKED TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
               WHEN "OR"
                 MOVE OP-SRC1(I) TO S1-ARG
                 MOVE OP-SRC2(I) TO S2-ARG
                 PERFORM GET-TWO-ARGS
                 IF S1-SOLVED = "Y" AND S2-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   COMPUTE UNMASKED = S1-RESULT B-OR S2-RESULT
                   COMPUTE MASKED = UNMASKED B-AND 65535
                   MOVE MASKED TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
               WHEN "LSHIFT"
                 MOVE OP-SRC1(I) TO S1-ARG
                 PERFORM GET-ONE-ARG
                 IF S1-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   COMPUTE UNMASKED = S1-RESULT * (2 ** OP-AMT(I))
                   COMPUTE MASKED = UNMASKED B-AND 65535
                   MOVE MASKED TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
               WHEN "RSHIFT"
                 MOVE OP-SRC1(I) TO S1-ARG
                 PERFORM GET-ONE-ARG
                 IF S1-SOLVED = "Y"
                   ADD 1 TO WIRES-LAST-PTR
                   MOVE OP-DEST(I) TO WIRE-NAME(WIRES-LAST-PTR)
                   COMPUTE UNMASKED = S1-RESULT / (2 ** OP-AMT(I))
                   COMPUTE MASKED = UNMASKED B-AND 65535
                   MOVE MASKED TO WIRE-VAL(WIRES-LAST-PTR)
                   MOVE "Y" TO OP-SOLVED(I)
                 END-IF
             END-EVALUATE
           END-IF
           PERFORM SHOULD-HALT
         END-PERFORM.

       GET-ONE-ARG.
         MOVE "N" TO S1-SOLVED
         IF FUNCTION TRIM(S1-ARG) IS NUMERIC
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(S1-ARG)) TO S1-RESULT
           MOVE "Y" TO S1-SOLVED
         ELSE
           PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > WIRES-LAST-PTR
             IF FUNCTION TRIM(WIRE-NAME(I1)) = FUNCTION TRIM(S1-ARG)
               MOVE WIRE-VAL(I1) TO S1-RESULT
               MOVE "Y" TO S1-SOLVED
               EXIT PERFORM
             END-IF
           END-PERFORM
         END-IF.

       GET-TWO-ARGS.
         PERFORM GET-ONE-ARG
         MOVE "N" TO S2-SOLVED
         IF FUNCTION TRIM(S2-ARG) IS NUMERIC
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(S2-ARG)) TO S2-RESULT
           MOVE "Y" TO S2-SOLVED
         ELSE
           PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > WIRES-LAST-PTR
             IF FUNCTION TRIM(WIRE-NAME(I1)) = FUNCTION TRIM(S2-ARG)
               MOVE WIRE-VAL(I1) TO S2-RESULT
               MOVE "Y" TO S2-SOLVED
               EXIT PERFORM
             END-IF
           END-PERFORM
         END-IF.

       SHOULD-HALT.
         PERFORM VARYING I2 FROM 1 BY 1 UNTIL I2 > OPS-LAST-PTR
           IF FUNCTION TRIM(OP-DEST(I2)) = FUNCTION TRIM(WIRE-TO-SOLVE)
             AND OP-SOLVED(I2) = "Y" THEN
               MOVE "Y" TO LOOP-STOP
               EXIT PERFORM
           END-IF
         END-PERFORM.

       GET-RESULT.
         PERFORM VARYING I2 FROM 1 BY 1 UNTIL I2 > WIRES-LAST-PTR
           IF FUNCTION TRIM(WIRE-NAME(I2)) =
             FUNCTION TRIM(WIRE-TO-SOLVE)
               MOVE WIRE-VAL(I2) TO TOTAL
               EXIT PERFORM
           END-IF
         END-PERFORM.
