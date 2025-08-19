       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01A.

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
       77 L-PART               PIC X(4).
       77 W-PART               PIC X(4).
       77 H-PART               PIC X(4).
       77 L                    PIC 9(4).
       77 W                    PIC 9(4).
       77 H                    PIC 9(4).
       77 PERIM1               PIC 9(9).
       77 PERIM2               PIC 9(9).
       77 PERIM3               PIC 9(9).
       77 PRESENT-LENGTH       PIC 9(9).
       77 BOW-LENGTH           PIC 9(9).
       77 TOTAL-LENGTH         PIC 9(9) VALUE 0.
       77 RESULT               PIC Z(9).

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
             PERFORM PROCESS-BOX
         END-READ
       END-PERFORM
       CLOSE INPUT-FILE

       MOVE TOTAL-LENGTH TO RESULT
       DISPLAY "result = " RESULT

       STOP RUN.
       
       PROCESS-BOX.
           UNSTRING INPUT-RECORD DELIMITED BY "x"
               INTO L-PART, W-PART, H-PART
           MOVE FUNCTION NUMVAL(L-PART) TO L
           MOVE FUNCTION NUMVAL(W-PART) TO W
           MOVE FUNCTION NUMVAL(H-PART) TO H

           COMPUTE PERIM1 = 2 * (L + W)
           COMPUTE PERIM2 = 2 * (L + H)
           COMPUTE PERIM3 = 2 * (W + H)

           MOVE PERIM1 TO PRESENT-LENGTH
           IF PERIM2 < PRESENT-LENGTH THEN
               MOVE PERIM2 TO PRESENT-LENGTH
           END-IF
           IF PERIM3 < PRESENT-LENGTH THEN
               MOVE PERIM3 TO PRESENT-LENGTH
           END-IF
           ADD PRESENT-LENGTH TO TOTAL-LENGTH

           COMPUTE BOW-LENGTH = L * W * H
           ADD BOW-LENGTH TO TOTAL-LENGTH.
