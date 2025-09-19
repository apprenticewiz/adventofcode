       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY08A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(64).
       
       WORKING-STORAGE SECTION.
       77 ARGC                 PIC 9(4).
       77 PROGNAME             PIC X(256).
       77 ARG                  PIC X(256) VALUE SPACES.
       77 EOF-FLAG             PIC X(1) VALUE "N".
       77 RESULT               PIC 9(4) VALUE 0.
       77 BUF                  PIC X(64).
       77 CODE-LEN             PIC 9(2).
       77 LAST-QUOTED          PIC 9(2).
       77 MEM-LEN              PIC 9(2).
       77 I                    PIC 9(2).
       77 I1                   PIC 9(2).
       77 DELTA                PIC 9(2).
       77 DISP-RESULT          PIC Z(4).

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
                 PERFORM SCAN-LINE
           END-READ
       END-PERFORM
       CLOSE INPUT-FILE

       MOVE RESULT TO DISP-RESULT
       DISPLAY "result = " FUNCTION TRIM(DISP-RESULT)

       STOP RUN.
       
       SCAN-LINE.
           MOVE FUNCTION TRIM(INPUT-RECORD) TO BUF
           MOVE FUNCTION LENGTH(FUNCTION TRIM(BUF)) TO CODE-LEN
           COMPUTE LAST-QUOTED = CODE-LEN - 1
           MOVE 0 TO MEM-LEN
           MOVE 2 TO I
           PERFORM UNTIL I > LAST-QUOTED
               EVALUATE BUF(I:1)
                   WHEN '\'
                       COMPUTE I1 = I + 1
                       EVALUATE BUF(I1:1)
                           WHEN '\' 
                               ADD 2 TO I
                           WHEN '"'
                               ADD 2 TO I
                           WHEN 'x'
                               ADD 4 TO I
                           WHEN OTHER
                               ADD 1 TO I
                       END-EVALUATE
                   WHEN OTHER
                       ADD 1 TO I
               END-EVALUATE
               ADD 1 TO MEM-LEN
           END-PERFORM
           COMPUTE DELTA = CODE-LEN - MEM-LEN
           ADD DELTA TO RESULT.
