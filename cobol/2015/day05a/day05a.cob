       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY05A.

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
       77 LINE-LEN             PIC 9(4).
       77 NICE-COUNT           PIC 9(6) VALUE 0.
       77 PROP1                PIC X(1) VALUE "N".
       77 PROP2                PIC X(1) VALUE "N".
       77 PROP3                PIC X(1) VALUE "N".
       77 VOWEL-COUNT          PIC 9(4).
       77 I                    PIC 9(4).
       77 CURR-CHAR            PIC X(1).
       77 NEXT-CHAR            PIC X(1).
       77 PAIR                 PIC X(2).
       77 RESULT               PIC Z(6).

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
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD))
                     TO LINE-LEN
                   PERFORM CHECK-PROP1
                   PERFORM CHECK-PROP2
                   PERFORM CHECK-PROP3
                   IF PROP1 = "Y" AND PROP2 = "Y" AND PROP3 = "Y"
                       ADD 1 TO NICE-COUNT
                   END-IF
           END-READ
       END-PERFORM
       CLOSE INPUT-FILE

       MOVE NICE-COUNT TO RESULT
       DISPLAY "result = " RESULT

       STOP RUN.
       
       CHECK-PROP1.
           MOVE 0 TO VOWEL-COUNT
           MOVE "N" TO PROP1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LINE-LEN
               MOVE INPUT-RECORD(I:1) TO CURR-CHAR
               IF CURR-CHAR = "a" OR "e" OR "i" OR "o" OR "u"
                   ADD 1 TO VOWEL-COUNT
               END-IF
           END-PERFORM
           IF VOWEL-COUNT >= 3
               MOVE "Y" TO PROP1
           END-IF.

       CHECK-PROP2.
           MOVE "N" TO PROP2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (LINE-LEN - 1)
               MOVE INPUT-RECORD(I:1) TO CURR-CHAR
               MOVE INPUT-RECORD(I + 1:1) TO NEXT-CHAR
               IF CURR-CHAR = NEXT-CHAR
                   MOVE "Y" TO PROP2
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       CHECK-PROP3.
           MOVE "Y" TO PROP3
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (LINE-LEN - 1)
               MOVE INPUT-RECORD(I:1) TO CURR-CHAR
               MOVE INPUT-RECORD(I + 1:1) TO NEXT-CHAR
               STRING CURR-CHAR DELIMITED BY SIZE
                   NEXT-CHAR DELIMITED BY SIZE
                   INTO PAIR
               IF PAIR = "ab" OR "cd" OR "pq" OR "xy"
                   MOVE "N" TO PROP3
                   EXIT PERFORM
               END-IF
           END-PERFORM.
