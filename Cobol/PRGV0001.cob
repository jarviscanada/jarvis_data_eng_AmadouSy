       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGV0001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "initial.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE           PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE          PIC X(150).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG             PIC X VALUE "N".

       01 WS-FIELD1            PIC X(4).      *> ID
       01 WS-FIELD2            PIC X(18).     *> Name
       01 WS-FIELD3            PIC X(8).      *> Birthdate
       01 WS-FIELD4            PIC X(8).      *> Course

       01 WS-INSERT-DATE       PIC X(8).
       01 WS-UPDATE-DATE       PIC X(8) VALUE "00000000".

       01 WS-CURRENT-DATE.
           05 WS-YEAR          PIC X(4).
           05 WS-MONTH         PIC X(2).
           05 WS-DAY           PIC X(2).

       01 WS-CSV-LINE          PIC X(150).

       PROCEDURE DIVISION.
       BEGIN.
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-CURRENT-DATE(1:4) TO WS-YEAR
           MOVE WS-CURRENT-DATE(5:2) TO WS-MONTH
           MOVE WS-CURRENT-DATE(7:2) TO WS-DAY
           STRING WS-YEAR WS-MONTH WS-DAY INTO WS-INSERT-DATE

           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM UNTIL EOF-FLAG = "Y"
               READ INPUT-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       UNSTRING INPUT-LINE DELIMITED BY ","
                           INTO WS-FIELD1 WS-FIELD2 WS-FIELD3 WS-FIELD4

                       STRING
                           WS-FIELD1 DELIMITED BY SIZE "," 
                           WS-FIELD2 DELIMITED BY SIZE "," 
                           WS-FIELD3 DELIMITED BY SIZE "," 
                           WS-FIELD4 DELIMITED BY SIZE "," 
                           WS-INSERT-DATE DELIMITED BY SIZE "," 
                           WS-UPDATE-DATE DELIMITED BY SIZE
                           INTO WS-CSV-LINE

                       MOVE WS-CSV-LINE TO OUTPUT-LINE
                       WRITE OUTPUT-LINE
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           DISPLAY "Initial data converted with today's date."
           EXIT PROGRAM.
