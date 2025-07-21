       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGI0002.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE        PIC X(150).

       WORKING-STORAGE SECTION.
       01 WS-STUDENT-ID       PIC X(4).
       01 WS-STUDENT-NAME     PIC X(18).
       01 WS-BIRTHDATE        PIC X(8).
       01 WS-COURSE           PIC X(8).
       01 WS-INSERT-DATE      PIC X(8).
       01 WS-UPDATE-DATE      PIC X(8) VALUE "00000000".
       01 WS-OUTPUT-LINE      PIC X(150).

       01 WS-CURRENT-DATE.
           05 WS-YEAR         PIC X(4).
           05 WS-MONTH        PIC X(2).
           05 WS-DAY          PIC X(2).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "+-----------------------------------+"
           DISPLAY "|   A D D   N E W   S T U D E N T   |"
           DISPLAY "+-----------------------------------+"

           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-CURRENT-DATE(1:4) TO WS-YEAR
           MOVE WS-CURRENT-DATE(5:2) TO WS-MONTH
           MOVE WS-CURRENT-DATE(7:2) TO WS-DAY
           STRING WS-YEAR WS-MONTH WS-DAY INTO WS-INSERT-DATE

           DISPLAY "ENTER STUDENT ID (4 DIGITS) >>"
           ACCEPT WS-STUDENT-ID

           DISPLAY "ENTER FULL NAME (MAX 25 CHARS) >>"
           ACCEPT WS-STUDENT-NAME

           DISPLAY "ENTER BIRTHDAY (YYYYMMDD) >>"
           ACCEPT WS-BIRTHDATE

           DISPLAY "ENTER COURSE (MAX 15 CHARS) >>"
           ACCEPT WS-COURSE

           OPEN EXTEND STUDENT-FILE

           STRING
               WS-STUDENT-ID DELIMITED BY SIZE ","
               WS-STUDENT-NAME DELIMITED BY SIZE ","
               WS-BIRTHDATE DELIMITED BY SIZE ","
               WS-COURSE DELIMITED BY SIZE ","
               WS-INSERT-DATE DELIMITED BY SIZE ","
               WS-UPDATE-DATE DELIMITED BY SIZE
               INTO WS-OUTPUT-LINE

           WRITE STUDENT-LINE FROM WS-OUTPUT-LINE

           CLOSE STUDENT-FILE
           DISPLAY "STUDENT ADDED SUCCESSFULLY."
           EXIT PROGRAM.
