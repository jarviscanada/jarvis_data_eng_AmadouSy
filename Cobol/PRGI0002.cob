       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGI0002.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           *> We will OPEN INPUT then EXTEND on the same file

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE        PIC X(150).  *> One line per student

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

       01 WS-EOF              PIC X VALUE "N".
       01 WS-FOUND            PIC X VALUE "N".

       01 TEMP-ID             PIC X(4).
       01 TEMP-NAME           PIC X(25).
       01 TEMP-BIRTHDATE      PIC X(8).
       01 TEMP-COURSE         PIC X(15).
       01 TEMP-INSERT         PIC X(8).
       01 TEMP-UPDATE         PIC X(8).

       PROCEDURE DIVISION.
       BEGIN.
       
           MOVE "N" TO WS-EOF
           MOVE "N" TO WS-FOUND

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

           *> Step 1: Check if ID already exists
           OPEN INPUT STUDENT-FILE

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","
                           INTO TEMP-ID TEMP-NAME TEMP-BIRTHDATE
                                TEMP-COURSE TEMP-INSERT TEMP-UPDATE

                       IF TEMP-ID = WS-STUDENT-ID
                           MOVE "Y" TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE

           IF WS-FOUND = "Y"
               DISPLAY "***ERROR: STUDENT ID ALREADY EXISTS.***"
               DISPLAY "PLEASE USE A UNIQUE STUDENT ID."
               EXIT PROGRAM
           END-IF

           *> Step 2: If ID is unique, proceed with insertion
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

           DISPLAY "*** STUDENT ADDED SUCCESSFULLY. ***"

           EXIT PROGRAM.
