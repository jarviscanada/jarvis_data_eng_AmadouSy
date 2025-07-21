       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0007.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE         PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-INCL-DATE         PIC X(8).
       01 WS-ID                PIC X(4).
       01 WS-NAME              PIC X(25).
       01 WS-BIRTHDAY          PIC X(8).
       01 WS-COURSE            PIC X(15).
       01 WS-INSERT-DATE       PIC X(8).
       01 WS-UPDATE-DATE       PIC X(8).
       01 WS-STUDENT-COUNT     PIC 9(3) VALUE 0.
       01 WS-EOF               PIC X VALUE "N".
       01 WS-FORMATTED-DATE.
           05 WS-YEAR          PIC X(4).
           05 WS-MONTH         PIC X(2).
           05 WS-DAY           PIC X(2).

       PROCEDURE DIVISION.
       BEGIN.

           DISPLAY "+-------------------------------------------------"&
           "------------------+"
           DISPLAY "|   Q U E R Y   S T U D E N T   B Y   I N C L U S "&
           "I O N   D A T E   |"
           DISPLAY "+-------------------------------------------------"&
           "------------------+"

           DISPLAY " "
           DISPLAY "ENTER THE DATE OF INCLUSION (YYMMDD) >>"
           ACCEPT WS-INCL-DATE

           MOVE WS-INCL-DATE(1:4) TO WS-YEAR
           MOVE WS-INCL-DATE(5:2) TO WS-MONTH
           MOVE WS-INCL-DATE(7:2) TO WS-DAY

           DISPLAY " "
           DISPLAY "LIST OF STUDENTS INCLUDED ON: " WS-MONTH "/"
            WS-DAY "/" WS-YEAR

           OPEN INPUT STUDENT-FILE

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY,
                               WS-COURSE, WS-INSERT-DATE, WS-UPDATE-DATE
                       IF WS-INSERT-DATE = WS-INCL-DATE
                           IF WS-STUDENT-COUNT = 0
                               DISPLAY "------------------------------"&
                               "--------------------------------------"&
                               "----------"
                               DISPLAY " ID  | STUDENT NAME       | BI"&
                               "RTHDAY | COURSE | INSERT   | UPDATE"
                               DISPLAY "------------------------------"&
                               "--------------------------------------"&
                               "----------"
                           END-IF
                           ADD 1 TO WS-STUDENT-COUNT
               DISPLAY WS-ID " | " WS-NAME(1:18) " | " WS-BIRTHDAY " | "
               WS-COURSE(1:6) " | " WS-INSERT-DATE " | " WS-UPDATE-DATE
           END-PERFORM

           CLOSE STUDENT-FILE

           IF WS-STUDENT-COUNT > 0
               DISPLAY "----------------------------------------------"&
               "--------------------------------"
               DISPLAY "TOTAL STUDENTS : " WS-STUDENT-COUNT
           ELSE
               DISPLAY "NO STUDENTS FOUND FOR THIS DATE."
           END-IF

           EXIT PROGRAM.
