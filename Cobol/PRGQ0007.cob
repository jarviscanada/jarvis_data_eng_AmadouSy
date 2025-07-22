       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0007.                          *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.      *> Input student file

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE         PIC X(100).           *> Each line from the file

       WORKING-STORAGE SECTION.
       01 WS-INCL-DATE         PIC X(8).             *> Input inclusion date
       01 WS-ID                PIC X(4).             *> Student ID
       01 WS-NAME              PIC X(25).            *> Student name
       01 WS-BIRTHDAY          PIC X(8).             *> Birthday (YYYYMMDD)
       01 WS-COURSE            PIC X(15).            *> Course name
       01 WS-INSERT-DATE       PIC X(8).             *> Insertion date
       01 WS-UPDATE-DATE       PIC X(8).             *> Update date
       01 WS-STUDENT-COUNT     PIC 9(3) VALUE 0.     *> Number of matching students
       01 WS-EOF               PIC X VALUE "N".      *> End-of-file flag
       01 WS-FORMATTED-DATE.                         *> Used to format display date
           05 WS-YEAR          PIC X(4).
           05 WS-MONTH         PIC X(2).
           05 WS-DAY           PIC X(2).

       PROCEDURE DIVISION.
       BEGIN.
           
           MOVE "N" TO WS-EOF
           MOVE 0 TO WS-STUDENT-COUNT
           
           DISPLAY "+-------------------------------------------------"&
           "------------------+"
           DISPLAY "|   Q U E R Y   S T U D E N T   B Y   I N C L U S "&
           "I O N   D A T E   |"
           DISPLAY "+-------------------------------------------------"&
           "------------------+"   *> Display header

           DISPLAY " "
           DISPLAY "ENTER THE DATE OF INCLUSION (YYMMDD) >>"     *> Prompt user
           ACCEPT WS-INCL-DATE                            *> Read input date

           MOVE WS-INCL-DATE(1:4) TO WS-YEAR              *> Extract year
           MOVE WS-INCL-DATE(5:2) TO WS-MONTH             *> Extract month
           MOVE WS-INCL-DATE(7:2) TO WS-DAY               *> Extract day

           DISPLAY " "
           DISPLAY "LIST OF STUDENTS INCLUDED ON: " WS-MONTH "/"  *> Formatted display
            WS-DAY "/" WS-YEAR

           OPEN INPUT STUDENT-FILE                        *> Open file

           PERFORM UNTIL WS-EOF = "Y"                     *> Read until EOF
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF                 *> Set EOF flag
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","  *> Parse line
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY,
                               WS-COURSE, WS-INSERT-DATE, WS-UPDATE-DATE
                       IF WS-INSERT-DATE = WS-INCL-DATE       *> Match inclusion date
                           IF WS-STUDENT-COUNT = 0             *> If first match
                               DISPLAY "------------------------------"&
                               "--------------------------------------"&
                               "----------"
                               DISPLAY " ID  | STUDENT NAME       | BI"&
                               "RTHDAY | COURSE | INSERT   | UPDATE"
                               DISPLAY "------------------------------"&
                               "--------------------------------------"&
                               "----------"
                           END-IF
                           ADD 1 TO WS-STUDENT-COUNT        *> Increment counter
               DISPLAY WS-ID " | " WS-NAME(1:18) " | " WS-BIRTHDAY " | "  *> Display record
               WS-COURSE(1:6) " | " WS-INSERT-DATE " | " WS-UPDATE-DATE
           END-PERFORM

           CLOSE STUDENT-FILE                             *> Close file

           IF WS-STUDENT-COUNT > 0                        *> If students found
               DISPLAY "----------------------------------------------"&
               "--------------------------------"
               DISPLAY "TOTAL STUDENTS : " WS-STUDENT-COUNT
           ELSE
               DISPLAY "NO STUDENTS FOUND FOR THIS DATE."  *> No results
           END-IF

           EXIT PROGRAM.                                 *> End program
