       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0006.                         *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     *> Input student file

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE         PIC X(100).           *> Each line from the student file

       WORKING-STORAGE SECTION.
       01 WS-STUDENT-ID        PIC X(4).             *> Student ID to search for
       01 WS-ID                PIC X(4).             *> Parsed student ID from line
       01 WS-NAME              PIC X(25).            *> Parsed student name
       01 WS-BIRTHDAY          PIC X(8).             *> Parsed birthday (YYYYMMDD)
       01 WS-COURSE            PIC X(15).            *> Parsed course
       01 WS-INSERT-DATE       PIC X(8).             *> Parsed insert date
       01 WS-UPDATE-DATE       PIC X(8).             *> Parsed update date
       01 WS-FOUND             PIC X VALUE "N".      *> Flag if student found
       01 WS-EOF               PIC X VALUE "N".      *> End-of-file flag

       PROCEDURE DIVISION.
       BEGIN.
           MOVE "N" TO WS-EOF
           MOVE "N" TO WS-FOUND

           DISPLAY "+-------------------------------------------+"
           DISPLAY "|   Q U E R Y   S T U D E N T   B Y   I D   |"
           DISPLAY "+-------------------------------------------+"   *> Title

           DISPLAY " "
           DISPLAY "ENTER STUDENT ID (MAX 4 DIGITS) >>"         *> Prompt for ID
           ACCEPT WS-STUDENT-ID                          *> Read ID from user

           OPEN INPUT STUDENT-FILE                       *> Open the student file

           PERFORM UNTIL WS-EOF = "Y"                    *> Loop through file
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF                *> Set EOF flag when done
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","  *> Parse CSV line
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY,
                               WS-COURSE, WS-INSERT-DATE, WS-UPDATE-DATE
                       IF WS-ID = WS-STUDENT-ID          *> If ID matches input
                           MOVE "Y" TO WS-FOUND
                           DISPLAY " "
                           DISPLAY "----------------------------------"&
                          "--------------------------------------------"
                           DISPLAY " ID  | STUDENT NAME       | BIRTHD"&
                           "AY | COURSE   | INSERT DATE | UPDATE DATE"
                           DISPLAY "----------------------------------"&
                          "--------------------------------------------"
               DISPLAY WS-ID " | " WS-NAME(1:18) " | " WS-BIRTHDAY " | "
             WS-COURSE(1:8) " | " WS-INSERT-DATE "    | " WS-UPDATE-DATE
                           DISPLAY "----------------------------------"&
                          "--------------------------------------------"
           END-PERFORM

           CLOSE STUDENT-FILE                           *> Close file

           IF WS-FOUND NOT = "Y"
               DISPLAY "STUDENT ID NOT FOUND."            *> Message if not found
           END-IF

           EXIT PROGRAM.                                *> End of program
