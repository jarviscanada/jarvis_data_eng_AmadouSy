       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0006.

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
       01 WS-STUDENT-ID        PIC X(4).
       01 WS-ID                PIC X(4).
       01 WS-NAME              PIC X(25).
       01 WS-BIRTHDAY          PIC X(8).
       01 WS-COURSE            PIC X(15).
       01 WS-INSERT-DATE       PIC X(8).
       01 WS-UPDATE-DATE       PIC X(8).
       01 WS-FOUND             PIC X VALUE "N".
       01 WS-EOF               PIC X VALUE "N".

       PROCEDURE DIVISION.
       BEGIN.

           DISPLAY "+-------------------------------------------+"
           DISPLAY "|   Q U E R Y   S T U D E N T   B Y   I D   |"
           DISPLAY "+-------------------------------------------+"

           DISPLAY " "
           DISPLAY "ENTER STUDENT ID (MAX 4 DIGITS) >>"
           ACCEPT WS-STUDENT-ID

           OPEN INPUT STUDENT-FILE

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY,
                               WS-COURSE, WS-INSERT-DATE, WS-UPDATE-DATE
                       IF WS-ID = WS-STUDENT-ID
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

           CLOSE STUDENT-FILE

           IF WS-FOUND NOT = "Y"
               DISPLAY "STUDENT ID NOT FOUND."
           END-IF

           EXIT PROGRAM.
