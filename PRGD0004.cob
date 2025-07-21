       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGD0004.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT-FILE.
       01 STUDENT-LINE              PIC X(100).
       01 FIELDS-RECORD REDEFINES STUDENT-LINE.
           05 F-ID                 PIC X(4).
           05 F-COMMA1             PIC X.
           05 F-NAME               PIC X(18).
           05 F-COMMA2             PIC X.
           05 F-BIRTHDAY           PIC X(8).
           05 F-COMMA3             PIC X.
           05 F-COURSE             PIC X(8).
           05 F-COMMA4             PIC X.
           05 F-INSERT             PIC X(8).
           05 F-COMMA5             PIC X.
           05 F-UPDATE             PIC X(8).

       FD TEMP-FILE.
       01 TEMP-LINE                PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-STUDENT-ID            PIC X(4).
       01 WS-FOUND                 PIC X VALUE "N".
       01 WS-EOF                   PIC X VALUE "N".
       01 CONFIRMATION             PIC X.
       01 MOVE-CMD.
           05 CMD-TEXT             PIC X(50)
              VALUE "mv temp.txt vsam-simulated.txt".

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "---------------------------------------------"
           DISPLAY "       DELETE STUDENT DETAILS                "
           DISPLAY "---------------------------------------------"
           DISPLAY "ENTER STUDENT ID (MAX 4 DIGITS) >>"
           ACCEPT WS-STUDENT-ID

           OPEN INPUT STUDENT-FILE
           OPEN OUTPUT TEMP-FILE

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF F-ID = WS-STUDENT-ID
                           MOVE "Y" TO WS-FOUND
                           DISPLAY "-----------------------------------"
                           DISPLAY "ID         : " F-ID
                           DISPLAY "NAME       : " F-NAME
                           DISPLAY "BIRTHDAY   : " F-BIRTHDAY
                           DISPLAY "COURSE     : " F-COURSE
                           DISPLAY "INSERT DATE: " F-INSERT
                           DISPLAY "UPDATE DATE: " F-UPDATE
                           DISPLAY "-----------------------------------"
                           DISPLAY "DELETE THIS STUDENT? (Y/N) >>"
                           ACCEPT CONFIRMATION
                           IF CONFIRMATION NOT = "Y"
                               WRITE TEMP-LINE FROM STUDENT-LINE
                           END-IF
                       ELSE
                           WRITE TEMP-LINE FROM STUDENT-LINE
                       END-IF
           END-PERFORM

           CLOSE STUDENT-FILE
           CLOSE TEMP-FILE

           IF WS-FOUND = "Y" AND CONFIRMATION = "Y"
               CALL "SYSTEM" USING CMD-TEXT
               DISPLAY "<<----- STUDENT DELETED SUCCESSFULLY ----->>"
           ELSE
               DISPLAY "STUDENT NOT DELETED OR NOT FOUND."
           END-IF

           EXIT PROGRAM.
