       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGD0004.                         *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     *> Input file (main student file)
           SELECT TEMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     *> Temporary file to store filtered data

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT-FILE.
       01 STUDENT-LINE              PIC X(100).     *> Raw line from student file
       01 FIELDS-RECORD REDEFINES STUDENT-LINE.     *> Parsed fields using REDEFINES
           05 F-ID                 PIC X(4).        *> Student ID
           05 F-COMMA1             PIC X.           *> Separator
           05 F-NAME               PIC X(18).       *> Student Name
           05 F-COMMA2             PIC X.           *> Separator
           05 F-BIRTHDAY           PIC X(8).        *> Birthday (YYYYMMDD)
           05 F-COMMA3             PIC X.           *> Separator
           05 F-COURSE             PIC X(8).        *> Course
           05 F-COMMA4             PIC X.           *> Separator
           05 F-INSERT             PIC X(8).        *> Insert date
           05 F-COMMA5             PIC X.           *> Separator
           05 F-UPDATE             PIC X(8).        *> Update date

       FD TEMP-FILE.
       01 TEMP-LINE                PIC X(100).      *> Line to write in temp file

       WORKING-STORAGE SECTION.
       01 WS-STUDENT-ID            PIC X(4).        *> Student ID entered by user
       01 WS-FOUND                 PIC X VALUE "N". *> Flag if student is found
       01 WS-EOF                   PIC X VALUE "N". *> End-of-file flag
       01 CONFIRMATION             PIC X.           *> User confirmation (Y/N)
       01 MOVE-CMD.
           05 CMD-TEXT             PIC X(50)
              VALUE "mv temp.txt vsam-simulated.txt". *> Command to replace original file

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "---------------------------------------------"
           DISPLAY "       DELETE STUDENT DETAILS                "
           DISPLAY "---------------------------------------------"
           DISPLAY "ENTER STUDENT ID (MAX 4 DIGITS) >>"
           ACCEPT WS-STUDENT-ID                     *> Ask for student ID

           OPEN INPUT STUDENT-FILE
           OPEN OUTPUT TEMP-FILE                    *> Open files

           PERFORM UNTIL WS-EOF = "Y"               *> Loop until end of file
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF           *> End of file
                   NOT AT END
                       IF F-ID = WS-STUDENT-ID      *> If ID matches
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
                           ACCEPT CONFIRMATION      *> Ask for confirmation
                           IF CONFIRMATION NOT = "Y"
                               WRITE TEMP-LINE FROM STUDENT-LINE *> Keep the record
                           END-IF
                       ELSE
                           WRITE TEMP-LINE FROM STUDENT-LINE     *> Copy other records
                       END-IF
           END-PERFORM

           CLOSE STUDENT-FILE
           CLOSE TEMP-FILE                           *> Close files

           IF WS-FOUND = "Y" AND CONFIRMATION = "Y"
               CALL "SYSTEM" USING CMD-TEXT          *> Overwrite original file
               DISPLAY "<<----- STUDENT DELETED SUCCESSFULLY ----->>"
           ELSE
               DISPLAY "STUDENT NOT DELETED OR NOT FOUND."  *> If not confirmed or not found
           END-IF

           EXIT PROGRAM.                             *> End program
