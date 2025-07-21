       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGU0003.

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
       01 WS-LINE              PIC X(100).
       01 WS-LINE-TABLE.
           05 WS-LINE-ENTRY    OCCURS 1 TO 100 TIMES
                               DEPENDING ON WS-LINE-COUNT.
               10 WS-LINE-ITEM PIC X(100).

       01 WS-LINE-COUNT        PIC 9(3) VALUE 0.
       01 WS-INDEX             PIC 9(3) VALUE 1.
       01 WS-EOF               PIC X VALUE "N".
       01 WS-FOUND             PIC X VALUE "N".

       01 WS-ID                PIC X(4).
       01 WS-NAME              PIC X(18).
       01 WS-BIRTHDAY          PIC X(8).
       01 WS-COURSE            PIC X(8).
       01 WS-INSERT-DATE       PIC X(8).
       01 WS-UPDATE-DATE       PIC X(8).

       01 WS-NEW-NAME          PIC X(18).
       01 WS-NEW-BIRTHDAY      PIC X(8).
       01 WS-NEW-COURSE        PIC X(8).

       01 WS-TODAY-FORMATTED   PIC X(8).

       01 WS-FINAL-ID          PIC X(4).
       01 WS-FINAL-NAME        PIC X(18).
       01 WS-FINAL-BIRTHDAY    PIC X(8).
       01 WS-FINAL-COURSE      PIC X(8).
       01 WS-FINAL-INSERT      PIC X(8).
       01 WS-FINAL-UPDATE      PIC X(8).

       PROCEDURE DIVISION.
       BEGIN.

           DISPLAY "+---------------------------------+"
           DISPLAY "|   U P D A T E   S T U D E N T   |"
           DISPLAY "+---------------------------------+"

           DISPLAY "ENTER THE 4 DIGIT STUDENT ID >>"
           ACCEPT WS-STUDENT-ID

           ACCEPT WS-TODAY-FORMATTED FROM DATE YYYYMMDD

           OPEN INPUT STUDENT-FILE

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-LINE-COUNT
                       MOVE STUDENT-LINE TO WS-LINE-ENTRY(WS-LINE-COUNT)
                       UNSTRING STUDENT-LINE DELIMITED BY ","
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY, WS-COURSE,
                                WS-INSERT-DATE, WS-UPDATE-DATE
                       IF WS-ID = WS-STUDENT-ID
                           MOVE "Y" TO WS-FOUND
                           DISPLAY " "
                           DISPLAY "<--- STUDENT TO BE UPDATED --->"
                           DISPLAY "---------------------------------" &
                                   "----------------------------------"&
                                   "-------"
                           DISPLAY " ID  | STUDENT NAME       | BIRTH" &
                                   "DAY | COURSE   | INSERT   | UPDA"&
                                   "TE"
                           DISPLAY "---------------------------------" &
                                   "----------------------------------"&
                                   "-------"
               DISPLAY WS-ID " | " WS-NAME(1:18) " | " WS-BIRTHDAY " | " 
                         WS-COURSE(1:8) " | " WS-INSERT-DATE(1:8) " | " 
                                   WS-UPDATE-DATE(1:8)
                           DISPLAY "---------------------------------" &
                                   "----------------------------------"&
                                   "-------"
                           DISPLAY "ENTER THE DETAILS TO BE CHANGED"
             DISPLAY "NEW STUDENT NAME (MAX 25 CHAR) - SPACE TO SKIP >>"
                           ACCEPT WS-NEW-NAME
                    DISPLAY "NEW BIRTHDAY (YYYYMMDD) - SPACE TO SKIP >>"
                           ACCEPT WS-NEW-BIRTHDAY
              DISPLAY "NEW COURSE NAME (MAX 15 CHAR) - SPACE TO SKIP >>"
                           ACCEPT WS-NEW-COURSE
                           IF WS-NEW-NAME NOT = SPACES
                               MOVE WS-NEW-NAME TO WS-NAME
                           END-IF
                           IF WS-NEW-BIRTHDAY NOT = SPACES
                               MOVE WS-NEW-BIRTHDAY TO WS-BIRTHDAY
                           END-IF
                           IF WS-NEW-COURSE NOT = SPACES
                               MOVE WS-NEW-COURSE TO WS-COURSE
                           END-IF
                           MOVE WS-TODAY-FORMATTED TO WS-UPDATE-DATE
                           MOVE WS-ID           TO WS-FINAL-ID
                           MOVE WS-NAME         TO WS-FINAL-NAME
                           MOVE WS-BIRTHDAY     TO WS-FINAL-BIRTHDAY
                           MOVE WS-COURSE       TO WS-FINAL-COURSE
                           MOVE WS-INSERT-DATE  TO WS-FINAL-INSERT
                           MOVE WS-UPDATE-DATE  TO WS-FINAL-UPDATE
                           STRING
                               WS-ID DELIMITED BY SIZE ","
                               WS-NAME DELIMITED BY SIZE ","
                               WS-BIRTHDAY DELIMITED BY SIZE ","
                               WS-COURSE DELIMITED BY SIZE ","
                               WS-INSERT-DATE DELIMITED BY SIZE ","
                               WS-UPDATE-DATE DELIMITED BY SIZE
                               INTO WS-LINE-ENTRY(WS-LINE-COUNT)
                       END-IF
           END-PERFORM

           CLOSE STUDENT-FILE

           IF WS-FOUND = "Y"
               OPEN OUTPUT STUDENT-FILE
               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL 
                   WS-INDEX > WS-LINE-COUNT
                   MOVE WS-LINE-ENTRY(WS-INDEX) TO STUDENT-LINE
                   WRITE STUDENT-LINE
               END-PERFORM
               CLOSE STUDENT-FILE

               DISPLAY " "
               DISPLAY "<--- UPDATED STUDENT DETAILS --->"
               DISPLAY "---------------------------------------------" &
                       "----------------------------"
               DISPLAY " ID  | STUDENT NAME       | BIRTHDAY | COURSE" &
                       "  | INSERT  | UPDATE  "
               DISPLAY "---------------------------------------------" &
                       "----------------------------"
               DISPLAY WS-FINAL-ID " | " WS-FINAL-NAME(1:18) " | " 
                       WS-FINAL-BIRTHDAY " | " WS-FINAL-COURSE(1:8) "|" 
                       WS-FINAL-INSERT(1:8) " | " WS-FINAL-UPDATE(1:8)
               DISPLAY "---------------------------------------------" &
                       "----------------------------"
           ELSE
               DISPLAY "STUDENT ID NOT FOUND."
           END-IF

           EXIT PROGRAM.
