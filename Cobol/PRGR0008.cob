       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGR0008.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE            PIC X(100).

       FD REPORT-FILE.
       01 REPORT-LINE             PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-EOF                  PIC X VALUE 'N'.
       01 WS-STUDENT-COUNT        PIC 9(4) VALUE 0.

       01 WS-ID                   PIC X(4).
       01 WS-NAME                 PIC X(25).
       01 WS-BIRTHDAY             PIC X(8).
       01 WS-COURSE               PIC X(15).
       01 WS-INSERT-DATE          PIC X(8).
       01 WS-UPDATE-DATE          PIC X(8).

       01 WS-PREV-COURSE          PIC X(15) VALUE SPACES.
       01 WS-CURRENT-COURSE       PIC X(15).

       PROCEDURE DIVISION.
       BEGIN.

           OPEN INPUT STUDENT-FILE
           OPEN OUTPUT REPORT-FILE

          MOVE "------------------------------------------------------"&
          "--------" TO REPORT-LINE
           WRITE REPORT-LINE
           DISPLAY REPORT-LINE

           MOVE "               C L A S S   R E P O R T" TO REPORT-LINE
           WRITE REPORT-LINE
           DISPLAY REPORT-LINE

           PERFORM UNTIL WS-EOF = 'Y'
               READ STUDENT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY, WS-COURSE,
                                WS-INSERT-DATE, WS-UPDATE-DATE

                       MOVE WS-COURSE TO WS-CURRENT-COURSE

                       IF WS-CURRENT-COURSE NOT = WS-PREV-COURSE
                           MOVE SPACES TO REPORT-LINE
                           WRITE REPORT-LINE
                           DISPLAY REPORT-LINE

                           STRING "   COURSE: " DELIMITED BY SIZE
                                  WS-CURRENT-COURSE DELIMITED BY SIZE
                                  INTO REPORT-LINE
                           WRITE REPORT-LINE
                           DISPLAY REPORT-LINE

                   MOVE "--------------------------------------------"&
                   "------------------" TO REPORT-LINE
                           WRITE REPORT-LINE
                           DISPLAY REPORT-LINE

                    MOVE "ID   | STUDENT NAME       | BIRTHDAY | INSRT"& 
                    "    | UPDATE" 
                                TO REPORT-LINE
                           WRITE REPORT-LINE
                           DISPLAY REPORT-LINE

                   MOVE "---------------------------------------------"&
                   "-----------------" TO REPORT-LINE
                           WRITE REPORT-LINE
                           DISPLAY REPORT-LINE

                           MOVE WS-CURRENT-COURSE TO WS-PREV-COURSE
                       END-IF

                       STRING WS-ID DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              WS-NAME(1:18) DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              WS-BIRTHDAY DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              WS-INSERT-DATE DELIMITED BY SIZE
                              " | " DELIMITED BY SIZE
                              WS-UPDATE-DATE DELIMITED BY SIZE
                              INTO REPORT-LINE
                       WRITE REPORT-LINE
                       DISPLAY REPORT-LINE

                       ADD 1 TO WS-STUDENT-COUNT
               END-READ
           END-PERFORM

          MOVE "------------------------------------------------------"&
          "--------" TO REPORT-LINE
           WRITE REPORT-LINE
           DISPLAY REPORT-LINE

           STRING "TOTAL STUDENTS : " DELIMITED BY SIZE
                  WS-STUDENT-COUNT DELIMITED BY SIZE
                  INTO REPORT-LINE
           WRITE REPORT-LINE
           DISPLAY REPORT-LINE

           CLOSE STUDENT-FILE
           CLOSE REPORT-FILE

           EXIT PROGRAM.
