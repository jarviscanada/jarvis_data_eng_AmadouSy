       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGR0008.                     *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL. *> Input file with student data
           SELECT REPORT-FILE ASSIGN TO "report.txt"
               ORGANIZATION IS LINE SEQUENTIAL. *> Output report file

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE            PIC X(100).   *> Each input line is 100 chars

       FD REPORT-FILE.
       01 REPORT-LINE             PIC X(100).   *> Each report line is 100 chars

       WORKING-STORAGE SECTION.
       01 WS-EOF                  PIC X VALUE 'N'.       *> End-of-file flag
       01 WS-STUDENT-COUNT        PIC 9(4) VALUE 0.      *> Student counter

       01 WS-ID                   PIC X(4).              *> Student ID
       01 WS-NAME                 PIC X(25).             *> Student name
       01 WS-BIRTHDAY             PIC X(8).              *> Date of birth
       01 WS-COURSE               PIC X(15).             *> Course name
       01 WS-INSERT-DATE          PIC X(8).              *> Inserted on
       01 WS-UPDATE-DATE          PIC X(8).              *> Updated on

       01 WS-PREV-COURSE          PIC X(15) VALUE SPACES. *> Last course printed
       01 WS-CURRENT-COURSE       PIC X(15).              *> Current course

       PROCEDURE DIVISION.
       BEGIN.

           OPEN INPUT STUDENT-FILE                *> Open input file
           OPEN OUTPUT REPORT-FILE                *> Open output report file

          MOVE "------------------------------------------------------"&
          "--------" TO REPORT-LINE              *> Top separator
           WRITE REPORT-LINE
           DISPLAY REPORT-LINE

           MOVE "               C L A S S   R E P O R T" TO REPORT-LINE
           WRITE REPORT-LINE                      *> Report title
           DISPLAY REPORT-LINE

           PERFORM UNTIL WS-EOF = 'Y'             *> Loop until end of file
               READ STUDENT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF         *> Set EOF flag
                   NOT AT END
                       UNSTRING STUDENT-LINE DELIMITED BY ","  *> Parse line
                           INTO WS-ID, WS-NAME, WS-BIRTHDAY, WS-COURSE,
                                WS-INSERT-DATE, WS-UPDATE-DATE

                       MOVE WS-COURSE TO WS-CURRENT-COURSE

                       IF WS-CURRENT-COURSE NOT = WS-PREV-COURSE  *> New course
                           MOVE SPACES TO REPORT-LINE
                           WRITE REPORT-LINE                      *> Blank line
                           DISPLAY REPORT-LINE

                           STRING "   COURSE: " DELIMITED BY SIZE
                                  WS-CURRENT-COURSE DELIMITED BY SIZE
                                  INTO REPORT-LINE
                           WRITE REPORT-LINE                      *> Course title
                           DISPLAY REPORT-LINE

                   MOVE "--------------------------------------------"&
                   "------------------" TO REPORT-LINE
                           WRITE REPORT-LINE                      *> Separator
                           DISPLAY REPORT-LINE

                    MOVE "ID   | STUDENT NAME       | BIRTHDAY | INSRT"& 
                    "    | UPDATE" 
                                TO REPORT-LINE
                           WRITE REPORT-LINE                      *> Header line
                           DISPLAY REPORT-LINE

                   MOVE "---------------------------------------------"&
                   "-----------------" TO REPORT-LINE
                           WRITE REPORT-LINE                      *> Separator
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
                       WRITE REPORT-LINE                      *> Student line
                       DISPLAY REPORT-LINE

                       ADD 1 TO WS-STUDENT-COUNT              *> Count student
               END-READ
           END-PERFORM

          MOVE "------------------------------------------------------"&
          "--------" TO REPORT-LINE
           WRITE REPORT-LINE                          *> Final separator
           DISPLAY REPORT-LINE

           STRING "TOTAL STUDENTS : " DELIMITED BY SIZE
                  WS-STUDENT-COUNT DELIMITED BY SIZE
                  INTO REPORT-LINE
           WRITE REPORT-LINE                          *> Print total
           DISPLAY REPORT-LINE

           CLOSE STUDENT-FILE
           CLOSE REPORT-FILE

           EXIT PROGRAM.                              *> End of program
