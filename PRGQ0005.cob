       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0005.

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
       01 EOF-FLAG             PIC X VALUE "N".

       01 WS-STUDENT-ID        PIC X(4).
       01 WS-STUDENT-NAME      PIC X(18).
       01 WS-BIRTHDATE         PIC X(8).
       01 WS-COURSE            PIC X(8).
       01 WS-INCLUSION-DATE    PIC X(8).
       01 WS-UPDATE-DATE       PIC X(8).

       01 WS-FIELDS OCCURS 6 TIMES INDEXED BY IDX.
           05 WS-FIELD         PIC X(20).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT STUDENT-FILE

           DISPLAY "---------------------------------------------------"
           DISPLAY " C L A S S   R E P O R T"
           DISPLAY "---------------------------------------------------"
           DISPLAY " ID  | STUDENT NAME | BIRTHDAY | COURSE | INS | UPT"
           DISPLAY "--------------------------------------------------"

           PERFORM UNTIL EOF-FLAG = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       PERFORM PARSE-LINE
                       PERFORM DISPLAY-STUDENT
               END-READ
           END-PERFORM

           DISPLAY "-------------------"

           CLOSE STUDENT-FILE
           EXIT PROGRAM.

       PARSE-LINE.
           UNSTRING STUDENT-LINE DELIMITED BY ","
               INTO WS-FIELDS (1) WS-FIELDS (2) WS-FIELDS (3)
                    WS-FIELDS (4) WS-FIELDS (5) WS-FIELDS (6)

           MOVE WS-FIELDS (1) TO WS-STUDENT-ID
           MOVE WS-FIELDS (2) TO WS-STUDENT-NAME
           MOVE WS-FIELDS (3) TO WS-BIRTHDATE
           MOVE WS-FIELDS (4) TO WS-COURSE
           MOVE WS-FIELDS (5) TO WS-INCLUSION-DATE
           MOVE WS-FIELDS (6) TO WS-UPDATE-DATE.

       DISPLAY-STUDENT.
           DISPLAY WS-STUDENT-ID " | "
               WS-STUDENT-NAME " | "
               WS-BIRTHDATE " | "
               WS-COURSE " | "
               WS-INCLUSION-DATE " | "
               WS-UPDATE-DATE.
