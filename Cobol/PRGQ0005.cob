       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGQ0005.                         *> Program name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     *> Input student file

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE         PIC X(100).           *> Each line from student file

       WORKING-STORAGE SECTION.
       01 EOF-FLAG             PIC X VALUE "N".      *> End-of-file flag

       01 WS-STUDENT-ID        PIC X(4).             *> Student ID
       01 WS-STUDENT-NAME      PIC X(18).            *> Student Name
       01 WS-BIRTHDATE         PIC X(8).             *> Birthday (YYYYMMDD)
       01 WS-COURSE            PIC X(8).             *> Course name
       01 WS-INCLUSION-DATE    PIC X(8).             *> Date of inclusion
       01 WS-UPDATE-DATE       PIC X(8).             *> Last update date

       01 WS-FIELDS OCCURS 6 TIMES INDEXED BY IDX.   *> Temporary storage of parsed fields
           05 WS-FIELD         PIC X(20).            *> Each field holds a segment of the line

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT STUDENT-FILE                   *> Open the student file

           DISPLAY "---------------------------------------------------"
           DISPLAY " C L A S S   R E P O R T"            *> Report header
           DISPLAY "---------------------------------------------------"
           DISPLAY " ID  | STUDENT NAME | BIRTHDAY | COURSE | INS | UPT"
           DISPLAY "--------------------------------------------------"

           PERFORM UNTIL EOF-FLAG = "Y"              *> Read loop until end of file
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG          *> Set flag when file ends
                   NOT AT END
                       PERFORM PARSE-LINE            *> Split line into fields
                       PERFORM DISPLAY-STUDENT       *> Show the parsed fields
               END-READ
           END-PERFORM

           DISPLAY "-------------------"              *> Footer

           CLOSE STUDENT-FILE                        *> Close file
           EXIT PROGRAM.                             *> End program

       PARSE-LINE.
           UNSTRING STUDENT-LINE DELIMITED BY ","    *> Split CSV line into parts
               INTO WS-FIELDS (1) WS-FIELDS (2) WS-FIELDS (3)
                    WS-FIELDS (4) WS-FIELDS (5) WS-FIELDS (6)

           MOVE WS-FIELDS (1) TO WS-STUDENT-ID       *> Assign fields to named variables
           MOVE WS-FIELDS (2) TO WS-STUDENT-NAME
           MOVE WS-FIELDS (3) TO WS-BIRTHDATE
           MOVE WS-FIELDS (4) TO WS-COURSE
           MOVE WS-FIELDS (5) TO WS-INCLUSION-DATE
           MOVE WS-FIELDS (6) TO WS-UPDATE-DATE.

       DISPLAY-STUDENT.
           DISPLAY WS-STUDENT-ID " | "               *> Show formatted student data
               WS-STUDENT-NAME " | "
               WS-BIRTHDATE " | "
               WS-COURSE " | "
               WS-INCLUSION-DATE " | "
               WS-UPDATE-DATE.
