       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGV0001.             *> Program to convert initial student data

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "initial.txt"       *> Input source file
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "vsam-simulated.txt" *> Output target file
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE           PIC X(100).  *> One line of input text

       FD OUTPUT-FILE.
       01 OUTPUT-LINE          PIC X(150).  *> One line to write in output

       WORKING-STORAGE SECTION.
       01 EOF-FLAG             PIC X VALUE "N".  *> End-of-file flag

       01 WS-FIELD1            PIC X(4).      *> Student ID
       01 WS-FIELD2            PIC X(18).     *> Student Name
       01 WS-FIELD3            PIC X(8).      *> Birthdate (YYYYMMDD)
       01 WS-FIELD4            PIC X(8).      *> Course name

       01 WS-INSERT-DATE       PIC X(8).      *> Date when the record is added
       01 WS-UPDATE-DATE       PIC X(8) VALUE "00000000".  *> Default update date

       01 WS-CURRENT-DATE.
           05 WS-YEAR          PIC X(4).     *> Current year
           05 WS-MONTH         PIC X(2).     *> Current month
           05 WS-DAY           PIC X(2).     *> Current day

       01 WS-CSV-LINE          PIC X(150).   *> Line combining all fields

       PROCEDURE DIVISION.
       BEGIN.

           *> Get current system date
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-CURRENT-DATE(1:4) TO WS-YEAR
           MOVE WS-CURRENT-DATE(5:2) TO WS-MONTH
           MOVE WS-CURRENT-DATE(7:2) TO WS-DAY

           *> Create INSERT-DATE in format YYYYMMDD
           STRING WS-YEAR WS-MONTH WS-DAY INTO WS-INSERT-DATE

           *> Open the input and output files
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           *> Loop until end of file is reached
           PERFORM UNTIL EOF-FLAG = "Y"
               READ INPUT-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG  *> Stop reading
                   NOT AT END
                       *> Break line into fields separated by commas
                       UNSTRING INPUT-LINE DELIMITED BY ","
                           INTO WS-FIELD1 WS-FIELD2 WS-FIELD3 WS-FIELD4

                       *> Combine all fields into a CSV-formatted line
                       STRING
                           WS-FIELD1 DELIMITED BY SIZE "," 
                           WS-FIELD2 DELIMITED BY SIZE "," 
                           WS-FIELD3 DELIMITED BY SIZE "," 
                           WS-FIELD4 DELIMITED BY SIZE "," 
                           WS-INSERT-DATE DELIMITED BY SIZE "," 
                           WS-UPDATE-DATE DELIMITED BY SIZE
                           INTO WS-CSV-LINE

                       *> Write the formatted line into output file
                       MOVE WS-CSV-LINE TO OUTPUT-LINE
                       WRITE OUTPUT-LINE
               END-READ
           END-PERFORM

           *> Close both files
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE

           *> Notify the user
           DISPLAY "Initial data converted with today's date."

           EXIT PROGRAM.
