       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGI0002.               *> Program to add a new student record

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "vsam-simulated.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           *> Define the file where student records will be stored/appended

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-LINE        PIC X(150).  *> One line per student in the file

       WORKING-STORAGE SECTION.
       01 WS-STUDENT-ID       PIC X(4).   *> Input: Student ID
       01 WS-STUDENT-NAME     PIC X(18).  *> Input: Student name
       01 WS-BIRTHDATE        PIC X(8).   *> Input: Birthday (YYYYMMDD)
       01 WS-COURSE           PIC X(8).   *> Input: Course name
       01 WS-INSERT-DATE      PIC X(8).   *> Automatically generated current date
       01 WS-UPDATE-DATE      PIC X(8) VALUE "00000000". *> No update yet
       01 WS-OUTPUT-LINE      PIC X(150). *> Formatted CSV line for writing

       01 WS-CURRENT-DATE.
           05 WS-YEAR         PIC X(4).   *> System year
           05 WS-MONTH        PIC X(2).   *> System month
           05 WS-DAY          PIC X(2).   *> System day

       PROCEDURE DIVISION.
       BEGIN.

           *> Display title banner
           DISPLAY "+-----------------------------------+"
           DISPLAY "|   A D D   N E W   S T U D E N T   |"
           DISPLAY "+-----------------------------------+"

           *> Get today's system date and format it as YYYYMMDD
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-CURRENT-DATE(1:4) TO WS-YEAR
           MOVE WS-CURRENT-DATE(5:2) TO WS-MONTH
           MOVE WS-CURRENT-DATE(7:2) TO WS-DAY
           STRING WS-YEAR WS-MONTH WS-DAY INTO WS-INSERT-DATE

           *> Prompt the user for student details
           DISPLAY "ENTER STUDENT ID (4 DIGITS) >>"
           ACCEPT WS-STUDENT-ID

           DISPLAY "ENTER FULL NAME (MAX 25 CHARS) >>"
           ACCEPT WS-STUDENT-NAME

           DISPLAY "ENTER BIRTHDAY (YYYYMMDD) >>"
           ACCEPT WS-BIRTHDATE

           DISPLAY "ENTER COURSE (MAX 15 CHARS) >>"
           ACCEPT WS-COURSE

           *> Open the student file in EXTEND mode (to append at the end)
           OPEN EXTEND STUDENT-FILE

           *> Create a comma-separated record line
           STRING
               WS-STUDENT-ID DELIMITED BY SIZE ","
               WS-STUDENT-NAME DELIMITED BY SIZE ","
               WS-BIRTHDATE DELIMITED BY SIZE ","
               WS-COURSE DELIMITED BY SIZE ","
               WS-INSERT-DATE DELIMITED BY SIZE ","
               WS-UPDATE-DATE DELIMITED BY SIZE
               INTO WS-OUTPUT-LINE

           *> Write the new record to the file
           WRITE STUDENT-LINE FROM WS-OUTPUT-LINE

           *> Close the file
           CLOSE STUDENT-FILE

           *> Notify user of success
           DISPLAY "STUDENT ADDED SUCCESSFULLY."

           EXIT PROGRAM.
