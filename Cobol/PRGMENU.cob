       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMENU.                              *> Main menu program

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 USER-CHOICE-RAW  PIC X(10).                    *> User input (up to 10 chars)
       01 USER-CHOICE      PIC 9(2).                     *> Parsed numeric choice
       01 EXIT-FLAG        PIC X VALUE "N".              *> Loop control flag ("Y" to exit)

       PROCEDURE DIVISION.
       BEGIN.

           *> Loop until user decides to exit the program
           PERFORM UNTIL EXIT-FLAG = "Y"

               *> Display the menu header and options
               DISPLAY "==============================="
               DISPLAY "          MAIN MENU"
               DISPLAY "==============================="
               DISPLAY "1 - GENERATE VSAM FILE"
               DISPLAY "2 - INSERT STUDENT DATA"
               DISPLAY "3 - UPDATE STUDENT DATA"
               DISPLAY "4 - DELETE STUDENT DATA"
               DISPLAY "5 - CLASS QUERY (ALL STUDENTS)"
               DISPLAY "6 - QUERY STUDENT BY ID"
               DISPLAY "7 - QUERY BY DATE OF INCLUSION"
               DISPLAY "8 - REPORT FILE WITH DATE BREAK"
               DISPLAY "9 - EXIT"

               *> Ask the user to choose an option
               DISPLAY "CHOOSE YOUR OPTION (1 TO 9) >> "
               ACCEPT USER-CHOICE-RAW                    *> Accept the raw input string

               *> Evaluate if the choice is valid (between 1 and 9)
               EVALUATE TRUE

                   *> If the numeric value is not between 1 and 9, show an error
                   WHEN FUNCTION NUMVAL(USER-CHOICE-RAW) < 1
                        OR FUNCTION NUMVAL(USER-CHOICE-RAW) > 9
                       DISPLAY "Invalid choice. Try again."

                   *> Otherwise, compute the numeric value and perform action
                   WHEN OTHER
                  COMPUTE USER-CHOICE = FUNCTION NUMVAL(USER-CHOICE-RAW)

                       *> Evaluate the numeric input and call the corresponding program
                       EVALUATE USER-CHOICE
                           WHEN 1
                               CALL "PRGV0001"           *> Generate VSAM from initial file
                           WHEN 2
                               CALL "PRGI0002"           *> Insert a new student
                           WHEN 3
                               CALL "PRGU0003"           *> Update student by ID
                           WHEN 4
                               CALL "PRGD0004"           *> Delete student by ID
                           WHEN 5
                               CALL "PRGQ0005"           *> List all students
                           WHEN 6
                               CALL "PRGQ0006"           *> Search student by ID
                           WHEN 7
                               CALL "PRGQ0007"           *> Search by inclusion date
                           WHEN 8
                               CALL "PRGR0008"           *> Generate report grouped by course
                           WHEN 9
                               MOVE "Y" TO EXIT-FLAG     *> Set exit flag to break the loop
                       END-EVALUATE
               END-EVALUATE

           END-PERFORM

           *> End message
           DISPLAY "Exiting program. Goodbye!"
           STOP RUN.                                    *> End the program
