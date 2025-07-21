       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMENU.                              *> Main program to display a menu and call other sub-programs

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-CHOICE-RAW  PIC X.                        *> Raw user input as character
       01 USER-CHOICE      PIC 9.                        *> Converted numeric value from USER-CHOICE-RAW
       01 EXIT-FLAG        PIC X VALUE "N".              *> Loop control flag (Y = exit)

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM UNTIL EXIT-FLAG = "Y"                 *> Loop until user chooses to exit
               DISPLAY "==============================="
               DISPLAY "          MAIN MENU"
               DISPLAY "==============================="
               DISPLAY "1 - GENERATE VSAM FILE"         *> Calls PRGV0001
               DISPLAY "2 - INSERT STUDENT DATA"        *> Calls PRGI0002
               DISPLAY "3 - UPDATE STUDENT DATA"        *> Calls PRGU0003
               DISPLAY "4 - DELETE STUDENT DATA"        *> Calls PRGD0004
               DISPLAY "5 - CLASS QUERY (ALL STUDENTS)" *> Calls PRGQ0005
               DISPLAY "6 - QUERY STUDENT BY ID"        *> Calls PRGQ0006
               DISPLAY "7 - QUERY BY DATE OF INCLUSION" *> Calls PRGQ0007
               DISPLAY "8 - REPORT FILE WITH DATE BREAK"*> Calls PRGR0008
               DISPLAY "9 - EXIT"                       *> Ends program
               DISPLAY "CHOOSE YOUR OPTION (1 TO 9) >> "
               ACCEPT USER-CHOICE-RAW                   *> Accept user input (as character)
               COMPUTE USER-CHOICE = FUNCTION NUMVAL(USER-CHOICE-RAW)
                                                        *> Convert character to numeric

               EVALUATE USER-CHOICE                    *> Choose operation based on user input
                   WHEN 1
                       CALL "PRGV0001"                 *> Generate VSAM file from students.txt
                   WHEN 2
                       CALL "PRGI0002"                 *> Insert new student
                   WHEN 3
                       CALL "PRGU0003"                 *> Update student by ID
                   WHEN 4
                       CALL "PRGD0004"                 *> Delete student by ID
                   WHEN 5
                       CALL "PRGQ0005"                 *> Display all students
                   WHEN 6
                       CALL "PRGQ0006"                 *> Search student by ID
                   WHEN 7
                       CALL "PRGQ0007"                 *> Search students by inclusion date
                   WHEN 8
                       CALL "PRGR0008"                 *> Generate grouped report by date
                   WHEN 9
                       MOVE "Y" TO EXIT-FLAG           *> Exit the loop
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM

           DISPLAY "Exiting program. Goodbye!"         *> Final message
           STOP RUN.                                   *> End program
