       IDENTIFICATION DIVISION.
       PROGRAM-ID. validate-password.
       
       DATA DIVISION.

           WORKING-STORAGE SECTION.
           01 COUNTER UNSIGNED-INT.  
           01 WS-LENGTH PIC 99. 
           01 WS-NUMBERS PIC 9. 
           01 PWORD PIC X(20).

           LINKAGE SECTION.
           01 LS-NEW-PWORD PIC X(20).
           01 LS-ER-MSG PIC X(50). 
           01 LS-RAISE-ERROR PIC 9. 
           01 LS-OK-MSG PIC X(50).
       
       PROCEDURE DIVISION USING LS-NEW-PWORD LS-ER-MSG LS-RAISE-ERROR
       LS-OK-MSG. 
       
           MOVE 0 TO LS-RAISE-ERROR.
           INSPECT LS-NEW-PWORD TALLYING WS-NUMBERS FOR ALL "1" "2"
           "3" "4" "5" "6" "7" "8" "9".   

           MOVE LS-NEW-PWORD TO PWORD. 

           INSPECT PWORD REPLACING ALL 'a' BY '*'.
           INSPECT PWORD REPLACING ALL 'b' BY '*'.
           INSPECT PWORD REPLACING ALL 'c' BY '*'.
           INSPECT PWORD REPLACING ALL 'd' BY '*'.
           INSPECT PWORD REPLACING ALL 'e' BY '*'.
           INSPECT PWORD REPLACING ALL 'f' BY '*'.
           INSPECT PWORD REPLACING ALL 'g' BY '*'.
           INSPECT PWORD REPLACING ALL 'h' BY '*'.
           INSPECT PWORD REPLACING ALL 'i' BY '*'.
           INSPECT PWORD REPLACING ALL 'j' BY '*'.
           INSPECT PWORD REPLACING ALL 'k' BY '*'.
           INSPECT PWORD REPLACING ALL 'l' BY '*'.
           INSPECT PWORD REPLACING ALL 'm' BY '*'.
           INSPECT PWORD REPLACING ALL 'n' BY '*'.
           INSPECT PWORD REPLACING ALL 'o' BY '*'.
           INSPECT PWORD REPLACING ALL 'p' BY '*'.
           INSPECT PWORD REPLACING ALL 'q' BY '*'.
           INSPECT PWORD REPLACING ALL 'r' BY '*'.
           INSPECT PWORD REPLACING ALL 's' BY '*'.
           INSPECT PWORD REPLACING ALL 't' BY '*'.
           INSPECT PWORD REPLACING ALL 'u' BY '*'.
           INSPECT PWORD REPLACING ALL 'v' BY '*'.
           INSPECT PWORD REPLACING ALL 'w' BY '*'.
           INSPECT PWORD REPLACING ALL 'x' BY '*'.
           INSPECT PWORD REPLACING ALL 'y' BY '*'.
           INSPECT PWORD REPLACING ALL 'z' BY '*'.
           INSPECT PWORD REPLACING ALL '0' BY '*'.
           INSPECT PWORD REPLACING ALL '1' BY '*'.
           INSPECT PWORD REPLACING ALL '2' BY '*'.
           INSPECT PWORD REPLACING ALL '3' BY '*'.
           INSPECT PWORD REPLACING ALL '4' BY '*'.
           INSPECT PWORD REPLACING ALL '5' BY '*'.
           INSPECT PWORD REPLACING ALL '6' BY '*'.
           INSPECT PWORD REPLACING ALL '7' BY '*'.
           INSPECT PWORD REPLACING ALL '8' BY '*'.
           INSPECT PWORD REPLACING ALL '9' BY '*'.

           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LENGTH.
           PERFORM UNTIL COUNTER = 21
             IF '*' EQUALS PWORD(COUNTER:1) 
              THEN ADD 1 TO WS-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.

           MOVE SPACES TO LS-ER-MSG.
           MOVE SPACES TO LS-OK-MSG. 
         
           IF WS-NUMBERS < 1 AND WS-LENGTH < 6 THEN 
               MOVE "MUST BE AT LEAST 6 CHARACTERS & CONTAIN A NUMBER" 
               TO LS-ER-MSG
               ADD 1 TO LS-RAISE-ERROR
           ELSE IF WS-NUMBERS < 1 AND WS-LENGTH > 5 THEN
               MOVE "PASSWORD MUST CONTAIN AT LEAST 1 NUMBER" TO 
               LS-ER-MSG
               ADD 1 TO LS-RAISE-ERROR
           ELSE IF WS-NUMBERS > 0 AND WS-LENGTH < 6 THEN
               MOVE "PASSWORD MUST HAVE A MIN OF 6 CHARACTERS" TO 
               LS-ER-MSG
               ADD 1 TO LS-RAISE-ERROR
           ELSE 
               MOVE "PASSWORD STRENGTH GOOD" TO LS-OK-MSG
           END-IF. 
