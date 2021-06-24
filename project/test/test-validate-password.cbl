       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-validate-password.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-PASSWORD PIC X(20).
           01 ERR-MSG PIC X(50).
           01 RAISE-ERROR PIC 9.
           01 OK-MSG PIC X(50). 

       PROCEDURE DIVISION.
           
      * Password must be at least 6 characters and include a minimum
      * of 1 number.  

           TEST-PASSWORD-CORRECT-LENGTH-NO-NUM. 

      * Password fails as although it is the correct length, it does
      * not inlude a number. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abcdef" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING 
           "PASSWORD MUST CONTAIN AT LEAST 1 NUMBER" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           CALL 'assert-equals' USING " " OK-MSG.

           TEST-PASSWORD-WRONG-LENGTH-NO-NUM. 

      * Password fails as it is not 6 characters and does not include 
      * a number. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abc" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING 
           "MUST BE AT LEAST 6 CHARACTERS & CONTAIN A NUMBER" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           CALL 'assert-equals' USING " " OK-MSG.
           
           TEST-PASSWORD-WRONG-LENGTH-INCLUDES-NUM. 
      
      * Password fails as althugh it includes a number, it is not 
      * 6 characters. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abc1" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING 
           "PASSWORD MUST HAVE A MIN OF 6 CHARACTERS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           CALL 'assert-equals' USING " " OK-MSG.
           
           TEST-PASSWORD-OK. 
       
      * Password is permitted. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abcde1" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING " " ERR-MSG.
           CALL 'assert-equals' USING "0" RAISE-ERROR.
           CALL 'assert-equals' USING "PASSWORD STRENGTH GOOD" OK-MSG.
