       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-validate-password.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-PASSWORD PIC X(20).
           01 ERR-MSG PIC X(50).
           01 RAISE-ERROR PIC 9. 

       PROCEDURE DIVISION.
           
           TEST-PASSWORD-CORRECT-LENGTH-NO-NUM. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abcdef" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING 
           "PASSWORD MUST CONTAIN AT LEAST 1 NUMBER" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           
           TEST-PASSWORD-WRONG-LENGTH-NO-NUM. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abc" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING 
           "MUST BE AT LEAST 6 CHARACTERS & CONTAIN A NUMBER" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           
           TEST-PASSWORD-WRONG-LENGTH-INCLUDES-NUM. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abc1" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING 
           "PASSWORD MUST HAVE A MIN OF 6 CHARACTERS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           

           TEST-PASSWORD-OK. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "abcde1" TO WS-PASSWORD.

           CALL 'validate-password' USING WS-PASSWORD ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING " " ERR-MSG.
           CALL 'assert-equals' USING "0" RAISE-ERROR.