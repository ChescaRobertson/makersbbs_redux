       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-validate-bank-details.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 ACCOUNT-NUM PIC X(8).
           01 ERR-MSG PIC X(50).
           01 RAISE-ERROR PIC 9. 
           01 OK-MSG PIC X(50). 

       PROCEDURE DIVISION.
           
      * Account number must be 8 characters and can only include 
      * numbers. 

           TEST-ACCOUNT-NUMBER-WRONG-LENGTH. 

      * Account number fails as it is not the correct length. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "123" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING "INVALID BANK DETAILS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           CALL 'assert-equals' USING " " OK-MSG.
           
           TEST-ACCOUNT-NUMBER-WRONG-CHARACTERS. 
      
      * Account number fails as it includes characters that are not
      * permitted. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "123abc*8" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING "INVALID BANK DETAILS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           CALL 'assert-equals' USING " " OK-MSG.

           TEST-ACCOUNT-NUMBER-OK. 

      * Account number is accepted. 

           MOVE SPACES TO ERR-MSG.
           MOVE SPACES TO OK-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "12345678" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR OK-MSG.
           CALL 'assert-equals' USING " " ERR-MSG.
           CALL 'assert-equals' USING "0" RAISE-ERROR.
           CALL 'assert-equals' USING "ACCOUNT NUMBER VALID" OK-MSG.

