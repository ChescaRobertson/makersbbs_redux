       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-validate-bank-details.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 ACCOUNT-NUM PIC X(8).
           01 ERR-MSG PIC X(50).
           01 RAISE-ERROR PIC 9. 

       PROCEDURE DIVISION.
           
           TEST-ACCOUNT-NUMBER-WRONG-LENGTH. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "123" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING "INVALID BANK DETAILS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.
           
           TEST-ACCOUNT-NUMBER-WRONG-CHARACTERS. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "123abc78" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING "INVALID BANK DETAILS" ERR-MSG.
           CALL 'assert-equals' USING "1" RAISE-ERROR.

           TEST-ACCOUNT-NUMBER-OK. 

           MOVE SPACES TO ERR-MSG.
           MOVE 0 TO RAISE-ERROR.
           MOVE "12345678" TO ACCOUNT-NUM.

           CALL 'validate-bank-details' USING ACCOUNT-NUM ERR-MSG 
           RAISE-ERROR.
           CALL 'assert-equals' USING " " ERR-MSG.
           CALL 'assert-equals' USING "0" RAISE-ERROR.
           