       IDENTIFICATION DIVISION.
       PROGRAM-ID. validate-bank-details.
       
       DATA DIVISION.

           WORKING-STORAGE SECTION.
           01 COUNTER PIC 9. 
           01 WS-ACCOUNT PIC X(8).
           01 WS-LENGTH PIC 99. 

           
           LINKAGE SECTION.
           01 LS-ACCOUNT-NUM PIC X(8).
           01 LS-ERR-MSG PIC X(50).
           01 LS-RAISE-ERROR PIC 9. 
           
           
       PROCEDURE DIVISION USING LS-ACCOUNT-NUM LS-ERR-MSG 
       LS-RAISE-ERROR. 
           
           MOVE LS-ACCOUNT-NUM TO WS-ACCOUNT. 
           
           INSPECT WS-ACCOUNT REPLACING ALL '0' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '1' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '2' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '3' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '4' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '5' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '6' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '7' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '8' BY '*'.
           INSPECT WS-ACCOUNT REPLACING ALL '9' BY '*'.

           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LENGTH.
           PERFORM UNTIL COUNTER = 9
             IF '*' EQUALS WS-ACCOUNT(COUNTER:1) 
              THEN ADD 1 TO WS-LENGTH
             END-IF
             ADD 1 TO COUNTER
           END-PERFORM.

           IF WS-LENGTH < 8 
               MOVE "INVALID BANK DETAILS" TO LS-ERR-MSG
               ADD 1 TO LS-RAISE-ERROR
           END-IF. 
           