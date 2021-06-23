       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-buy-credits.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CONV-CRED-TO-MON
               FUNCTION VERIFY-PASSWORD.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-CREDITS PIC 999.
           01 GAP1 PIC X(10).
           01 LS-MON-AMOUNT PIC 999.99.
           01 GAP2 PIC X(10).

       PROCEDURE DIVISION.
           
       TEST-ADD-TO-TRANSACTIONS.
           MOVE "Jim" TO LS-USERNAME.
           MOVE "300" TO LS-CREDITS.
           MOVE "          " TO GAP1.
           MOVE "030.00" TO LS-MON-AMOUNT.
           MOVE "          " TO GAP2.
           SET ENVIRONMENT "transactions_dat" TO "transactions.dat".
           CALL "add-to-transactions" USING LS-USERNAME, LS-CREDITS, 
           GAP1, LS-MON-AMOUNT, GAP2.

       TEST-CONV-CRED-TO-MON.
           CALL "assert-equals" USING CONV-CRED-TO-MON("300")'030.00'.

       TEST-VERIFY-PASSWORD.
           CALL "assert-equals" USING VERIFY-PASSWORD("Correct-Password"
             , "Incorrect-Password") "FALSE".

           CALL "assert-equals" USING VERIFY-PASSWORD("Correct-Password"
             ,"Correct-Password") "TRUE".