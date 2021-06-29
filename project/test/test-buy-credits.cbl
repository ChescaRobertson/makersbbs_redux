       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-buy-credits.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CONV-CRED-TO-MON
               FUNCTION VERIFY-PASSWORD
               FUNCTION CHECK-LIMIT.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-ACCOUNT-NUM PIC X(10).
           01 LS-CREDITS PIC 999.
           01 LS-DATE-OF-TRANS PIC X(8).
           01 LS-PAYMENT-STATUS PIC X(20).

           01 CREDIT-AMOUNT PIC 999.
           01 CREDIT-BALANCE PIC 999.


       PROCEDURE DIVISION.
           
       TEST-ADD-TO-TRANSACTIONS.
           MOVE "Jim" TO LS-USERNAME.
           MOVE "12345678" TO LS-ACCOUNT-NUM.
           MOVE "300" TO LS-CREDITS.
           MOVE "20210628" TO LS-DATE-OF-TRANS
           SET ENVIRONMENT "transactions_dat" TO "transactions.dat".
           CALL "add-to-transactions" USING LS-USERNAME, LS-ACCOUNT-NUM,
           LS-CREDITS, LS-DATE-OF-TRANS.


       TEST-VERIFY-PASSWORD.
           CALL "assert-equals" USING VERIFY-PASSWORD("Correct-Password"
             , "Incorrect-Password") "FALSE".

           CALL "assert-equals" USING VERIFY-PASSWORD("Correct-Password"
             ,"Correct-Password") "TRUE".

       TEST-CHECK-LIMIT-PASS.
           MOVE "300" TO CREDIT-BALANCE
           MOVE "100" TO CREDIT-AMOUNT
           CALL "assert-equals" USING CHECK-LIMIT(CREDIT-AMOUNT, 
           CREDIT-BALANCE) "PASS".
       
       TEST-CHECK-LIMIT-FAIL.
           MOVE "900" TO CREDIT-BALANCE
           MOVE "100" TO CREDIT-AMOUNT
           CALL "assert-equals" USING CHECK-LIMIT(CREDIT-AMOUNT, 
           CREDIT-BALANCE) "FAIL".
           