       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-process-payment.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CONV-MON-TO-CRED.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 BANK-ACCOUNT PIC X(8).
           01 CREDITS PIC 999.
           01 WS-PAYMENT-STATUS PIC X(20).

       PROCEDURE DIVISION.
       
       TEST-CONV-MON-TO-CRED.
           CALL "assert-equals" USING CONV-MON-TO-CRED("030.00")'300'.

       TEST-PROCESS-SINGLE-PAYMENT.
           MOVE "98765432" TO BANK-ACCOUNT
           MOVE "300" TO CREDITS
           CALL 'process-single-payment' USING BANK-ACCOUNT, CREDITS.
          
       TEST-PROCESS-BANK-STATEMENT.
           MOVE "PENDING" TO WS-PAYMENT-STATUS.
           SET ENVIRONMENT "users_dat" TO "users-bank-statement.dat".
           SET ENVIRONMENT "transactions_dat" 
             TO "process-transactions.dat"
           CALL "process-bank-statement" USING WS-PAYMENT-STATUS.
    