       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-deduct-credits.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CHECK-BALANCE.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-COST PIC 999.
           01 LS-CREDIT-BALANCE PIC 999.
        
       PROCEDURE DIVISION.

       TEST-CHECK-BALANCE-PASS.
           MOVE "20" TO LS-CREDIT-BALANCE
           MOVE "5" TO LS-COST
           CALL "assert-equals" USING CHECK-BALANCE(LS-COST, 
           LS-CREDIT-BALANCE) "TRUE".
           
       TEST-CHECK-BALANCE-FAIL.
           MOVE "4" TO LS-CREDIT-BALANCE
           MOVE "5" TO LS-COST
           CALL "assert-equals" USING CHECK-BALANCE(LS-COST, 
           LS-CREDIT-BALANCE) "FALSE".

       TEST-DEDUCT-CREDITS.
           MOVE "Chesca" TO LS-USERNAME
           MOVE "5" TO LS-COST
           SET ENVIRONMENT "users_dat" TO "users-spend-credits.dat".
           CALL "deduct-credits" USING LS-USERNAME, LS-COST, 
           LS-CREDIT-BALANCE.
           
           
       
   