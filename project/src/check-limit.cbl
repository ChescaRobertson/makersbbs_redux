           IDENTIFICATION DIVISION.
           FUNCTION-ID. CHECK-LIMIT.
           ENVIRONMENT DIVISION.
        
           DATA DIVISION.
   
           LINKAGE SECTION.
           01 LS-CREDIT-AMOUNT PIC 999.
           01 LS-CREDIT-BALANCE PIC 999.
           01 LS-RESULT PIC X(5).

       PROCEDURE DIVISION USING LS-CREDIT-AMOUNT, LS-CREDIT-BALANCE 
       RETURNING LS-RESULT.
           
           IF LS-CREDIT-AMOUNT + LS-CREDIT-BALANCE >= 1000
               MOVE "FAIL" TO LS-RESULT
           ELSE 
               MOVE "PASS" TO LS-RESULT
           END-IF.
           
           END FUNCTION CHECK-LIMIT.
           