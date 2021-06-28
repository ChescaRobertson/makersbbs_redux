           IDENTIFICATION DIVISION.
           FUNCTION-ID. CHECK-BALANCE.
           ENVIRONMENT DIVISION.
        
           DATA DIVISION.
   
           LINKAGE SECTION.
           01 LS-COST PIC 999.
           01 LS-CREDIT-BALANCE PIC 999.
           01 LS-RESULT PIC X(5).

       PROCEDURE DIVISION USING LS-COST, LS-CREDIT-BALANCE RETURNING
       LS-RESULT.
           
           IF LS-CREDIT-BALANCE - LS-COST >= 0
               MOVE "TRUE" TO LS-RESULT
           ELSE 
               MOVE "FALSE" TO LS-RESULT
           END-IF.
           
           END FUNCTION CHECK-BALANCE.