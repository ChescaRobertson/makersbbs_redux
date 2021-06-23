       IDENTIFICATION DIVISION.
           FUNCTION-ID. CONV-CRED-TO-MON.
       DATA DIVISION.
           LINKAGE SECTION.
           01 CREDIT-AMOUNT PIC 999.
           01 MON-AMOUNT PIC 999.99.

       PROCEDURE DIVISION USING CREDIT-AMOUNT RETURNING MON-AMOUNT.
           
           COMPUTE MON-AMOUNT = CREDIT-AMOUNT * 0.1.

           END FUNCTION CONV-CRED-TO-MON. 
