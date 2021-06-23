       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-process-payment.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CONV-MON-TO-CRED.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
         

       PROCEDURE DIVISION.
           
  
       TEST-CONV-MON-TO-CRED.
           CALL "assert-equals" USING CONV-MON-TO-CRED("030.00")'300'.

    