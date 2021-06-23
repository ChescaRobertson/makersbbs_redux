       IDENTIFICATION DIVISION.
           PROGRAM-ID. add-to-transactions.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-TRANSACTIONS-FILE ASSIGN TO "transactions.dat"
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-TRANSACTIONS-FILE.
           01 TRANSACTIONS.
               05 USERNAME PIC X(16).
               05 BANK-ACCOUNT PIC X(10).
               05 CREDITS-TO-ADD PIC 999.
               05 GAP1 PIC X(10).
               05 MON-AMOUNT PIC 999.99.
               05 GAP2 PIC X(10).
               05 DATE-OF-TRANS PIC X(10).
               05 PAYMENT-STATUS PIC X(20).

           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-CREDITS PIC 999.
           01 LS-MON-AMOUNT PIC 999.99.
           01 LS-BANK-ACCOUNT PIC X(10).

       PROCEDURE DIVISION USING LS-USERNAME, LS-BANK-ACCOUNT, LS-CREDITS
       LS-MON-AMOUNT.

           OPEN EXTEND F-TRANSACTIONS-FILE.
               MOVE LS-USERNAME TO USERNAME.
               MOVE LS-BANK-ACCOUNT TO BANK-ACCOUNT.
               MOVE LS-CREDITS TO CREDITS-TO-ADD.
               MOVE "         " TO GAP1.
               MOVE LS-MON-AMOUNT TO MON-AMOUNT.
               MOVE "         " TO GAP2.
               MOVE FUNCTION CURRENT-DATE(1:8) TO DATE-OF-TRANS 
               MOVE "PENDING" TO PAYMENT-STATUS

               WRITE TRANSACTIONS
               END-WRITE.
           
           CLOSE F-TRANSACTIONS-FILE.
           