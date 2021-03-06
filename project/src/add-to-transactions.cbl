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
               05 BANK-ACCOUNT PIC X(8).
               05 FILLER PIC XX VALUE SPACES.
               05 CREDITS-TO-ADD PIC 999.
               05 FILLER PIC XX VALUE SPACES.
               05 DATE-OF-TRANS PIC X(8).
               05 FILLER PIC XX VALUE SPACES.
               05 PAYMENT-STATUS PIC X.

           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-CREDITS PIC 999.
           01 LS-ACNT-NUM PIC X(8).
           01 LS-CURRENT-DATE PIC X(8).

       PROCEDURE DIVISION USING LS-USERNAME, LS-ACNT-NUM, LS-CREDITS,
        LS-CURRENT-DATE.

           OPEN EXTEND F-TRANSACTIONS-FILE.
               MOVE LS-USERNAME TO USERNAME
               MOVE LS-ACNT-NUM TO BANK-ACCOUNT
               MOVE LS-CREDITS TO CREDITS-TO-ADD
               MOVE LS-CURRENT-DATE TO DATE-OF-TRANS 
               MOVE "P" TO PAYMENT-STATUS

               WRITE TRANSACTIONS
               END-WRITE.
           
           CLOSE F-TRANSACTIONS-FILE.
           