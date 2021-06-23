       IDENTIFICATION DIVISION.
           PROGRAM-ID. process-bank-statement-payment.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-TRANSACTIONS-FILE ASSIGN TO "transactions.dat".
      *       ORGANISATION IS LINE SEQUENTIAL.
           SELECT F-USERS-FILE ASSIGN TO "users.dat".
      *       ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(10).  
              05 USER-CREDIT PIC 999. 


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
           01 WS-TRANS-FILE-IS-ENDED PIC 9 VALUE 0.
           01 WS-USER-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USER-BANK-ACCOUNT PIC X(8).
           01 LS-CREDIT-AMOUNT PIC 999.
           01 LS-PAYMENT-STATUS PIC X(20).

       PROCEDURE DIVISION USING LS-USER-BANK-ACCOUNT, LS-CREDIT-AMOUNT, 
       LS-PAYMENT-STATUS.

           OPEN I-O F-TRANSACTIONS-FILE.
           OPEN I-O F-USERS-FILE.
               
           PERFORM UNTIL WS-TRANS-FILE-IS-ENDED = 1
               READ F-TRANSACTIONS-FILE 
               NOT AT END   
                   IF LS-USER-BANK-ACCOUNT = BANK-ACCOUNT
                       PERFORM UNTIL WS-USER-FILE-IS-ENDED = 1
                           READ F-USERS-FILE
                           NOT AT END
                               IF LS-USER-BANK-ACCOUNT = USER-ACNT-NUM
                                   ADD LS-CREDIT-AMOUNT TO USER-CREDIT
                               END-IF 
                           AT END MOVE 1 TO WS-USER-FILE-IS-ENDED
                           END-PERFORM
                        MOVE "PAID" TO PAYMENT-STATUS
                   END-IF
               AT END MOVE 1 TO WS-TRANS-FILE-IS-ENDED
           END-PERFORM.

           CLOSE F-TRANSACTIONS-FILE.
           CLOSE F-USERS-FILE.
