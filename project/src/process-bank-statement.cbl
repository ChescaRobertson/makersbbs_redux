       IDENTIFICATION DIVISION.
           PROGRAM-ID. process-bank-statement.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-TRANSACTIONS-FILE ASSIGN TO "transactions.dat"
             ORGANISATION IS SEQUENTIAL.
           SELECT F-USERS-FILE ASSIGN TO "users.dat"
             ORGANISATION IS SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(10).  
              05 USER-CREDIT PIC 999. 
              05 FILLER PIC X VALUE X'0A'.

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
               05 FILLER PIC X VALUE X'0A'.


           WORKING-STORAGE SECTION.
           01 WS-TRANS-FILE-IS-ENDED PIC 9 VALUE 0.
           01 WS-USER-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-PAYMENT-STATUS PIC X(30).

       PROCEDURE DIVISION USING LS-PAYMENT-STATUS.

           OPEN I-O F-TRANSACTIONS-FILE.
           
               
           PERFORM UNTIL WS-TRANS-FILE-IS-ENDED = 1
               READ F-TRANSACTIONS-FILE 
                   NOT AT END
                   IF PAYMENT-STATUS = "PENDING"
                    MOVE 0 TO WS-USER-FILE-IS-ENDED
                    OPEN I-O F-USERS-FILE
                    PERFORM UNTIL WS-USER-FILE-IS-ENDED = 1
                         READ F-USERS-FILE 
                         NOT AT END   
                             IF BANK-ACCOUNT = USER-ACNT-NUM
                                 ADD CREDITS-TO-ADD TO USER-CREDIT
                                 MOVE "PAID" TO PAYMENT-STATUS
                                 REWRITE USERS FROM USERS
                                 REWRITE TRANSACTIONS FROM TRANSACTIONS
                             END-IF
                         AT END 
                             MOVE 1 TO WS-USER-FILE-IS-ENDED
                         END-READ
                     END-PERFORM
                     CLOSE F-USERS-FILE
                  
                    AT END 
                        MOVE 1 TO WS-TRANS-FILE-IS-ENDED
               END-READ
           END-PERFORM.

           MOVE "ALL PAYMENTS UP TO DATE" TO LS-PAYMENT-STATUS.

           CLOSE F-TRANSACTIONS-FILE.
  
     