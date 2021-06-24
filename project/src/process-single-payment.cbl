       IDENTIFICATION DIVISION.
           PROGRAM-ID. process-single-payment.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
       
           SELECT F-USERS-FILE ASSIGN TO "users.dat"
             ORGANISATION IS SEQUENTIAL.
             
       DATA DIVISION.
           FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(8). 
              05 FILLER PIC XX VALUE SPACES. 
              05 USER-CREDIT PIC 999. 
              05 FILLER PIC X VALUE X'0A'.

           WORKING-STORAGE SECTION.
           01 WS-USER-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USER-BANK-ACCOUNT PIC X(8).
           01 LS-CREDIT-AMOUNT PIC 999.
     
       PROCEDURE DIVISION USING LS-USER-BANK-ACCOUNT, LS-CREDIT-AMOUNT.

           OPEN I-O F-USERS-FILE.

           PERFORM UNTIL WS-USER-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                  NOT AT END
                    IF LS-USER-BANK-ACCOUNT = USER-ACNT-NUM
                       ADD LS-CREDIT-AMOUNT TO USER-CREDIT
                       REWRITE USERS FROM USERS
                     END-IF
                  AT END 
                   MOVE 1 TO WS-USER-FILE-IS-ENDED
               END-READ
           END-PERFORM.
           
           CLOSE F-USERS-FILE.
