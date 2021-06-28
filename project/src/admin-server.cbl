       IDENTIFICATION DIVISION.
       PROGRAM-ID. admin-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION CONV-MON-TO-CRED.
              
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT F-USERS-FILE ASSIGN TO 'users.dat'
                 ORGANIZATION IS SEQUENTIAL. 

             SELECT F-TRANSACTIONS-FILE ASSIGN TO 'transactions.dat'
                  ORGANIZATION IS SEQUENTIAL.

             SELECT F-ADMIN-FILE ASSIGN TO 'admins.dat'
                 ORGANIZATION IS LINE SEQUENTIAL. 

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

           FD F-TRANSACTIONS-FILE.
           01 TRANSACTIONS.
               05 USERNAME PIC X(16).
               05 BANK-ACCOUNT PIC X(8).
               05 FILLER PIC XX VALUE SPACES.
               05 CREDITS-TO-ADD PIC 999.
               05 FILLER PIC XX VALUE SPACES.
               05 MON-AMOUNT PIC 999.99.
               05 FILLER PIC XX VALUE SPACES.
               05 DATE-OF-TRANS PIC X(10).
               05 PAYMENT-STATUS PIC X(20).
               05 FILLER PIC X VALUE X'0A'.

           FD F-ADMIN-FILE.
           01 ADMINS. 
               05 ADMIN PIC X(16).
               05 ADMIN-PWORD PIC X(20).

           WORKING-STORAGE SECTION.

           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 WS-IDX UNSIGNED-INT.

           01 USER-BANK-ACCOUNT PIC X(8).

           01 RAISE-ERROR PIC 9. 
           01 COUNTER UNSIGNED-INT. 
           01 ERROR-MSG-1 PIC X(50).
           01 ERROR-MSG-2 PIC X(50).
           01 OK-MSG-1 PIC X(50).
           01 OK-MSG-2 PIC X(50).
           01 REGISTER-CHOICE PIC X. 
           01 NEW-ADMIN-NAME PIC X(16).
           01 ADMIN-PASSWORD PIC X(20).

           01 WS-ADMINS.
               05 WS-ADMIN OCCURS 10 TIMES
               ASCENDING KEY IS WS-ADMIN-NAME
               INDEXED BY ADMIN-IDX.
                   10 WS-ADMIN-NAME PIC X(16).    
                   10 WS-ADMIN-PWORD PIC X(20).

      *     01 CREDIT-AMOUNT PIC 999.
           01 CAPS-PAID PIC 999.
           01 PAYMENT-STATUS PIC X(8).
           01 ADMIN-CHOICE PIC X.
           01 PROCESS-PAGE-CHOICE PIC X.
           01 SINGLE-ENTRY-PROCESS-CHOICE PIC X.
           01 SINGLE-ENTRY-CHOICE PIC X.
           01 PROCESS-STATUS-MESSAGE PIC X(30).
           01 PAYMENT-STATUS-MESSAGE PIC X(30).
           01 BANK-STATEMENT-PROCESS-CHOICE PIC X.

           01 FILE-BA-NUM PIC X(8).
           
           LINKAGE SECTION.
           01 ADMIN-NAME PIC X(16).

           SCREEN SECTION.

           01 ADMIN-MENU-SCREEN.
      *        BACKGROUND-COLOR IS 0.
              05 BLANK SCREEN.
              *> 05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
              *> 05 LINE 2 COL 4 VALUE ":".
              *> 05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS. 
              05 LINE 4 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK
              HIGHLIGHT, FOREGROUND-COLOR IS 3.
              05 LINE 8 COL 10 VALUE "Welcome, ".
              05 LINE 8 COL 19 PIC X(16) USING ADMIN-NAME.
              05 LINE 10 COL 10 VALUE "Please select from the below opti
      -         "ons.".  
              05 LINE 13 COL 10 VALUE "(p) Process payments "
                   REVERSE-VIDEO HIGHLIGHT.
              05 LINE 13 COL 33 VALUE "(u) Manage Users     "
                   REVERSE-VIDEO, HIGHLIGHT.
              05 LINE 15 COL 10 VALUE "(a) Add Admin        "
                   REVERSE-VIDEO HIGHLIGHT.
              05 LINE 15 COL 33 VALUE "(u) Manage Posts     "
                   REVERSE-VIDEO, HIGHLIGHT.
              05 LINE 17 COL 10 VALUE "(l) Logout           "
                   REVERSE-VIDEO , HIGHLIGHT.             
              05 LINE 17 COL 33 VALUE "(q) Quit             "
                   REVERSE-VIDEO, HIGHLIGHT.  
              05 LINE 21 COL 14 VALUE "Pick: ".
              05 ADMIN-CHOICE-FIELD LINE 21 COL 20 PIC X
                   USING ADMIN-CHOICE.

           01 REGISTER-ADMIN-SCREEN. 
             05 BLANK SCREEN.
             05 LINE 27 COLUMN 12 VALUE "ADD AN OVERSEER" HIGHLIGHT,
             FOREGROUND-COLOR IS 3.
             05 LINE 29 COLUMN 12 VALUE "input intro text explaining the
      -      " BBS and everything you can do. Why we need bank details."  
             FOREGROUND-COLOR IS 5.
             05 LINE 30 COLUMN 12 VALUE "input intro text explaining the
      -      " BBS and everything you can do. Why we need bank details."  
             FOREGROUND-COLOR IS 5.
             05 LINE 31 COLUMN 12 VALUE "input intro text explaining the
      -      " BBS and everything you can do. Why we need bank details."  
             FOREGROUND-COLOR IS 5.
             05 LINE 33 COLUMN 12 VALUE "Enter Overseer Name:".
             05 LINE 33 COLUMN 33 VALUE " (Overseer name must be unique.
      -      ")".
             05 LINE 34 COLUMN 12 PIC X(50) USING ERROR-MSG-1 HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 NEW-ADMIN-NAME-FIELD LINE 35 COLUMN 12 PIC X(16)
                USING NEW-ADMIN-NAME.
             05 LINE 36 COLUMN 12 PIC X(50) USING OK-MSG-1 HIGHLIGHT
             FOREGROUND-COLOR is 2.
             05 LINE 37 COLUMN 12 VALUE "Enter a password:".
             05 LINE 37 COLUMN 30 VALUE " (Your password must be a minim
      -      "um of 6 characters and include at least 1 number.) ".
             05 LINE 38 COLUMN 12 PIC X(50) USING ERROR-MSG-2 HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 ADMIN-PASSWORD-FIELD LINE 39 COLUMN 12 PIC X(20)
                USING ADMIN-PASSWORD.
             05 LINE 40 COLUMN 12 PIC X(50) USING OK-MSG-2 HIGHLIGHT
             FOREGROUND-COLOR is 2.
             05 LINE 41 COLUMN 12 VALUE "Enter a valid Bank Account numb
      -      "er:".
             05 LINE 46 COLUMN 12 VALUE "(s) Submit".
             05 LINE 47 COLUMN 12 VALUE "(q) Go Back".
             05 LINE 49 COLUMN 12 VALUE "Pick: ".
             05 REGISTER-CHOICE-FIELD LINE 49 COLUMN 18 PIC X
                USING REGISTER-CHOICE. 
   
           01 PROCESS-PAYMENT-SCREEN.
             05 BLANK SCREEN.
             05 LINE 10 COL 10 VALUE "Please select from the below optio
      -         "ns: ".  
             05 LINE 13 COL 10 VALUE "(s) Process single payment "
                  REVERSE-VIDEO HIGHLIGHT.
             05 LINE 15 COL 10 VALUE "(b) Process all payments from bank 
      -         "statment."
                  REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 17 COL 10 VALUE "(g) Go back         "
                  REVERSE-VIDEO HIGHLIGHT.
             05 LINE 19 COL 10 VALUE "(l) Logout          "
                  REVERSE-VIDEO , HIGHLIGHT.             
             05 LINE 21 COL 10 VALUE "(q) Quit            "
                  REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 23 COL 14 VALUE "Pick: ".
             05 PROCESS-PAYMENT-FIELD LINE 23 COL 20 PIC X 
                 USING PROCESS-PAGE-CHOICE.
  
           01 SINGLE-ENTRY-CREDIT-SCREEN.
               05 BLANK SCREEN.
               05 LINE 12 COL 10 VALUE "User Bank Account Number: ".
               05 USER-BA-FIELD LINE 12 COL 37 PIC X(8)
                   USING USER-BANK-ACCOUNT. 
               05 LINE 14 COL 10 VALUE "Caps paid: ".
               05 CAPS-PAID-FIELD LINE 14 COL 21 PIC 999
                   USING CAPS-PAID. 
               05 LINE 17 COL 10 VALUE "(s) Submit          "
                    REVERSE-VIDEO HIGHLIGHT.
               05 LINE 19 COL 10 VALUE "(g) Go back         "
                    REVERSE-VIDEO , HIGHLIGHT.             
               05 LINE 21 COL 10 VALUE "(q) Quit            "
                    REVERSE-VIDEO, HIGHLIGHT.  
               05 LINE 23 COL 14 VALUE "Pick: ".
               05 SINGLE-ENTRY-CREDIT-FIELD LINE 23 COL 20 PIC X 
                   USING SINGLE-ENTRY-CHOICE.
         
           01 SINGLE-ENTRY-PROCESS-SCREEN.
            05 BLANK SCREEN.
            05 LINE 12 COL 10 PIC X(30) USING PROCESS-STATUS-MESSAGE.
            05 LINE 13 COL 10 VALUE "User bank account from file: ".
            05 LINE 14 COL 10 PIC X(8) USING FILE-BA-NUM.
            05 LINE 15 COL 10 VALUE "Entered bank account: ".
            05 LINE 16 COL 10 PIC X(8) USING USER-BANK-ACCOUNT.
            *> 05 LINE 16 COL 10 VALUE "(g) Go back         "
            *>      REVERSE-VIDEO , HIGHLIGHT.             
            *> 05 LINE 17 COL 32 VALUE "(q) Quit            "
            *>      REVERSE-VIDEO, HIGHLIGHT.  
            05 LINE 21 COL 14 VALUE "Pick: ".
            05 SINGLE-ENTRY-PROCESS-FIELD LINE 21 COL 20 PIC X 
                USING SINGLE-ENTRY-PROCESS-CHOICE.
        
 
           01 BANK-STATEMENT-PROCESS-SCREEN.
            05 BLANK SCREEN.
            05 LINE 10 COL 10 VALUE "Select yes to process all transacti
      -    "ons".
            05 LINE 12 COL 10 PIC X(30) USING PAYMENT-STATUS-MESSAGE.
            05 LINE 14 COL 10 VALUE "(y) Yes             ".
            05 LINE 16 COL 10 VALUE "(g) Go back         "
                 REVERSE-VIDEO , HIGHLIGHT.             
            05 LINE 17 COL 10 VALUE "(q) Quit            "
                 REVERSE-VIDEO, HIGHLIGHT.  
            05 LINE 21 COL 14 VALUE "Pick: ".
            05 BANK-STATEMENT-PROCESS-FIELD LINE 21 COL 20 PIC X 
               USING BANK-STATEMENT-PROCESS-CHOICE.
               

       PROCEDURE DIVISION USING ADMIN-NAME.
           
       0110-ADMIN-MENU.
      *     PERFORM 0200-TIME-AND-DATE.
           INITIALIZE ADMIN-CHOICE.
           DISPLAY ADMIN-MENU-SCREEN.
           ACCEPT ADMIN-CHOICE-FIELD.
           IF ADMIN-CHOICE = "q" or "Q" THEN
             STOP RUN
           ELSE IF ADMIN-CHOICE = "l" or "L" THEN
           CALL 'server'
      *    Think about how to return to main server initial page here *
           ELSE IF ADMIN-CHOICE = 'p' or 'P'
             PERFORM 0300-PROCESS-PAYMENT
           ELSE IF ADMIN-CHOICE = 'a' or 'A'
             PERFORM 0130-REGISTER-ADMIN
      *     Add other menu options for administrator here *
           ELSE 
             PERFORM 0110-ADMIN-MENU
           END-IF.

       0120-GENERATE-ADMIN-TABLE. 
           SET COUNTER TO 0.
           OPEN INPUT F-ADMIN-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-ADMIN-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE ADMIN TO WS-ADMIN-NAME(COUNTER)
                       MOVE ADMIN-PWORD TO WS-ADMIN-PWORD(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-ADMIN-FILE.

       0130-REGISTER-ADMIN SECTION. 

           MOVE SPACES TO ERROR-MSG-1.
           MOVE SPACES TO ERROR-MSG-2.
           MOVE SPACES TO OK-MSG-1.
           MOVE SPACES TO OK-MSG-2.
           
           VALIDATE-USERNAME.
           INITIALIZE NEW-ADMIN-NAME. 
           INITIALIZE ADMIN-PASSWORD.
           INITIALIZE REGISTER-CHOICE.
           DISPLAY REGISTER-ADMIN-SCREEN.
           ACCEPT NEW-ADMIN-NAME-FIELD.
           MOVE 0 TO RAISE-ERROR.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF ADMIN-NAME = WS-ADMIN-NAME(ADMIN-IDX) 
                   ADD 1 TO RAISE-ERROR
               END-IF
                   ADD 1 TO WS-IDX
           END-PERFORM.
           IF RAISE-ERROR > 0 
               MOVE 'OVERSEER NAME IN USE' TO ERROR-MSG-1
               PERFORM VALIDATE-USERNAME
           ELSE 
               MOVE 'OVERSEER NAME OK' TO OK-MSG-1
               PERFORM VALIDATE-PASSWORD
           END-IF. 

           VALIDATE-PASSWORD.
           INITIALIZE ADMIN-PASSWORD.
           DISPLAY REGISTER-ADMIN-SCREEN.
      *    DISPLAY TIME-SCREEN.
           ACCEPT ADMIN-PASSWORD-FIELD.
           CALL 'validate-password' USING ADMIN-PASSWORD ERROR-MSG-2 
           RAISE-ERROR OK-MSG-2.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-PASSWORD
           END-IF. 

           DISPLAY REGISTER-ADMIN-SCREEN.
      *    DISPLAY TIME-SCREEN.
           ACCEPT REGISTER-CHOICE-FIELD.
           IF REGISTER-CHOICE = "q" THEN 
               PERFORM 0110-ADMIN-MENU
           ELSE IF REGISTER-CHOICE = "s" 
               OPEN EXTEND F-ADMIN-FILE
               MOVE NEW-ADMIN-NAME TO ADMIN
               MOVE ADMIN-PASSWORD TO ADMIN-PWORD
               WRITE ADMINS
               END-WRITE 
           END-IF.
           CLOSE F-ADMIN-FILE.
           PERFORM 0110-ADMIN-MENU.


       0300-PROCESS-PAYMENT.
      *     PERFORM 0200-TIME-AND-DATE.
           INITIALIZE PROCESS-PAGE-CHOICE
           DISPLAY PROCESS-PAYMENT-SCREEN
           ACCEPT PROCESS-PAYMENT-FIELD
           IF PROCESS-PAGE-CHOICE = 's' OR 'S'
             PERFORM 0320-SINGLE-ENTRY-CREDITS
           ELSE IF PROCESS-PAGE-CHOICE = 'b' OR 'B'
             PERFORM 0350-BANK-STATEMENT-PROCESS
           ELSE IF PROCESS-PAGE-CHOICE = 'g' OR 'G'
             PERFORM 0110-ADMIN-MENU
           ELSE 
               PERFORM 0300-PROCESS-PAYMENT
           END-IF.

       0320-SINGLE-ENTRY-CREDITS.
      *     PERFORM 0200-TIME-AND-DATE.
           INITIALIZE SINGLE-ENTRY-CHOICE
           INITIALIZE USER-BANK-ACCOUNT
           INITIALIZE CAPS-PAID
           DISPLAY SINGLE-ENTRY-CREDIT-SCREEN
           ACCEPT USER-BA-FIELD
           ACCEPT CAPS-PAID-FIELD
           ACCEPT SINGLE-ENTRY-CREDIT-FIELD

           IF SINGLE-ENTRY-CHOICE = 's' OR 'S'
             PERFORM 0325-SINGLE-ENTRY-PROCESS
           ELSE IF SINGLE-ENTRY-CHOICE = 'g' OR 'G'
             PERFORM 0110-ADMIN-MENU
           ELSE IF SINGLE-ENTRY-CHOICE = 'q' OR 'Q' THEN
               STOP RUN
           ELSE 
               PERFORM 0320-SINGLE-ENTRY-CREDITS
           END-IF.

       0325-SINGLE-ENTRY-PROCESS.
      *     PERFORM 0200-TIME-AND-DATE.
           INITIALIZE SINGLE-ENTRY-PROCESS-CHOICE
          
      *     MOVE CONV-MON-TO-CRED(MON-AMOUNT-PAID) TO CREDIT-AMOUNT
           CALL 'process-single-payment' USING USER-BANK-ACCOUNT, 
           CAPS-PAID, PROCESS-STATUS-MESSAGE, FILE-BA-NUM.

        
           DISPLAY SINGLE-ENTRY-PROCESS-SCREEN
           ACCEPT SINGLE-ENTRY-PROCESS-FIELD
          
           IF SINGLE-ENTRY-PROCESS-CHOICE = 'g' OR 'G'
             PERFORM 0110-ADMIN-MENU
           ELSE IF SINGLE-ENTRY-PROCESS-CHOICE = 'q' OR 'Q' THEN
               STOP RUN
           ELSE 
               PERFORM 0325-SINGLE-ENTRY-PROCESS
           END-IF.
           
       0350-BANK-STATEMENT-PROCESS.
           INITIALIZE BANK-STATEMENT-PROCESS-CHOICE
           DISPLAY BANK-STATEMENT-PROCESS-SCREEN
           ACCEPT BANK-STATEMENT-PROCESS-FIELD

           IF BANK-STATEMENT-PROCESS-CHOICE = 'y' OR 'Y'
              CALL 'process-bank-statement' USING PAYMENT-STATUS-MESSAGE
               PERFORM 0350-BANK-STATEMENT-PROCESS
           ELSE IF BANK-STATEMENT-PROCESS-CHOICE = 'g' OR 'G'
               PERFORM 0110-ADMIN-MENU
           ELSE IF BANK-STATEMENT-PROCESS-CHOICE = 'q' OR 'Q' THEN
               STOP RUN
           END-IF.


