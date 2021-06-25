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
               05 BANK-ACCOUNT PIC X(10).
               05 CREDITS-TO-ADD PIC 999.
               05 GAP1 PIC X(10).
               05 MON-AMOUNT PIC 999.99.
               05 GAP2 PIC X(10).
               05 DATE-OF-TRANS PIC X(10).
               05 PAYMENT-STATUS PIC X(20).
               05 FILLER PIC X VALUE X'0A'.

           FD F-ADMIN-FILE.
           01 ADMINS. 
               05 ADMIN PIC X(16).
               05 ADMIN-PWORD PIC X(20).

           WORKING-STORAGE SECTION.
           01 USER-BANK-ACCOUNT PIC X(8).
           01 MON-AMOUNT-PAID PIC 999.99.
           01 CREDIT-AMOUNT PIC 999.
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
              05 LINE 15 COL 10 VALUE "(s) Add Admin        "
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
               05 LINE 14 COL 10 VALUE "Amount paid: £ ".
               05 MON-PAID-FIELD LINE 14 COL 25 PIC 999.99
                   USING MON-AMOUNT-PAID. 
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
          *>  ELSE IF ADMIN-CHOICE = "l" or "L" THEN
          *>    PERFORM 0100-DISPLAY-START
      *    Think about how to return to main server initial page here *
           ELSE IF ADMIN-CHOICE = 'p' or 'P'
             PERFORM 0300-PROCESS-PAYMENT
      *     Add other menu options for administrator here *
           ELSE 
             PERFORM 0110-ADMIN-MENU
           END-IF.


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
           INITIALIZE MON-AMOUNT-PAID
           DISPLAY SINGLE-ENTRY-CREDIT-SCREEN
           ACCEPT USER-BA-FIELD
           ACCEPT MON-PAID-FIELD
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
          
           MOVE CONV-MON-TO-CRED(MON-AMOUNT-PAID) TO CREDIT-AMOUNT
           CALL 'process-single-payment' USING USER-BANK-ACCOUNT, 
           CREDIT-AMOUNT, PROCESS-STATUS-MESSAGE, FILE-BA-NUM.
        
           DISPLAY SINGLE-ENTRY-PROCESS-SCREEN
           ACCEPT SINGLE-ENTRY-PROCESS-FIELD
          
           IF SINGLE-ENTRY-PROCESS-CHOICE = 'g' OR 'G'
             PERFORM 0320-SINGLE-ENTRY-CREDITS
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
               PERFORM 0300-PROCESS-PAYMENT
           ELSE IF BANK-STATEMENT-PROCESS-CHOICE = 'q' OR 'Q' THEN
               STOP RUN
           END-IF.


