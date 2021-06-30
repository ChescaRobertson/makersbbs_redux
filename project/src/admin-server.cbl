       IDENTIFICATION DIVISION.
       PROGRAM-ID. admin-server.

       ENVIRONMENT DIVISION.

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

            *>----- Date and Time-Screen Variables -----

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   


           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 WS-IDX UNSIGNED-INT.
           01 WS-FOUND PIC 9. 

           01 USER-BANK-ACCOUNT PIC X(8).

           01 RAISE-ERROR PIC 9. 
           01 COUNTER UNSIGNED-INT.
           01 COUNTER2 UNSIGNED-INT.  

      * message variables *    
           01 ERROR-MSG-1 PIC X(50).
           01 ERROR-MSG-2 PIC X(50).
           01 OK-MSG-1 PIC X(50).
           01 OK-MSG-2 PIC X(50).
           01 ADMIN-ERR-MSG PIC X(50).

           01 REGISTER-CHOICE PIC X. 
           01 ADMIN-NAME PIC X(16).
           01 ADMIN-PASSWORD PIC X(20).
           01 ADMIN-ENTER PIC X.
           
           01 NEW-ADMIN-NAME PIC X(16).
           01 NEW-ADMIN-PASSWORD PIC X(20).

           01 WS-ADMINS.
               05 WS-ADMIN OCCURS 10 TIMES
               ASCENDING KEY IS WS-ADMIN-NAME
               INDEXED BY ADMIN-IDX.
                   10 WS-ADMIN-NAME PIC X(16).    
                   10 WS-ADMIN-PWORD PIC X(20).

      *     01 CREDIT-AMOUNT PIC 999.
           01 CAPS-PAID PIC 999.
           01 ADMIN-CHOICE PIC X.
           01 PROCESS-PAGE-CHOICE PIC X.
           01 SINGLE-ENTRY-PROCESS-CHOICE PIC X.
           01 SINGLE-ENTRY-CHOICE PIC X.
           01 PROCESS-STATUS-MESSAGE PIC X(30).
           01 PAYMENT-STATUS-MESSAGE PIC X(30).
           01 BANK-STATEMENT-PROCESS-CHOICE PIC X.

           01 FILE-BA-NUM PIC X(8).

           SCREEN SECTION.
  
           01 CONNECTED-SCREEN.
               05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
                  UNDERLINE, BLINK
                  HIGHLIGHT, FOREGROUND-COLOR 3.

           01 TIME-SCREEN.
               05 LINE 8 COL 117 PIC X(2) USING WS-FORMATTED-HOUR
                   FOREGROUND-COLOR IS 2.
               05 LINE 8 COL 119 VALUE ":"
                   FOREGROUND-COLOR IS 2.
               05 LINE 8 COL 120 PIC X(2) USING WS-FORMATTED-MINS
                   FOREGROUND-COLOR IS 2.

           01 PIP-BOY-SCREEN.        
               05 LINE 5 COL 10 VALUE 
               "========================================================
      -        "========================================================
      -        "=======".
               05 LINE 6 COL 10 VALUE
               "   ______      _________________________________________
      -        "________________________________________________________
      -        "____".
               05 LINE 7 COL 10 VALUE
               "   / |    \      /".
               05 LINE 7 COL 124 VALUE
               "\  ".
               05 LINE 8 COL 10 VALUE 
               "  (  |)-   )  | 0|".
               05 line 8 COL 124 VALUE
               "| |__".
               05 LINE 9 COL 10 VALUE 
               "  /\_|____/   |_ |".
               05 LINE 9 COL 124 VALUE
               "| |0|".
               05 LINE 14 COL 10 VALUE
               "|    _________  ||".
               05 LINE 14 COL 124 VALUE
               "| | |".
               05 LINE 15 COL 10 VALUE 
               "|   |-Pip-Boy-|TT|".
               05 LINE 15 COL 124 VALUE
               "| | |".
               05 LINE 16 COL 10 VALUE
               "|   |-=======-|++|".
               05 LINE 16 COL 124 VALUE
               "| |_|".
               05 LINE 17 COL 10 VALUE
               "|   |-=======-|++|".
               05 LINE 17 COL 124 VALUE
               "| |=|".
               05 LINE 18 COL 10 VALUE
               "|   |- MODEL -|11|".
               05 LINE 18 COL 124 VALUE
               "| |=|".
               05 LINE 19 COL 10 VALUE
               "|   |- 3000  -| ||".
               05 LINE 19 COL 124 VALUE
               "| |=|".
               05 LINE 20 COL 10 VALUE
               "|   '---------' ||".
               05 LINE 20 COL 124 VALUE
               "| |=|".
               05 LINE 10 COL 10 VALUE
               "/ '             ||".
               05 LINE 10 COL 124 VALUE
               "| |=|".
               05 LINE 11 COL 10 VALUE
               "|               ||".
               05 LINE 11 COL 124 VALUE
               "| |=|".
               05 LINE 12 COL 10 VALUE
               "|               ||".
               05 LINE 12 COL 124 VALUE
               "| |=|".
               05 LINE 13 COL 10 VALUE
               "|               ||".
               05 LINE 13 COL 124 VALUE
               "| |=|".
               05 LINE 21 COL 10 VALUE
               "|               ||".
               05 LINE 21 COL 124 VALUE
               "| |=|".
               05 LINE 22 COL 10 VALUE
               "|               ||".
               05 LINE 22 COL 124 VALUE
               "| |=|".
               05 LINE 23 COL 10 VALUE
               "|               ||".
               05 LINE 23 COL 124 VALUE
               "| |=|".
               05 LINE 24 COL 10 VALUE
               "|               ||".
               05 LINE 24 COL 124 VALUE
               "| |=|".
               05 LINE 25 COL 10 VALUE
               "|               ||".
               05 LINE 25 COL 124 VALUE
               "| |=|".
               05 LINE 26 COL 10 VALUE
               "|               ||".
               05 LINE 26 COL 124 VALUE
               "| |=|".
               05 LINE 27 COL 10 VALUE
               "|               ||".
               05 LINE 27 COL 124 VALUE
               "| |=|".
               05 LINE 28 COL 10 VALUE
               "|               ||".
               05 LINE 28 COL 124 VALUE
               "| |=|".
               05 LINE 29 COL 10 VALUE
               "|               ||".
               05 LINE 29 COL 124 VALUE
               "| |=|".
               05 LINE 30 COL 10 VALUE
               "|               ||".
               05 LINE 30 COL 124 VALUE
               "| |=|".
               05 LINE 31 COL 10 VALUE
               "|               ||".
               05 LINE 31 COL 124 VALUE
               "| |=|".
               05 LINE 32 COL 10 VALUE
               "|               ||".
               05 LINE 32 COL 124 VALUE
               "| |=|".
               05 LINE 33 COL 10 VALUE
               "|               ||".
               05 LINE 33 COL 124 VALUE
               "| |=|".
               05 LINE 34 COL 10 VALUE
               "|               ||".
               05 LINE 34 COL 124 VALUE
               "| |=|".
               05 LINE 35 COL 10 VALUE
               "|               ||".
               05 LINE 35 COL 124 VALUE
               "| |=|".
               05 LINE 36 COL 10 VALUE
               "|               ||".
               05 LINE 36 COL 124 VALUE
               "| |=|".
               05 LINE 37 COL 10 VALUE
               "|               ||".
               05 LINE 37 COL 124 VALUE
               "| |=|".
               05 LINE 38 COL 10 VALUE
               "|               ||".
               05 LINE 38 COL 124 VALUE
               "| |=|".
               05 LINE 39 COL 10 VALUE
               "|     _____     ||".
               05 LINE 39 COL 124 VALUE
               "| |=|".
               05 LINE 40 COL 10 VALUE
               "|   .'\ | /'.   ||".
               05 LINE 40 COL 124 VALUE
               "| |=|".
               05 LINE 41 COL 10 VALUE
               "|   | e( )it|   ||".
               05 LINE 41 COL 18 VALUE "x"
                   HIGHLIGHT, FOREGROUND-COLOR 4.
               05 LINE 41 COL 124 VALUE
               "| |=|".
               05 LINE 42 COL 10 VALUE
               "|   './_|_\.'   ||".
               05 LINE 42 COL 124 VALUE
               "| | |".
               05 LINE 43 COL 10 VALUE
               "|               ||".
               05 LINE 43 COL 124 VALUE
               "| | |".
               05 LINE 44 COL 10 VALUE
               "|              _||".
               05 LINE 44 COL 124 VALUE
               "| | |".
               05 LINE 45 COL 10 VALUE
               "\              /0|______________________________________
      -        "________________________________________________________
      -        "__| |0|".          
               05 LINE 46 COL 10 VALUE
               " \            ''|_______________________________________
      -        "________|---TORCH---|___________________________________
      -        "____/".
               05 LINE 47 COL 10 VALUE
               "  \_________/     |===|                                    
      -        "        |    ( )    |                                    
      -        "   /".
               05 LINE 47 COL 80 VALUE "T"
                    HIGHLIGHT, FOREGROUND-COLOR 4.
               05 LINE 48 COL 10 VALUE
               "             \_____|___/________________________________
      -        "________|||||||||||||___________________________________
      -        "__/".
               05 LINE 50 COL 10 VALUE
               "========================================================
      -        "========================================================
      -        "=======". 

   
           01 ADMIN-LOGIN-SCREEN.
               05 BLANK SCREEN. 
               05 LINE 18 COL 43 PIC X(50) USING ADMIN-ERR-MSG 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 LINE 20 COL 43 VALUE "Enter Administrator username:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 ADMIN-NAME-FIELD LINE 22 COL 43 PIC X(16) 
               USING ADMIN-NAME FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 43 VALUE "Enter Administrator password:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 ADMIN-PASSWORD-FIELD LINE 26 COLUMN 43 PIC X(20)
               USING ADMIN-PASSWORD FOREGROUND-COLOR IS 2 .  
               05 LINE 28 COLUMN 43 VALUE "(l) Log-in."
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 29 COLUMN 43 VALUE "(g) Go Back."
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 31 COLUMN 43 VALUE "Pick: " 
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 ADMIN-ENTER-FIELD LINE 31 COLUMN 49 PIC X
               USING ADMIN-ENTER.

           01 ADMIN-MENU-SCREEN.
               05 BLANK SCREEN.
               05 LINE 18 COL 43 VALUE "Welcome, "
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 52 PIC X(16) USING ADMIN-NAME 
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 43 VALUE "Please select from the below opt
      -         "ions." FOREGROUND-COLOR IS 2.
               05 LINE 23 COL 43 VALUE "(p) Process payments "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 43 VALUE "(a) Add Admin        "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 27 COL 43 VALUE "(l) Logout           "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 31 COL 43 VALUE "Pick: " HIGHLIGHT 
               FOREGROUND-COLOR IS 2.
               05 ADMIN-CHOICE-FIELD LINE 31 COL 49 PIC X
               USING ADMIN-CHOICE.

           01 REGISTER-ADMIN-SCREEN.
               05 BLANK SCREEN.
               05 LINE 18 COLUMN 43 VALUE "Enter Overseer Name:"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COLUMN 68 VALUE "(Overseer name must be unique
      -        ")" FOREGROUND-COLOR IS 2.
               05 NEW-ADMIN-NAME-FIELD LINE 20 COLUMN 43 PIC X(16)
               USING NEW-ADMIN-NAME FOREGROUND-COLOR IS 2.
               05 LINE 19 COLUMN 43 PIC X(50) 
               USING ERROR-MSG-1 HIGHLIGHT FOREGROUND-COLOR is 4.
               05 LINE 21 COLUMN 43 PIC X(50) USING OK-MSG-1 HIGHLIGHT
               FOREGROUND-COLOR IS 2.

               05 LINE 23 COLUMN 43 VALUE "Enter a password:"
               FOREGROUND-COLOR IS 2.
               05 LINE 24 COLUMN 43 VALUE "(Your password must be a mini
      -        "mum of 6 characters and include at least 1 number.) "
               FOREGROUND-COLOR IS 2.
               05 NEW-ADMIN-PASSWORD-FIELD LINE 27 COLUMN 43 PIC X(20)
               USING NEW-ADMIN-PASSWORD FOREGROUND-COLOR IS 2.
               05 LINE 28 COLUMN 43 PIC X(50) USING OK-MSG-2 HIGHLIGHT
               FOREGROUND-COLOR IS 2.
               05 LINE 26 COLUMN 43 PIC X(50) 
               USING ERROR-MSG-2 HIGHLIGHT FOREGROUND-COLOR is 4.
               05 LINE 30 COLUMN 43 VALUE "(s) Submit"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 31 COLUMN 43 VALUE "(g) Go Back"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 33 COLUMN 43 VALUE "Pick: "
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 REGISTER-CHOICE-FIELD LINE 33 COLUMN 49 PIC X
               USING REGISTER-CHOICE. 
   
           01 PROCESS-PAYMENT-SCREEN.
               05 BLANK SCREEN.
              05 LINE 18 COL 43 VALUE "Please select from the below opti
      -        "ons: " FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 43 VALUE "(s) Process single payment "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
              05 LINE 23 COL 43 VALUE "(b) Process all payments from ban
      -        "k statment."
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 43 VALUE "(g) Go back         "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 27 COL 43 VALUE "(l) Logout          "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 31 COL 43 VALUE "Pick: "
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 PROCESS-PAYMENT-FIELD LINE 31 COL 49 PIC X 
               USING PROCESS-PAGE-CHOICE.
  
           01 SINGLE-ENTRY-CREDIT-SCREEN.
               05 BLANK SCREEN.
               05 LINE 18 COL 43 VALUE "User Bank Account Number: "
               FOREGROUND-COLOR IS 2.
               05 USER-BA-FIELD LINE 18 COL 69 PIC X(8)
               USING USER-BANK-ACCOUNT.
               05 LINE 20 COL 43 VALUE "Caps paid: "
               FOREGROUND-COLOR IS 2.
               05 CAPS-PAID-FIELD LINE 20 COL 54 PIC 999
               USING CAPS-PAID.
               05 LINE 23 COL 43 VALUE "(s) Submit          "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 43 VALUE "(g) Go back         "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 29 COL 43 VALUE "Pick: "
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 SINGLE-ENTRY-CREDIT-FIELD LINE 29 COL 49 PIC X 
               USING SINGLE-ENTRY-CHOICE.
         
           01 SINGLE-ENTRY-PROCESS-SCREEN.
               05 BLANK SCREEN.
               05 LINE 18 COL 43 PIC X(30) USING PROCESS-STATUS-MESSAGE
               HIGHLIGHT FOREGROUND-COLOR IS 2 BLINK.
               05 LINE 20 COL 43 VALUE "Entered bank account: "
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 65 PIC X(8) USING USER-BANK-ACCOUNT. 
               05 LINE 22 COL 43 VALUE "Credits added: "
               FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 65 PIC X(8) USING CAPS-PAID.
               05 LINE 25 COL 43 VALUE "(g) Go back"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 30 COL 43 VALUE "Pick: " HIGHLIGHT 
               FOREGROUND-COLOR IS 2.
               05 SINGLE-ENTRY-PROCESS-FIELD LINE 30 COL 49 PIC X 
               USING SINGLE-ENTRY-PROCESS-CHOICE.
        
           01 BANK-STATEMENT-PROCESS-SCREEN.
               05 BLANK SCREEN.
               05 LINE 18 COL 43 VALUE "Select yes to process all transa
      -        "ctions" FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 43 PIC X(30) USING PAYMENT-STATUS-MESSAGE
               HIGHLIGHT FOREGROUND-COLOR IS 2 BLINK.
               05 LINE 22 COL 43 VALUE "(y) Yes"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 24 COL 43 VALUE "(g) Go back"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 28 COL 43 VALUE "Pick: "
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 BANK-STATEMENT-PROCESS-FIELD LINE 28 COL 49 PIC X 
               USING BANK-STATEMENT-PROCESS-CHOICE.
               

       PROCEDURE DIVISION.

       0105-ADMIN-LOGIN-PAGE.
           PERFORM 0500-TIME-AND-DATE
           PERFORM 0120-GENERATE-ADMIN-TABLE.
           MOVE SPACES TO ADMIN-ERR-MSG.

           ENTER-ADMINISTRATOR-DETAILS. 
           INITIALIZE ADMIN-NAME.
           INITIALIZE ADMIN-PASSWORD.
           INITIALIZE ADMIN-ENTER.
           DISPLAY ADMIN-LOGIN-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.
     
           ACCEPT ADMIN-NAME-FIELD.
           ACCEPT ADMIN-PASSWORD-FIELD.
           ACCEPT ADMIN-ENTER-FIELD. 
           MOVE 0 TO WS-FOUND.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF ADMIN-NAME = WS-ADMIN-NAME(WS-IDX) AND 
               ADMIN-PASSWORD = WS-ADMIN-PWORD(WS-IDX) THEN
                   MOVE 1 TO WS-FOUND 
               END-IF
               ADD 1 TO WS-IDX 
           END-PERFORM.

           IF ADMIN-ENTER = "l" AND WS-FOUND = 1 THEN
               PERFORM 0110-ADMIN-MENU
           ELSE IF  ADMIN-ENTER = "g" THEN 
               GOBACK
           ELSE 
               MOVE "* Administrator details not recognised *" TO 
               ADMIN-ERR-MSG
               PERFORM ENTER-ADMINISTRATOR-DETAILS
           END-IF. 
           
       0110-ADMIN-MENU.
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE ADMIN-CHOICE.
           DISPLAY ADMIN-MENU-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

           ACCEPT ADMIN-CHOICE-FIELD.
           IF ADMIN-CHOICE = "x" or "X" THEN
             STOP RUN
           ELSE IF ADMIN-CHOICE = "l" or "L" THEN
             GOBACK
           ELSE IF ADMIN-CHOICE = 'p' or 'P'
             PERFORM 0300-PROCESS-PAYMENT
           ELSE IF ADMIN-CHOICE = 'a' or 'A'
             PERFORM 0130-REGISTER-ADMIN
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
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE NEW-ADMIN-NAME. 
           INITIALIZE NEW-ADMIN-PASSWORD.
           INITIALIZE REGISTER-CHOICE.
           DISPLAY REGISTER-ADMIN-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.
           
           ACCEPT NEW-ADMIN-NAME-FIELD.
           MOVE 0 TO RAISE-ERROR.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF NEW-ADMIN-NAME = WS-ADMIN-NAME(WS-IDX) 
                   ADD 1 TO RAISE-ERROR
               END-IF
                   ADD 1 TO WS-IDX
           END-PERFORM.
           IF RAISE-ERROR > 0 
               MOVE 'OVERSEER NAME IN USE' TO ERROR-MSG-1
               PERFORM VALIDATE-USERNAME
           ELSE 
               MOVE 'OVERSEER NAME OK' TO OK-MSG-1
               MOVE SPACES TO ERROR-MSG-1
               PERFORM VALIDATE-PASSWORD
           END-IF. 

           VALIDATE-PASSWORD.
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE NEW-ADMIN-PASSWORD.
           DISPLAY REGISTER-ADMIN-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.
 
           ACCEPT NEW-ADMIN-PASSWORD-FIELD.
           CALL 'validate-password' USING NEW-ADMIN-PASSWORD ERROR-MSG-2 
           RAISE-ERROR OK-MSG-2.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-PASSWORD
           END-IF. 

           DISPLAY REGISTER-ADMIN-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

           ACCEPT REGISTER-CHOICE-FIELD.
           IF REGISTER-CHOICE = "g" THEN 
               PERFORM 0110-ADMIN-MENU
           ELSE IF REGISTER-CHOICE = "s" 
               OPEN EXTEND F-ADMIN-FILE
               MOVE NEW-ADMIN-NAME TO ADMIN
               MOVE NEW-ADMIN-PASSWORD TO ADMIN-PWORD
               WRITE ADMINS
               END-WRITE 
           ELSE 
               PERFORM 0110-ADMIN-MENU
           END-IF.
           CLOSE F-ADMIN-FILE.
           PERFORM 0110-ADMIN-MENU.


       0300-PROCESS-PAYMENT.
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE PROCESS-PAGE-CHOICE
           DISPLAY PROCESS-PAYMENT-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

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
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE SINGLE-ENTRY-CHOICE
           INITIALIZE USER-BANK-ACCOUNT
           INITIALIZE CAPS-PAID
           DISPLAY SINGLE-ENTRY-CREDIT-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

           ACCEPT USER-BA-FIELD
           ACCEPT CAPS-PAID-FIELD
           ACCEPT SINGLE-ENTRY-CREDIT-FIELD

           IF SINGLE-ENTRY-CHOICE = 's' OR 'S'
               PERFORM 0325-SINGLE-ENTRY-PROCESS
           ELSE IF SINGLE-ENTRY-CHOICE = 'g' OR 'G'
               PERFORM 0110-ADMIN-MENU
           ELSE 
               PERFORM 0320-SINGLE-ENTRY-CREDITS
           END-IF.

       0325-SINGLE-ENTRY-PROCESS.
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE SINGLE-ENTRY-PROCESS-CHOICE  
           CALL 'process-single-payment' USING USER-BANK-ACCOUNT, 
           CAPS-PAID, PROCESS-STATUS-MESSAGE, FILE-BA-NUM.
           DISPLAY SINGLE-ENTRY-PROCESS-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

           ACCEPT SINGLE-ENTRY-PROCESS-FIELD
          
           IF SINGLE-ENTRY-PROCESS-CHOICE = 'g' OR 'G'
               PERFORM 0110-ADMIN-MENU
           ELSE 
               PERFORM 0325-SINGLE-ENTRY-PROCESS
           END-IF.
           
       0350-BANK-STATEMENT-PROCESS.
           PERFORM 0500-TIME-AND-DATE
           INITIALIZE BANK-STATEMENT-PROCESS-CHOICE
           DISPLAY BANK-STATEMENT-PROCESS-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-INFO.

           ACCEPT BANK-STATEMENT-PROCESS-FIELD

           IF BANK-STATEMENT-PROCESS-CHOICE = 'y' OR 'Y'
              CALL 'process-bank-statement' USING PAYMENT-STATUS-MESSAGE
               PERFORM 0350-BANK-STATEMENT-PROCESS
           ELSE IF BANK-STATEMENT-PROCESS-CHOICE = 'g' OR 'G'
               PERFORM 0110-ADMIN-MENU
           END-IF.

       0113-DISPLAY-TIME-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.


       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.