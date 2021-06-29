       IDENTIFICATION DIVISION.
       PROGRAM-ID. buy-credits-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.

            FUNCTION VERIFY-PASSWORD
            FUNCTION CHECK-LIMIT.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

             SELECT F-USERS-FILE ASSIGN TO 'users.dat'
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
                
           WORKING-STORAGE SECTION.

           01 COST PIC 999.
           01 UPDATED-BALANCE PIC 999.
           01 INSUFFICIENT-FUNDS PIC X(20).
           01 USER-INFO-LOGGED-IN PIC X(15) VALUE "Logged in as:".
           01 COUNTER UNSIGNED-INT.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   
           
         *>----- Buy Credits Variables ----- 
           01 CREDIT-AMOUNT PIC 999.
           01 MON-AMOUNT PIC 999.99.
           01 BUY-CREDITS-CHOICE PIC X.
           01 CONFIRM-CHOICE PIC X.
           01 PAY-CONFIRMATION-CHOICE PIC X.
           01 PASSWORD-ENTRY PIC X(20).
           01 INC-PASSWORD PIC X(20).
           01 CREDIT-LIMIT-MESSAGE PIC X(65).
           01 WS-CURRENT-DATE PIC X(8).
      
           LINKAGE SECTION.
           01 USER-INFO-NAME PIC X(16).
           01 USER-INFO-CRED-DISPLAY.
               05 USER-INFO-CR-MESSAGE PIC X(9) VALUE "Credits: ".
               05 USER-INFO-CREDITS PIC 999.

           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-USER-NAME
               INDEXED BY USER-IDX.
                   10 WS-USER-NAME PIC X(16).    
                   10 WS-PWORD PIC X(20).
                   10 WS-ACNT-NUM PIC X(8).
                   10 WS-CREDIT PIC 999. 

           01 USER-NAME PIC X(16).
           01 WS-PASSWORD PIC X(20).
           01 ACCOUNT-NUM PIC X(8).

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

           01 USER-INFO-SCREEN.
               05 LINE 10 COL 30 PIC X(15) USING USER-INFO-LOGGED-IN
                   FOREGROUND-COLOR IS 2.
               05 LINE 11 COL 30 PIC X(16) USING USER-INFO-NAME 
                   HIGHLIGHT, FOREGROUND-COLOR IS 5.
               05 LINE 13 COL 30 PIC X(12) USING 
               USER-INFO-CRED-DISPLAY
                   FOREGROUND-COLOR IS 2.
           
           01 PIP-BOY-SCREEN.
                     
               05 LINE 5 COL 10 VALUE 
           "============================================================
      -    "==========================================================="
           .
               05 LINE 6 COL 10 VALUE
           "   ______      _____________________________________________
      -    "________________________________________________________   "
           .
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
           "|   |-e(x)it|   ||".
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
           "\              /0|_________________________________________
      -    "______________________________________________________| |0|"
           .
                 05 LINE 46 COL 10 VALUE
           " \            ''|___________________________________________
      -    "____|SUBMIT  QUIT|______________________________________/".
                 05 LINE 47 COL 10 VALUE
           "   \_________/     |===|                                    
      -    "    | (s)     (q)|                                    /".
                 05 LINE 48 COL 10 VALUE
           "             \_____|___/____________________________________
      -    "____||||||||||||||___________________________________/".
                 05 LINE 50 COL 10 VALUE
           "============================================================
      -    "==========================================================="
           . 

           01 BUY-CREDITS-SCREEN.
             05 BLANK SCREEN.
             05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
               FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 34 VALUE 
               "Please enter the amount of credits you would like to purchas
      -        "e: " FOREGROUND-COLOR IS 2.   
             05 CREDIT-FIELD LINE 29 COLUMN 34 PIC 999 
             USING CREDIT-AMOUNT.
             05 LINE 31 COL 34 VALUE "(s) Submit "
                    HIGHLIGHT FOREGROUND-COLOR IS 3. 
             05 LINE 32 COL 45 VALUE "(g) Go back"
                    HIGHLIGHT FOREGROUND-COLOR IS 3.            
             05 LINE 32 COL 34 VALUE "(q) Quit   "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.  
             05 LINE 34 COL 34 VALUE "Pick: " 
               FOREGROUND-COLOR 2 HIGHLIGHT.
             05 BUY-CREDITS-CHOICE-FIELD LINE 34 COL 40 PIC X 
                   USING BUY-CREDITS-CHOICE.
    
             05 LINE 36 COL 34 PIC X(50) USING CREDIT-LIMIT-MESSAGE
               HIGHLIGHT, FOREGROUND-COLOR IS 3.

           01 CONFIRM-SCREEN.
             05 BLANK SCREEN.
             05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 34 VALUE "Credits will cost:"
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 52 PIC 999 USING CREDIT-AMOUNT
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 56 VALUE "bottle caps" 
             FOREGROUND-COLOR IS 2.
             05 LINE 29 COL 34 VALUE 
            "Please enter your password to confirm payment."
             FOREGROUND-COLOR IS 2.
             05 LINE 30 COL 34 VALUE "Password: "
             FOREGROUND-COLOR IS 2.
             05 BUY-PASSWORD-FIELD LINE 30 COL 34 PIC X(20) 
                USING PASSWORD-ENTRY
             FOREGROUND-COLOR IS 2.
             05 LINE 32 COL 34 PIC X(20) USING INC-PASSWORD 
                 HIGHLIGHT, FOREGROUND-COLOR IS 4.
             05 LINE 36 COL 34 VALUE "(s) Submit "
                 HIGHLIGHT, FOREGROUND-COLOR IS 2.
             05 LINE 37 COL 34 VALUE "(g) Go back"
                 HIGHLIGHT, FOREGROUND-COLOR IS 2.
             05 LINE 38 COL 34 VALUE "(q) Quit   "
                 HIGHLIGHT, FOREGROUND-COLOR IS 2.
             05 LINE 39 COL 34 VALUE "Pick: "
               FOREGROUND-COLOR IS 2.
             05 CONFIRM-CHOICE-FIELD LINE 39 COL 40 PIC X 
                USING CONFIRM-CHOICE.
          

           01 PAYMENT-PROCESS-SCREEN.
             05 BLANK SCREEN.
             05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 34 VALUE "Processing payment of caps: "
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 62 PIC 999 USING CREDIT-AMOUNT
             FOREGROUND-COLOR IS 2.
             05 LINE 31 COL 34 VALUE "Confirming payment"
             FOREGROUND-COLOR IS 2.
             05 LINE 35 COL 34 VALUE "This page will redirect in a few s
      -        "econds" FOREGROUND-COLOR IS 2.

           01 PAY-CONFIRMATION-SCREEN.
             05 BLANK SCREEN.
             05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
             FOREGROUND-COLOR 2.
             05 LINE 28 COL 34 VALUE "Thank you for your purchase "
             FOREGROUND-COLOR 2.
             05 LINE 30 COL 34 VALUE "Your transaction is pending"
             FOREGROUND-COLOR 2.
             05 LINE 31 COL 34 PIC 999 USING CREDIT-AMOUNT
             FOREGROUND-COLOR IS 2.
             05 LINE 31 COL 38 VALUE "credits will be added to your acco
      -       "unt within 24 hours" FOREGROUND-COLOR 2.
             05 LINE 36 COL 61 VALUE "(g) Go back"
             FOREGROUND-COLOR 2, HIGHLIGHT.
             05 LINE 36 COL 75 VALUE "(q) Quit   "
             FOREGROUND-COLOR 2, HIGHLIGHT.  
             05 LINE 37 COL 47 VALUE "Pick: "
             FOREGROUND-COLOR 2.
             05 PAY-CONFIRMATION-FIELD LINE 37 COL 54 PIC X 
               USING PAY-CONFIRMATION-CHOICE. 
       
               
       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY,
       WS-USERS, USER-NAME, WS-PASSWORD, ACCOUNT-NUM.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0400-BUY-CREDITS.
           INITIALIZE CREDIT-AMOUNT.
           INITIALIZE BUY-CREDITS-CHOICE.
           DISPLAY BUY-CREDITS-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT CREDIT-FIELD.
           ACCEPT BUY-CREDITS-CHOICE-FIELD.
           IF (BUY-CREDITS-CHOICE = 's'or 'S') 
           AND (CHECK-LIMIT(CREDIT-AMOUNT, USER-INFO-CREDITS) = "PASS")
              PERFORM 0450-CONFIRM
           ELSE IF (BUY-CREDITS-CHOICE = 's'or 'S') 
           AND (CHECK-LIMIT(CREDIT-AMOUNT, USER-INFO-CREDITS) = "FAIL")
               MOVE "CREDITS EXCEEDING MAX AMOUNT, TRANSACTION ABORTED"
               TO CREDIT-LIMIT-MESSAGE
               PERFORM 0400-BUY-CREDITS
           ELSE IF BUY-CREDITS-CHOICE = 'g' OR 'G'
               GOBACK
           ELSE IF BUY-CREDITS-CHOICE = 'q' OR 'Q' THEN
              STOP RUN  
           ELSE
              PERFORM 0400-BUY-CREDITS
           END-IF.
              
       0450-CONFIRM.
           INITIALIZE CONFIRM-CHOICE
           INITIALIZE PASSWORD-ENTRY
           DISPLAY CONFIRM-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT BUY-PASSWORD-FIELD
           ACCEPT CONFIRM-CHOICE-FIELD
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           
            SEARCH WS-USER
                WHEN WS-USER-NAME(USER-IDX) = USER-NAME
                    MOVE WS-ACNT-NUM(USER-IDX) TO ACCOUNT-NUM
            END-SEARCH

           IF CONFIRM-CHOICE = ('s' OR 'S') AND 
                VERIFY-PASSWORD(WS-PASSWORD, PASSWORD-ENTRY) = 'TRUE' 
               CALL 'add-to-transactions' USING USER-NAME, 
                ACCOUNT-NUM, CREDIT-AMOUNT, WS-CURRENT-DATE
               PERFORM 0460-PAYMENT-PROCESS
           ELSE IF CONFIRM-CHOICE = ('s' OR 'S') 
             AND VERIFY-PASSWORD(WS-PASSWORD, PASSWORD-ENTRY) = 'FALSE'
             MOVE "INCORRECT PASSWORD" TO INC-PASSWORD
             PERFORM 0450-CONFIRM
           ELSE IF CONFIRM-CHOICE = 'g' OR 'G'
               PERFORM 0400-BUY-CREDITS
           ELSE IF BUY-CREDITS-CHOICE = 'q' OR 'Q' THEN
              STOP RUN 
           ELSE
               PERFORM 0450-CONFIRM
           END-IF.

       0460-PAYMENT-PROCESS.
           INITIALIZE PAY-CONFIRMATION-CHOICE
           DISPLAY PAYMENT-PROCESS-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           CALL "CBL_GC_NANOSLEEP" USING 5000000000
           DISPLAY PAY-CONFIRMATION-SCREEN
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT PAY-CONFIRMATION-FIELD
           IF PAY-CONFIRMATION-CHOICE = 'g' OR 'G'
             GOBACK
           ELSE IF PAY-CONFIRMATION-CHOICE = 'q' OR 'Q' then
               STOP RUN 
           ELSE 
               DISPLAY PAY-CONFIRMATION-SCREEN
               PERFORM 0113-DISPLAY-TIME-USER-INFO
           END-IF.

           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           