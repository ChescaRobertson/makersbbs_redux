       IDENTIFICATION DIVISION.
       PROGRAM-ID. msg-board-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
           FUNCTION CHECK-BALANCE.

       DATA DIVISION.
           WORKING-STORAGE SECTION.

           01 COST PIC 999.
           01 UPDATED-BALANCE PIC 999.
           01 INSUFFICIENT-FUNDS PIC X(20).
           01 USER-INFO-CR-MESSAGE PIC X(9) VALUE "Credits: ".
           01 USER-INFO-LOGGED-IN PIC X(15) VALUE "Logged in as:".

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   

           
            *>----- Message Board Variables -----   
           01 MSG-MENU-CHOICE PIC XXX.
           01 NUM-FILE-LINES PIC 999.
           01 ID-NUM PIC 999 VALUE 1.
           01 WS-LIST-TABLE.
               05 WS-LIST-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON 
                 NUM-FILE-LINES.
                   10 LIST-ID PIC 999.
                   10 LIST-TITLE PIC X(50).
                   10 LIST-CONTENT PIC X(300).
                   10 LIST-USERNAME PIC X(16).        
           01 WS-CONTENT-DISPLAY.
               05 LS-PART-1 PIC X(60).
               05 LS-PART-2 PIC X(60).
               05 LS-PART-3 PIC X(60).
               05 LS-PART-4 PIC X(60).
               05 LS-PART-5 PIC X(60).
           01 MSG-SELECT PIC 999.
           01 MSG-VIEW-CHOICE PIC X.
           
           01 NEW-MESSAGE.
             05 WS-TITLE PIC X(50).
             05 WS-CONTENT PIC X(300).
             05 WS-USERNAME PIC X(16).

           LINKAGE SECTION.
           01 USER-INFO-NAME PIC X(16).
           01 USER-INFO-CRED-DISPLAY.
               05 USER-INFO-CR-MESSAGE PIC X(9) VALUE "Credits: ".
               05 USER-INFO-CREDITS PIC 999.

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

           
           01 MSG-MENU-SCREEN.               
               05 BLANK SCREEN.          
               05 LINE 18 COL 59 VALUE "|--------MESSAGE BOARD--------|"
                   HIGHLIGHT, FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 59 VALUE "*-------RECENT MESSAGES-------*"
                   FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 48 VALUE "--------------------------------
      -        "---------------------" 
                   FOREGROUND-COLOR IS 2.
               05 LINE  21 COL 48 PIC XXX USING LIST-ID(ID-NUM)
                   FOREGROUND-COLOR 2.
               05 LINE  21 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM)
                   FOREGROUND-COLOR 2.
               05 LINE 22 COL 48 PIC XXX USING LIST-ID(ID-NUM + 1)
                   FOREGROUND-COLOR 2.
               05 LINE 22 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 1)
                   FOREGROUND-COLOR 2.
               05 LINE 23 COL 48 PIC XXX USING LIST-ID(ID-NUM + 2)
                   FOREGROUND-COLOR 2.
               05 LINE 23 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 2)
                   FOREGROUND-COLOR 2.
               05 LINE 24 COL 48 PIC XXX USING LIST-ID(ID-NUM + 3)
                   FOREGROUND-COLOR 2.
               05 LINE 24 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 3)
                   FOREGROUND-COLOR 2.
               05 LINE 25 COL 48 PIC XXX USING LIST-ID(ID-NUM + 4)
                   FOREGROUND-COLOR 2.
               05 LINE 25 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 4)
                   FOREGROUND-COLOR 2.
               05 LINE 26 COL 48 PIC XXX USING LIST-ID(ID-NUM + 5)
                   FOREGROUND-COLOR 2.
               05 LINE 26 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 5)
                   FOREGROUND-COLOR 2.
               05 LINE 27 COL 48 PIC XXX USING LIST-ID(ID-NUM + 6)
                   FOREGROUND-COLOR 2.
               05 LINE 27 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 6)
                   FOREGROUND-COLOR 2.
               05 LINE 28 COL 48 PIC XXX USING LIST-ID(ID-NUM + 7)
                   FOREGROUND-COLOR 2.
               05 LINE 28 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 7)
                   FOREGROUND-COLOR 2.
               05 LINE 29 COL 48 PIC XXX USING LIST-ID(ID-NUM + 8)
                   FOREGROUND-COLOR 2.
               05 LINE 29 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 8)
                   FOREGROUND-COLOR 2.
               05 LINE 30 COL 48 PIC XXX USING LIST-ID(ID-NUM + 9)
                   FOREGROUND-COLOR 2.
               05 LINE 30 COL 56 PIC X(50) USING LIST-TITLE(ID-NUM + 9)
                   FOREGROUND-COLOR 2.
               05 LINE 31 COL 48 VALUE "--------------------------------
      -        "-----------------------" 
                   FOREGROUND-COLOR IS 2.     
               05 LINE 34 COL 66 VALUE "( ) Read Message by Number "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.  
               05 LINE 35 COL 66 VALUE "(w) Write your own message "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.               
               05 LINE 36 COL 66 VALUE "(n) Next Page     "
                   HIGHLIGHT FOREGROUND-COLOR IS 3. 
               05 LINE 37 COL 66 VALUE "(p) Previous Page "
                   HIGHLIGHT FOREGROUND-COLOR IS 3. 
               05 LINE 38 COL 66 VALUE "(g) Go back       "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 39 COL 66 VALUE "(q) Quit          "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 41 COL 66 VALUE "Pick:"
                   FOREGROUND-COLOR IS 2.
               05 MSG-MENU-CHOICE-FIELD LINE 41 COL 71 PIC XXX
                   USING MSG-MENU-CHOICE
                   BLINK, FOREGROUND-COLOR IS 2.
               05 LINE 32 COL 66 PIC X(20) USING INSUFFICIENT-FUNDS
                   HIGHLIGHT FOREGROUND-COLOR IS 4.
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE"
                   FOREGROUND-COLOR 2.

           01 MESSAGE-VIEW-SCREEN.
               05 BLANK SCREEN.    
               05 LINE 18 COL 59 VALUE "|--------MESSAGE BOARD--------|"
                   HIGHLIGHT, FOREGROUND-COLOR IS 2.
            
               05 LINE 20 COL 50 VALUE "--------------------------------
      -        "-----------------------" 
                   FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 45 VALUE "Title: "
                   FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 53 PIC X(50) USING LIST-TITLE(MSG-SELECT)
                   FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 45 VALUE "Message: "
                   FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 54 PIC X(60) USING LS-PART-1
                   FOREGROUND-COLOR IS 2.
               05 LINE 25 COL 54 PIC X(60) USING LS-PART-2
                   FOREGROUND-COLOR IS 2.
               05 LINE 26 COL 54 PIC X(60) USING LS-PART-3
                   FOREGROUND-COLOR IS 2.
               05 LINE 27 COL 54 PIC X(60) USING LS-PART-4
                   FOREGROUND-COLOR IS 2.
               05 LINE 28 COL 54 PIC X(60) USING LS-PART-5
                   FOREGROUND-COLOR IS 2.
               05 LINE 30 COL 45 VALUE "Author: "
                   FOREGROUND-COLOR IS 2.
               05 LINE 30 COL 54 PIC X(16) 
                USING LIST-USERNAME(MSG-SELECT)
                   FOREGROUND-COLOR IS 2.       
               05 LINE 34 COL 50 VALUE "--------------------------------
      -        "-----------------------" 
                   FOREGROUND-COLOR IS 2.
               05 LINE 37 COL 45 VALUE "(g) Go back"
                   HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 38 COL 45 VALUE "(q) Quit   "
                   HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 39 COL 45 VALUE "Pick: "
                   FOREGROUND-COLOR IS 2.
               05 MSG-VIEW-CHOICE-FIELD LINE 39 COL 53 PIC X 
               USING MSG-VIEW-CHOICE
                   FOREGROUND-COLOR IS 2, BLINK. 
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE"
                   FOREGROUND-COLOR 2.

           01 WRITE-MSG-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.
               05 LINE 18 COL 59 VALUE "|--------MESSAGE BOARD--------|"
                   HIGHLIGHT, FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 50 VALUE "--------------------------------
      -        "------------------------" 
                   FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 45 VALUE "TITLE:   "
                   FOREGROUND-COLOR IS 2.     
               05 WS-TITLE-FIELD LINE 22 COL 54 PIC X(50) USING WS-TITLE
                   FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 45 VALUE "MESSAGE: "
                   FOREGROUND-COLOR IS 2.
               05 LINE-1-FIELD LINE 24 COL 54 PIC X(60) USING LS-PART-1
                   FOREGROUND-COLOR IS 2.
               05 LINE-2-FIELD LINE 25 COL 54 PIC X(60) USING LS-PART-2
                   FOREGROUND-COLOR IS 2.
               05 LINE-3-FIELD LINE 26 COL 54 PIC X(60) USING LS-PART-3
                   FOREGROUND-COLOR IS 2.
               05 LINE-4-FIELD LINE 27 COL 54 PIC X(60) USING LS-PART-4
                   FOREGROUND-COLOR IS 2.
               05 LINE-5-FIELD LINE 28 COL 54 PIC X(60) USING LS-PART-5
                   FOREGROUND-COLOR IS 2.
               05 LINE 29 COL 50 VALUE "--------------------------------
      -        "-----------------------"
                   FOREGROUND-COLOR IS 2.
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE"
                   FOREGROUND-COLOR IS 2.    
      
               
       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

        0130-MSG-MENU.
           PERFORM 0500-TIME-AND-DATE.
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
           SORT WS-LIST-ENTRY ON ASCENDING LIST-ID.
           INITIALIZE MSG-MENU-CHOICE.
           MOVE "1" TO COST.
           DISPLAY MSG-MENU-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT MSG-MENU-CHOICE-FIELD.
           MOVE MSG-MENU-CHOICE TO MSG-SELECT.
         
           IF MSG-SELECT > 0 THEN
               MOVE SPACES TO INSUFFICIENT-FUNDS
               PERFORM 0140-MESSAGE-VIEW            
           END-IF. 
           IF MSG-MENU-CHOICE = "g" OR 'G' THEN
               MOVE SPACES TO INSUFFICIENT-FUNDS
               GOBACK
           ELSE IF MSG-MENU-CHOICE = "n" OR 'N' THEN
             COMPUTE ID-NUM = ID-NUM + 10
               IF ID-NUM IS GREATER THAN OR EQUAL TO NUM-FILE-LINES
                 COMPUTE ID-NUM = ID-NUM - 10
                 MOVE SPACES TO INSUFFICIENT-FUNDS
                 PERFORM 0130-MSG-MENU              
               ELSE                
                   MOVE SPACES TO INSUFFICIENT-FUNDS
                   PERFORM 0130-MSG-MENU                  
               END-IF               
           ELSE IF MSG-MENU-CHOICE = 'p' OR 'P'
               COMPUTE ID-NUM = ID-NUM - 10
               IF ID-NUM IS LESS THAN 10
                   MOVE 1 TO ID-NUM
                    MOVE SPACES TO INSUFFICIENT-FUNDS
                    PERFORM 0130-MSG-MENU             
               ELSE
                    MOVE SPACES TO INSUFFICIENT-FUNDS
                    PERFORM 0130-MSG-MENU                  
               END-IF
           ELSE IF (MSG-MENU-CHOICE = 'w' OR 'W')
            AND (CHECK-BALANCE (COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               PERFORM 0150-MESSAGE-WRITE
           ELSE IF (MSG-MENU-CHOICE = 'w' OR 'W')
           AND (CHECK-BALANCE (COST, USER-INFO-CREDITS) = "FALSE") THEN
             MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
             PERFORM 0130-MSG-MENU 
           ELSE IF MSG-MENU-CHOICE = 'q' OR 'Q' THEN
              STOP RUN  
           END-IF.

           PERFORM 0130-MSG-MENU.

       0140-MESSAGE-VIEW. 
           PERFORM 0500-TIME-AND-DATE.          
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT MSG-VIEW-CHOICE-FIELD.
           IF MSG-VIEW-CHOICE = 'g' OR 'G' THEN
               PERFORM 0130-MSG-MENU
           ELSE IF MSG-VIEW-CHOICE = 'q' OR 'Q' THEN
              STOP RUN  
           END-IF.

       0150-MESSAGE-WRITE.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE WS-TITLE.
           INITIALIZE LS-PART-1.
           INITIALIZE LS-PART-2.
           INITIALIZE LS-PART-3.
           INITIALIZE LS-PART-4.
           INITIALIZE LS-PART-5.
           DISPLAY WRITE-MSG-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           
           ACCEPT WS-TITLE-FIELD.
           ACCEPT LINE-1-FIELD.
           ACCEPT LINE-2-FIELD.
           ACCEPT LINE-3-FIELD.
           ACCEPT LINE-4-FIELD.
           ACCEPT LINE-5-FIELD.
           
           MOVE WS-CONTENT-DISPLAY TO WS-CONTENT.
           MOVE USER-INFO-NAME TO WS-USERNAME.

           IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
             CALL 'post-message' USING NEW-MESSAGE
             PERFORM 0130-MSG-MENU
           END-IF.

           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           