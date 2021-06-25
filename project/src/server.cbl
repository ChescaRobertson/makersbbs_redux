       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           *>----- X AND O File Control-----    
             SELECT FD-WINMASKS ASSIGN TO "PLACEMENT.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.

             SELECT F-USERS-FILE ASSIGN TO 'users.dat'
                 ORGANIZATION IS LINE SEQUENTIAL. 

             SELECT F-ADMIN-FILE ASSIGN TO 'admins.dat'
                 ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION.
           FILE SECTION.
           *>----- X AND O F-Section-----   
           FD FD-WINMASKS.
           01 FD-WINMASK PIC X(9).

           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(10).  
              05 USER-CREDIT PIC 99. 

           FD F-ADMIN-FILE.
           01 ADMINS. 
               05 ADMIN PIC X(16).
               05 ADMIN-PWORD PIC X(20).
           
           WORKING-STORAGE SECTION.
           *>----- General Variables -----
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.

           01 START-CHOICE PIC X.
           01 WS-COUNTER PIC 99.

           *>----- Login Variables -----

           01 USER-NAME PIC X(16).
           01 WS-PASSWORD PIC X(20).
           01 ACCOUNT-NUM PIC X(10).
           01 CREDIT PIC 99.

           01 LOGGED-IN PIC X(15).
           01 CRED-DISPLAY PIC X(8).
           01 CRED PIC 9.

           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-USER-NAME
               INDEXED BY USER-IDX.
                   10 WS-USER-NAME PIC X(16).    
                   10 WS-PWORD PIC X(20).
                   10 WS-ACNT-NUM PIC X(10).
                   10 WS-CREDIT PIC 99. 

           01 WS-FOUND PIC 9. 
           01 WS-IDX UNSIGNED-INT. 
           01 COUNTER UNSIGNED-INT. 

           01 NEW-USER-NAME PIC X(16).
           01 NEW-PASSWORD PIC X(20).
           01 REGISTER-CHOICE PIC X.
           01 ERROR-MSG-1 PIC X(50).
           01 ERROR-MSG-2 PIC X(50).
           01 ERROR-MSG-3 PIC X(50).
           01 NEW-CHOICE PIC X.
           01 ERROR-CHOICE PIC X. 

           01 ADMIN-NAME PIC X(16).
           01 ADMIN-PASSWORD PIC X(20).

           01 WS-ADMINS.
               05 WS-ADMIN OCCURS 10 TIMES
               ASCENDING KEY IS WS-ADMIN-NAME
               INDEXED BY ADMIN-IDX.
                   10 WS-ADMIN-NAME PIC X(16).    
                   10 WS-ADMIN-PWORD PIC X(20).

           01 ADMIN-ERROR PIC X.
           01 ADMIN-CHOICE PIC X.

           01 MENU-CHOICE PIC X.
         
           *>----- Date Variables -----
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

           *>----- Arcade Variables -----
           01 GAMES-MENU-CHOICE PIC X.
           01 HIDDEN-MENU-CHOICE PIC X.

           *>-----X AND O WS-SECTION-----   
           01 WS-PLAYER PIC A(1).
                   88 HUMAN-PLAYER VALUE "X".
                   88 COMPUTER-PLAYER VALUE "O".
               01 WS-STATE PIC A(5).
                   88 GAME-OVER VALUES "WIN", "LOSE", "STALE".
               01 WS-MOVE-OUTCOME PIC A(5).
                   88 MOVE-COMPLETE VALUES "WIN", "LOSE", "FAIL".
               01 WS-MASK-DETECTED PIC 9(1).
                   88 WIN-DETECTED VALUES 3, 4, 5, 6, 7, 8, 9.
               01 WS-COMPUTER-MOVED PIC 9(1).
                   88 COMPUTER-MOVED VALUE 1.
               01 WS-EOF PIC 9(1).
                   88 EOF VALUE 1.
               01 WS-SWAP-PLAYERS PIC 9(1).
                   88 SWAP-PLAYERS VALUE 1.
               01 WS-NEXT-MOVE PIC X(2).
                   88 FINISHED-PLAYING VALUES "N", "n".
               01 WS-GAME-GRID.
                   05 WS-GAME-GRID-ROW OCCURS 3 TIMES.
                       10 WS-GAME-GRID-COL OCCURS 3 TIMES.
                           15 WS-CELL PIC X(1).
               01 WS-COLOR-GREEN PIC 9(1) VALUE 2.
               01 WS-COLOR-BLACK PIC 9(1) VALUE 0.
               01 WS-COLOR-WHITE PIC 9(1) VALUE 7.
               01 WS-COLOR-BLUE PIC 9(1) VALUE 3.
               01 WS-COLOR-RED PIC 9(1) VALUE 4.
               01 WS-FG-CELL PIC 9(1).
               01 WS-FG PIC 9(1).
               01 WS-BG PIC 9(1).
               01 WS-COL PIC 9(1).
               01 WS-ROW PIC 9(1).
               01 WS-WINS PIC 9(2).
               01 WS-MOVES PIC 9(2).
               01 WS-GAMES PIC 9(2).
               01 WS-COMPUTER-MOVE PIC 9(1).
               01 WS-DETECT-LOOP-COUNT PIC 9(1).
               01 WS-OANDXMESSAGE PIC X(128).
               01 WS-INSTRUCTION PIC X(16).
               01 WS-FLAT-GAME-GRID PIC X(9).

           *>-----RANDOM-NUM-GAME WS-SECTION-----
           01 SEED PIC 9(8).
           01 GUESS-INPUT PIC XX.
           01 GUESS PIC 99.
           01 ANSWER PIC 99.
           01 TOTAL-GUESSES PIC 99.
           01 WS-RANDOM-NUM-MSG PIC X(128). 

           *>----- Library Variables -----


           *>----- Admin Variables -----   

           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

           SCREEN SECTION.
           01 USER-INFO-SCREEN.
            05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
            05 LINE 2 COL 4 VALUE ":".         
            05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
            05 LINE 7 COL 2 PIC X(7) USING CRED-DISPLAY.
            05 LINE 8 COL 2 PIC 9 USING CRED.   
            05 LINE 2 COL 66 PIC X(15) USING LOGGED-IN.
            05 LINE 3 COL 75 PIC X(16) USING USER-NAME HIGHLIGHT,
            FOREGROUND-COLOR IS 2.
               

           01 START-SCREEN. 
            05 BLANK SCREEN.
            05 LINE 2 COLUMN 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
            HIGHLIGHT, FOREGROUND-COLOR IS 3.
            05 LINE 4 COLUMN 12 VALUE "(l) Go to Log-in.".
            05 LINE 5 COLUMN 12 VALUE "(c) Create an account.".
            05 LINE 6 COLUMN 12 VALUE "(q) Quit.". 
            05 LINE 8 COLUMN 12 VALUE "Pick: ".
            05 START-CHOICE-FIELD LINE 8 COLUMN 18 PIC X
                USING START-CHOICE.
            05 LINE 12 COLUMN 12 VALUE "(a) Administrator.".
           
           01 REGISTER-NEW-USER-SCREEN
              BACKGROUND-COLOR IS 0.
                 05 BLANK SCREEN. 
                 05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
                 HIGHLIGHT, FOREGROUND-COLOR IS 3.
                 05 LINE 08 COl 12 VALUE
           "The TMNCT present:".                       
                 05 LINE 10 COl 12 VALUE   
           "______       _ _      _   _" FOREGROUND-COLOR IS 3.
                 05 LINE 11 COl 10 VALUE         
           "  | ___ \     | | |    | | (_)" FOREGROUND-COLOR IS 3.
                 05 LINE 12 COl 10 VALUE  
           "  | |_/ /_   _| | | ___| |_ _ _ __" FOREGROUND-COLOR IS 5.
                 05 LINE 13 COl 10 VALUE    
           "  | ___ \ | | | | |/ _ \ __| | '_ \" FOREGROUND-COLOR IS 5.
                 05 LINE 14 COl 10 VALUE   
           "  | |_/ / |_| | | |  __/ |_| | | | |" FOREGROUND-COLOR IS 2.
                 05 LINE 15 COl 10 VALUE  
           "  \____/ \__,_|_|_|\___|\__|_|_| |_|" FOREGROUND-COLOR IS 2.
                 05 LINE 18 COl 10 VALUE                                                                        
           "    ______                     _" FOREGROUND-COLOR IS 2.
                 05 LINE 19 COl 10 VALUE      
           "    | ___ \                   | |" FOREGROUND-COLOR IS 2.
                 05 LINE 20 COl 10 VALUE     
           "    | |_/ / ___   __ _ _ __ __| |" FOREGROUND-COLOR IS 5.
                 05 LINE 21 COl 10 VALUE     
           "    | ___ \/ _ \ / _` | '__/ _` |" FOREGROUND-COLOR IS 5.
                 05 LINE 22 COl 10 VALUE     
           "    | |_/ / (_) | (_| | | | (_| |" FOREGROUND-COLOR IS 3.
                 05 LINE 23 COl 10 VALUE     
           "    \____/ \___/ \__,_|_|  \__,_|" FOREGROUND-COLOR IS 3.
             05 LINE 27 COLUMN 12 VALUE "CREATE AN ACCOUNT" HIGHLIGHT,
             FOREGROUND-COLOR IS 3.
             05 LINE 29 COLUMN 12 VALUE "input intro text explaining" 
             FOREGROUND-COLOR IS 5.
             05 LINE 33 COLUMN 12 VALUE "Enter a username:".
             05 NEW-USER-NAME-FIELD LINE 35 COLUMN 12 PIC X(16)
                USING NEW-USER-NAME.
             05 LINE 37 COLUMN 12 VALUE "Enter a password ".
             05 LINE 37 COLUMN 30 VALUE "(password can be a maximum of".
             05 LINE 37 COLUMN 58 VALUE " 20 characters):".
             05 NEW-PASSWORD-FIELD LINE 39 COLUMN 12 PIC X(20)
                USING NEW-PASSWORD.
             05 LINE 41 COLUMN 12 VALUE "Enter a valid Bank Account numb
      -      "er :".
             05 ACCOUNT-NUM-FIELD LINE 43 COLUMN 12 PIC X(8)
                USING ACCOUNT-NUM.
             05 LINE 45 COLUMN 12 VALUE "(s) Submit".
             05 LINE 46 COLUMN 12 VALUE "(q) Go Back".
             05 LINE 48 COLUMN 12 VALUE "Pick: ".
             05 REGISTER-CHOICE-FIELD LINE 48 COLUMN 18 PIC X
                USING REGISTER-CHOICE.
               
           01 NEW-MENU. 
             05 LINE 45 COLUMN 12 VALUE "(r) Re-Enter Details" HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 LINE 46 COLUMN 12 VALUE "(q) Go Back" HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 LINE 48 COLUMN 12 VALUE "Pick: " HIGHLIGHT 
             FOREGROUND-COLOR is 4.
             05 NEW-CHOICE-FIELD  LINE 48 COLUMN 18 PIC X USING 
             NEW-CHOICE HIGHLIGHT FOREGROUND-COLOR is 4.
             05 LINE 36 COLUMN 12 PIC X(50) USING ERROR-MSG-1 HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 LINE 40 COLUMN 12 PIC X(50) USING ERROR-MSG-2 HIGHLIGHT
             FOREGROUND-COLOR is 4.
             05 LINE 44 COLUMN 12 PIC X(50) USING ERROR-MSG-3 HIGHLIGHT
             FOREGROUND-COLOR is 4.
             

           01 LOGIN-SCREEN
                 BACKGROUND-COLOR IS 0.
                 05 BLANK SCREEN.  
                 05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
                 HIGHLIGHT, FOREGROUND-COLOR IS 3.
                 05 LINE 08 COl 12 VALUE
           "The TMNCT present:".                       
                 05 LINE 10 COl 12 VALUE   
           "______       _ _      _   _" FOREGROUND-COLOR IS 3.
                 05 LINE 11 COl 10 VALUE         
           "  | ___ \     | | |    | | (_)" FOREGROUND-COLOR IS 3.
                 05 LINE 12 COl 10 VALUE  
           "  | |_/ /_   _| | | ___| |_ _ _ __" FOREGROUND-COLOR IS 5.
                 05 LINE 13 COl 10 VALUE    
           "  | ___ \ | | | | |/ _ \ __| | '_ \" FOREGROUND-COLOR IS 5.
                 05 LINE 14 COl 10 VALUE   
           "  | |_/ / |_| | | |  __/ |_| | | | |" FOREGROUND-COLOR IS 2.
                 05 LINE 15 COl 10 VALUE  
           "  \____/ \__,_|_|_|\___|\__|_|_| |_|" FOREGROUND-COLOR IS 2.
                 05 LINE 18 COl 10 VALUE                                                                        
           "    ______                     _" FOREGROUND-COLOR IS 2.
                 05 LINE 19 COl 10 VALUE      
           "    | ___ \                   | |" FOREGROUND-COLOR IS 2.
                 05 LINE 20 COl 10 VALUE     
           "    | |_/ / ___   __ _ _ __ __| |" FOREGROUND-COLOR IS 5.
                 05 LINE 21 COl 10 VALUE     
           "    | ___ \/ _ \ / _` | '__/ _` |" FOREGROUND-COLOR IS 5.
                 05 LINE 22 COl 10 VALUE     
           "    | |_/ / (_) | (_| | | | (_| |" FOREGROUND-COLOR IS 3.
                 05 LINE 23 COl 10 VALUE     
           "    \____/ \___/ \__,_|_|  \__,_|" FOREGROUND-COLOR IS 3.
                 05 LINE 27 COL 12 VALUE "Enter your username:".
                 05 USER-NAME-FIELD LINE 29 COL 12 PIC X(16)
                    USING USER-NAME.
                 05 LINE 31 COL 12 VALUE "Enter your password:".
                 05 PASSWORD-FIELD LINE 33 COLUMN 12 PIC X(20)
                    USING WS-PASSWORD.   
                              
           01 ERROR-SCREEN
                 BACKGROUND-COLOR IS 0.
                 05 BLANK SCREEN.                
                 05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
                 HIGHLIGHT, FOREGROUND-COLOR IS 3.
                 05 LINE 08 COl 12 VALUE
           "The TMNCT present:".                       
                 05 LINE 10 COl 12 VALUE   
           "______       _ _      _   _" FOREGROUND-COLOR IS 3.
                 05 LINE 11 COl 10 VALUE         
           "  | ___ \     | | |    | | (_)" FOREGROUND-COLOR IS 3.
                 05 LINE 12 COl 10 VALUE  
           "  | |_/ /_   _| | | ___| |_ _ _ __" FOREGROUND-COLOR IS 5.
                 05 LINE 13 COl 10 VALUE    
           "  | ___ \ | | | | |/ _ \ __| | '_ \" FOREGROUND-COLOR IS 5.
                 05 LINE 14 COl 10 VALUE   
           "  | |_/ / |_| | | |  __/ |_| | | | |" FOREGROUND-COLOR IS 2.
                 05 LINE 15 COl 10 VALUE  
           "  \____/ \__,_|_|_|\___|\__|_|_| |_|" FOREGROUND-COLOR IS 2.
                 05 LINE 18 COl 10 VALUE                                                                        
           "    ______                     _" FOREGROUND-COLOR IS 2.
                 05 LINE 19 COl 10 VALUE      
           "    | ___ \                   | |" FOREGROUND-COLOR IS 2.
                 05 LINE 20 COl 10 VALUE     
           "    | |_/ / ___   __ _ _ __ __| |" FOREGROUND-COLOR IS 5.
                 05 LINE 21 COl 10 VALUE     
           "    | ___ \/ _ \ / _` | '__/ _` |" FOREGROUND-COLOR IS 5.
                 05 LINE 22 COl 10 VALUE     
           "    | |_/ / (_) | (_| | | | (_| |" FOREGROUND-COLOR IS 3.
                 05 LINE 23 COl 10 VALUE     
           "    \____/ \___/ \__,_|_|  \__,_|" FOREGROUND-COLOR IS 3.
             05 LINE 27 COLUMN 12 VALUE "Incorrect Username or Password"
             HIGHLIGHT, FOREGROUND-COLOR IS 4.
             05 LINE 29 COLUMN 12 VALUE "(l) Back to Log-in.".
             05 LINE 30 COLUMN 12 VALUE "(c) Create an account.".
             05 LINE 31 COLUMN 12 VALUE "(q) Go Back." .
             05 LINE 33 COLUMN 12 VALUE "Pick: ".
             05 ERROR-CHOICE-FIELD LINE 33 COLUMN 18 PIC X
                USING ERROR-CHOICE.

           01 ADMIN-LOGIN-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN. 
             05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 6 COL 12 VALUE "Enter Administrator username:".
             05 ADMIN-NAME-FIELD LINE 8 COL 12 PIC X(16)
                USING ADMIN-NAME.
             05 LINE 10 COL 12 VALUE "Enter Administrator password:".
             05 ADMIN-PASSWORD-FIELD LINE 12 COLUMN 12 PIC X(20)
                USING ADMIN-PASSWORD.  

           01 ADMIN-ERROR-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.         
             05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 6 COLUMN 12 VALUE "* Administrator details not reco
      -       "gnised *." HIGHLIGHT, FOREGROUND-COLOR IS 4.
             05 LINE 8 COLUMN 12 VALUE "(a) Administrator Log-in.".
             05 LINE 9 COLUMN 12 VALUE "(q) Go Back." .
             05 LINE 11 COLUMN 12 VALUE "Pick: ".
             05 ADMIN-ERROR-FIELD LINE 11 COLUMN 18 PIC X
                USING ADMIN-ERROR.
           
           01 ADMIN-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 4 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 8 COL 10 VALUE "Welcome, ".
             05 LINE 8 COL 19 PIC X(16) USING ADMIN-NAME.
             05 LINE 10 COL 10 VALUE "Please select from the below optio
      -      "ns.".  
             05 LINE 13 COL 10 VALUE "(s) View Statements "
                REVERSE-VIDEO HIGHLIGHT.
             05 LINE 13 COL 32 VALUE "(u) Manage Users    "
                REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 15 COL 10 VALUE "(s) Add Admin       "
                REVERSE-VIDEO HIGHLIGHT.
             05 LINE 15 COL 32 VALUE "(u) Manage Posts    "
                REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 17 COL 10 VALUE "(l) Logout          "
                REVERSE-VIDEO , HIGHLIGHT.             
             05 LINE 17 COL 32 VALUE "(q) Quit            "
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 21 COL 14 VALUE "Pick: ".
             05 ADMIN-CHOICE-FIELD LINE 21 COL 20 PIC X
                USING ADMIN-CHOICE.

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING USER-NAME.
             05 LINE  8 COL 10 VALUE "Welcome to TMNCT's state of the ar
      -      "t Bulletin Board.".  
             05 LINE  9 COL 10 VALUE "Feel free to:".
             05 LINE 10 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 26 VALUE "Read our message board.".
             05 LINE 11 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 11 COL 26 VALUE "Play a few games.".
             05 LINE 12 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 12 COL 26 VALUE "Leave a message of your own.". 
             05 LINE 13 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 13 COL 26 VALUE "Most importantly. HAVE FUN!". 

             05 LINE 19 COL 24 VALUE "(m) Messages    "
                REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.
             05 LINE 19 COL 42 VALUE "(f) Fun & games "
                REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 21 COL 24 VALUE "(l) Logout      "
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 21 COL 42 VALUE "(q) Quit        "
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 23 COL 24 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 23 COL 30 PIC X
                USING MENU-CHOICE.

             05 LINE 27 COL 25 VALUE "     [.. [....... [..       [..".
             05 LINE 28 COL 25 VALUE "     [.. [..      [. [..   [...".
             05 LINE 29 COL 25 VALUE "     [.. [..      [.. [.. [ [..".
             05 LINE 30 COL 25 VALUE "     [.. [......  [..  [..  [..".
             05 LINE 31 COL 25 VALUE "     [.. [..      [..   [.  [..".
             05 LINE 32 COL 25 VALUE " [.  [.. [..      [..       [..".
             05 LINE 33 COL 25 VALUE " [...... [....... [..       [.. ".        
           
           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "          +++             -`^'-         
      -      "         )))" FOREGROUND-COLOR IS 6.
             05 LINE 7 COL 10 VALUE "         (o o)            (o o)            
      -      "        (o o)" FOREGROUND-COLOR IS 3.
             05 LINE 8 COL 10 VALUE "-----ooO--(_)--Ooo----ooO--(_)--Ooo
      -      "----ooO--(_)--Ooo----" FOREGROUND-COLOR IS 3.

             05 LINE 9 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 10 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE  11 COL 10 PIC XXX USING LIST-ID(ID-NUM).
             05 LINE  11 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM).
             05 LINE 12 COL 10 PIC XXX USING LIST-ID(ID-NUM + 1).
             05 LINE 12 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 1).
             05 LINE 13 COL 10 PIC XXX USING LIST-ID(ID-NUM + 2).
             05 LINE 13 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 2).
             05 LINE 14 COL 10 PIC XXX USING LIST-ID(ID-NUM + 3).
             05 LINE 14 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 3).
             05 LINE 15 COL 10 PIC XXX USING LIST-ID(ID-NUM + 4).
             05 LINE 15 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 4).
             05 LINE 16 COL 10 PIC XXX USING LIST-ID(ID-NUM + 5).
             05 LINE 16 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 5).
             05 LINE 17 COL 10 PIC XXX USING LIST-ID(ID-NUM + 6).
             05 LINE 17 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 6).
             05 LINE 18 COL 10 PIC XXX USING LIST-ID(ID-NUM + 7).
             05 LINE 18 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 7).
             05 LINE 19 COL 10 PIC XXX USING LIST-ID(ID-NUM + 8).
             05 LINE 19 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 8).
             05 LINE 20 COL 10 PIC XXX USING LIST-ID(ID-NUM + 9).
             05 LINE 20 COL 14 PIC X(50) USING LIST-TITLE(ID-NUM + 9).
             05 LINE 21 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 22 COL 10 VALUE "*********************RECENT MESSAG
      -      "ES*******************" FOREGROUND-COLOR IS 2.
             05 LINE 23 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 25 COL 24 VALUE "( ) Read Message by Number "
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.  
             05 LINE 27 COL 24 VALUE "(w) Write your own message "
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.               
             05 LINE 29 COL 18 VALUE "(n) Next Page     "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.  
             05 LINE 29 COL 41 VALUE "(p) Previous Page "
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6. 
             05 LINE 31 COL 18 VALUE "(g) Go back       "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 31 COL 41 VALUE "(q) Quit          "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 33 COL 18 VALUE "Pick: ".
             05 MSG-MENU-CHOICE-FIELD LINE 33 COL 24 PIC XXX
                USING MSG-MENU-CHOICE.

           01 MESSAGE-VIEW-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "          \|/             '%%%'         
      -      "         (((" FOREGROUND-COLOR IS 6.
             05 LINE  7 COL 10 VALUE "         (o o)            (> o)            
      -      "        (o o)" FOREGROUND-COLOR IS 3.
             05 LINE 8 COL 10 VALUE "-----ooO--(_)--Ooo----ooO--(_)--Ooo
      -      "----ooO--(_)--Ooo----" FOREGROUND-COLOR IS 3.

             05 LINE 9 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 10 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 12 COL 10 VALUE "Title: ".
             05 LINE 12 COL 19 PIC X(50) USING LIST-TITLE(MSG-SELECT).
             05 LINE 14 COL 10 VALUE "Message: ".
             05 LINE 14 COL 19 PIC X(60) USING LS-PART-1.
             05 LINE 15 COL 19 PIC X(60) USING LS-PART-2.
             05 LINE 16 COL 19 PIC X(60) USING LS-PART-3.
             05 LINE 17 COL 19 PIC X(60) USING LS-PART-4.
             05 LINE 18 COL 19 PIC X(60) USING LS-PART-5.
             05 LINE 20 COL 10 VALUE "Author: ".
             05 LINE 20 COL 19 PIC X(16) 
                USING LIST-USERNAME(MSG-SELECT).
             05 LINE 22 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 23 COL 10 VALUE "*********************CHOSEN MESSAG
      -      "E********************" FOREGROUND-COLOR IS 2.
             05 LINE 24 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.  
             05 LINE 27 COL 25 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 27 COL 39 VALUE "(q) Quit   "
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 28 COL 25 VALUE "Pick: ".
             05 MSG-VIEW-CHOICE-FIELD LINE 28 COL 31 PIC X 
               USING MSG-VIEW-CHOICE.

           01 WRITE-MSG-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "         ~@@@~            '^^^'         
      -      "        .:;:." FOREGROUND-COLOR IS 6.
             05 LINE  7 COL 10 VALUE "         (o-o)            (> <)            
      -      "        (> o)" FOREGROUND-COLOR IS 3.
             05 LINE  6 COL 10 VALUE "-----ooO--(_)--Ooo----ooO--(_)--Ooo
      -      "----ooO--(_)--Ooo----" FOREGROUND-COLOR IS 3.

             05 LINE 8 COL 10 VALUE "*********************BULLETIN BOARD
      -      "*********************" BLINK, HIGHLIGHT, FOREGROUND-COLOR 
             IS 2.
             05 LINE 9 COL 10 VALUE "-----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 11 COL 10 VALUE "TITLE:   ".
             05 WS-TITLE-FIELD LINE 11 COL 18 PIC X(50) USING WS-TITLE.
             05 LINE 13 COL 10 VALUE "MESSAGE: ".
             05 LINE-1-FIELD LINE 15 COL 10 PIC X(60) USING LS-PART-1.
             05 LINE-2-FIELD LINE 16 COL 10 PIC X(60) USING LS-PART-2.
             05 LINE-3-FIELD LINE 17 COL 10 PIC X(60) USING LS-PART-3.
             05 LINE-4-FIELD LINE 18 COL 10 PIC X(60) USING LS-PART-4.
             05 LINE-5-FIELD LINE 19 COL 10 PIC X(60) USING LS-PART-5. 
             05 LINE 21 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
             05 LINE 22 COL 10 VALUE "*********************LEAVE A MESSA
      -      "GE*******************" FOREGROUND-COLOR IS 2.
             05 LINE 23 COL 10 VALUE "----------------------------------
      -      "---------------------" FOREGROUND-COLOR IS 3.
           
           01 GAMES-MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 LINE 4 COL 10 VALUE".------..------..------..------..---
      -      "---." FOREGROUND-COLOR IS 3.
             05 LINE 5 COL 10 VALUE"|G.--. ||A.--. ||M.--. ||E.--. ||S.-
      -      "-. |" FOREGROUND-COLOR IS 2.
             05 LINE 6 COL 10 VALUE"| :/\: || (\/) || :/\: || (\/) || :/
      -      "\: |" FOREGROUND-COLOR IS 5.
             05 LINE 7 COL 10 VALUE"| :\/: || :\/: || :\/: || :\/: || :\
      -      "/: |" FOREGROUND-COLOR IS 5.
             05 LINE 8 COL 10 VALUE"| '--'G|| '--'A|| '--'M|| '--'E|| '-
      -      "-'S|" FOREGROUND-COLOR IS 2.
             05 LINE 9 COL 10 VALUE"`------'`------'`------'`------'`---
      -      "---'" FOREGROUND-COLOR IS 3.
             05 LINE 10 COL 18 VALUE"          ___"
             FOREGROUND-COLOR IS 4.
             05 LINE 11 COL 18 VALUE"        ,'---'."
             FOREGROUND-COLOR IS 4.
             05 LINE 12 COL 18 VALUE"        :     ;"
             FOREGROUND-COLOR IS 4.
             05 LINE 13 COL 18 VALUE"         `-.-'"
             FOREGROUND-COLOR IS 4.
             05 LINE 14 COL 18 VALUE"          | |" 
             FOREGROUND-COLOR IS 6.
             05 LINE 15 COL 18 VALUE"          | |"
             FOREGROUND-COLOR IS 6.
             05 LINE 16 COL 18 VALUE"          | |"
             FOREGROUND-COLOR IS 6.
             05 LINE 17 COL 18 VALUE"       _.-\_/-._"
             FOREGROUND-COLOR IS 3.
             05 LINE 18 COL 18 VALUE"    _ / |     | \ _"
             FOREGROUND-COLOR IS 3.
             05 LINE 19 COL 18 VALUE"   / /   `---'   \ \"
             FOREGROUND-COLOR IS 3.
             05 LINE 20 COL 18 VALUE"  /  `-----------'  \"
             FOREGROUND-COLOR IS 3.
             05 LINE 21 COL 18 VALUE" /,-''-.       ,-''-.\"
             FOREGROUND-COLOR IS 4.
             05 LINE 22 COL 18 VALUE"( i-..-i       i-..-i )"
             FOREGROUND-COLOR IS 4.
             05 LINE 23 COL 18 VALUE"|`|    |-------|    |'|"
             FOREGROUND-COLOR IS 4.
             05 LINE 24 COL 18 VALUE"\ `-..-'  ,=.  `-..-' /"
             FOREGROUND-COLOR IS 4.
             05 LINE 25 COL 18 VALUE" `--------|=|--------'"
             FOREGROUND-COLOR IS 3.

             05 LINE 30 COL 21 VALUE "(n) Guess The Number" 
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 32 COL 21 VALUE "(o) O and X         "  
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 5.
             05 LINE 34 COL 21 VALUE "(m) Monkey?         " 
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.
             05 LINE 36 COL 18 VALUE "(g) Go back "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 36 COL 32 VALUE "(q) Quit    "
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 38 COL 18 VALUE "Pick: ".
             05 GAMES-MENU-CHOICE-FIELD LINE 38 COL 24 PIC X
                USING GAMES-MENU-CHOICE.     
             
           01 BOARD-SCREEN.
               05 BLANK SCREEN.
               05 LINE 1 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 2 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 3 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               05 LINE 4 COl 18 VALUE  "  ___       _    _   _ ____   __
      -        "  __" FOREGROUND-COLOR IS 3.
               05 LINE 5 COl 18 VALUE " / _ \     / \  | \ | |  _ \  \ \
      -        "/ /" FOREGROUND-COLOR IS 5.
               05 LINE 6 COl 18 VALUE "| | | |   / _ \ |  \| | | | |  \  
      -        " /" FOREGROUND-COLOR IS 3.
               05 LINE 7 COl 18 VALUE "| |_| |  / ___ \| |\  | |_| |  /  
      -         " \" FOREGROUND-COLOR IS 2.
               05 LINE 8 COl 18 VALUE " \___/  /_/   \_\_| \_|____/  /_/
      -        "\_\" FOREGROUND-COLOR IS 5.
               05 LINE 10 COL 10 VALUE "---------------------------------
      -      "----------------------" FOREGROUND-COLOR IS 2.
               05 LINE 11 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 12 COL 10 VALUE "--------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 14 COLUMN 27 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 15 COLUMN 27 VALUE IS " A |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 16 COLUMN 27 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 17 COLUMN 27 VALUE IS " B |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 18 COLUMN 27 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 19 COLUMN 27 VALUE IS " C |   |   |   |   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 20 COLUMN 27 VALUE IS "   +---+---+---+   "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 21 COLUMN 27 VALUE IS "     1   2   3     "
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 15 COLUMN 32 PIC A(1) FROM WS-CELL(1,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 15 COLUMN 36 PIC A(1) FROM WS-CELL(1,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 15 COLUMN 40 PIC A(1) FROM WS-CELL(1,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 32 PIC A(1) FROM WS-CELL(2,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 36 PIC A(1) FROM WS-CELL(2,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 17 COLUMN 40 PIC A(1) FROM WS-CELL(2,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 32 PIC A(1) FROM WS-CELL(3,1)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 36 PIC A(1) FROM WS-CELL(3,2)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 19 COLUMN 40 PIC A(1) FROM WS-CELL(3,3)
                   BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.

               05 LINE 23 COLUMN 27 VALUE IS "Message: "
                   FOREGROUND-COLOR IS 6.
                   05 MSG PIC X(128) FROM WS-OANDXMESSAGE.
               05 LINE 25 COLUMN 27 PIC X(16) FROM WS-INSTRUCTION.
                   05 NEXT-MOVE PIC X(2) USING WS-NEXT-MOVE.
               05 LINE 27 COLUMN 27 VALUE IS "Stats: "
                   FOREGROUND-COLOR IS 6.
               05 LINE 28 COLUMN 27 VALUE IS "Moves played = "
                   FOREGROUND-COLOR IS 2.
                   05 MOVES PIC 9(1) FROM WS-MOVES.
               05 LINE 29 COLUMN 27 VALUE IS "Games won = "
                   FOREGROUND-COLOR IS 5.
                   05 WINS PIC 9(2) FROM WS-WINS.
               05 LINE 29 COLUMN 41 VALUE IS "/".
                   05 GAMES PIC 9(2) FROM WS-GAMES. 
               05 LINE 31 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 32 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 33 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.

           01 GUESS-SCREEN.
           05 BLANK SCREEN.
               05 LINE 2 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 3 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 4 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               05 LINE 6 COl 14 VALUE  " __    __ __ __   ___    __     
      -        "        _  __ _" FOREGROUND-COLOR IS 3.
               05 LINE 7 COl 14 VALUE  "/__| ||_ (_ (_     | |_||_    |\
      -        "|| ||V||_)|_ |_)" FOREGROUND-COLOR IS 5.
               05 LINE 8 COl 14 VALUE  "\_||_||____)__)    | | ||__   | 
      -        "||_|| ||_)|__| \" FOREGROUND-COLOR IS 2.
               05 LINE 10 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               05 LINE 11 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 12 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 14 COLUMN 14 VALUE IS "Message: "
               FOREGROUND-COLOR IS 6.
               05 MSG PIC X(128) FROM WS-RANDOM-NUM-MSG.
               05 GUESS-FIELD LINE 16 COLUMN 14 PIC XX USING GUESS-INPUT
               .
               05 LINE 20 COLUMN 14 VALUE IS "Stats: "
               FOREGROUND-COLOR IS 6.
               05 LINE 22 COLUMN 14 VALUE IS "Total Guesses = "
               FOREGROUND-COLOR IS 5.
                   05 GUESSES PIC 99 FROM TOTAL-GUESSES. 
               05 LINE 24 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 26 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.

       PROCEDURE DIVISION.
           
       0090-GENERATE-USER-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-USERS-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE USERNAME TO WS-USER-NAME(COUNTER)
                       MOVE USER-PASSWORD TO WS-PWORD(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-USERS-FILE.

       0100-DISPLAY-START.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE START-CHOICE.
           DISPLAY START-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT START-CHOICE-FIELD.
           IF START-CHOICE = "l" THEN 
               PERFORM 0110-DISPLAY-LOGIN 
           ELSE IF START-CHOICE = "c" THEN 
               PERFORM 0105-DISPLAY-REGISTER-NEW-USER
           ELSE IF START-CHOICE = "q" THEN 
               STOP RUN
           ELSE IF START-CHOICE = "a" THEN 
               PERFORM 0116-ADMIN-LOGIN-PAGE
           ELSE 
               PERFORM 0100-DISPLAY-START
           END-IF.
       
       0105-DISPLAY-REGISTER-NEW-USER.
           PERFORM 0500-TIME-AND-DATE.
           PERFORM 0090-GENERATE-USER-TABLE.
           MOVE SPACES TO ERROR-MSG-1.
           MOVE SPACES TO ERROR-MSG-2.
           MOVE SPACES TO ERROR-MSG-3.
           INITIALIZE NEW-USER-NAME.
           INITIALIZE NEW-PASSWORD.
           INITIALIZE ACCOUNT-NUM.
           INITIALIZE REGISTER-CHOICE
           DISPLAY REGISTER-NEW-USER-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT NEW-USER-NAME-FIELD.
           ACCEPT NEW-PASSWORD-FIELD.
           ACCEPT ACCOUNT-NUM-FIELD.
           ACCEPT REGISTER-CHOICE-FIELD.
           MOVE 0 TO WS-FOUND.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF NEW-USER-NAME = WS-USER-NAME(WS-IDX) 
                   MOVE "USER NAME IN USE" TO ERROR-MSG-1 
                   ADD 1 TO WS-FOUND
               END-IF
                   ADD 1 TO WS-IDX
           END-PERFORM.
           IF REGISTER-CHOICE = "q" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE IF REGISTER-CHOICE = "s" AND WS-FOUND > 0 
               PERFORM 0106-NEW-MENU
           ELSE IF REGISTER-CHOICE = "s" AND WS-FOUND = 0
               OPEN EXTEND F-USERS-FILE
               MOVE NEW-USER-NAME TO USERNAME
               MOVE NEW-PASSWORD TO USER-PASSWORD
               MOVE ACCOUNT-NUM TO USER-ACNT-NUM
               WRITE USERS
               END-WRITE 
           END-IF.
           CLOSE F-USERS-FILE.
           PERFORM 0110-DISPLAY-LOGIN.

       0106-NEW-MENU.
           INITIALIZE NEW-CHOICE.
           DISPLAY NEW-MENU 
           DISPLAY USER-INFO-SCREEN
           ACCEPT NEW-CHOICE-FIELD. 
           IF NEW-CHOICE = "r" THEN 
               PERFORM 0105-DISPLAY-REGISTER-NEW-USER
           ELSE IF NEW-CHOICE = "q" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE 
               PERFORM 0106-NEW-MENU
           END-IF.

       0110-DISPLAY-LOGIN.
           PERFORM 0500-TIME-AND-DATE.
           PERFORM 0090-GENERATE-USER-TABLE
           INITIALIZE USER-NAME.
           INITIALIZE WS-PASSWORD.
           DISPLAY LOGIN-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT USER-NAME-FIELD.
           ACCEPT PASSWORD-FIELD.
           MOVE "Logged in as:" TO LOGGED-IN.
           MOVE "Credits:" TO CRED-DISPLAY.
           MOVE "1" TO CRED.
           MOVE 0 TO WS-FOUND.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF USER-NAME = WS-USER-NAME(WS-IDX) AND 
               WS-PASSWORD = WS-PWORD(WS-IDX) THEN
                   MOVE 1 TO WS-FOUND 
               END-IF
               ADD 1 TO WS-IDX 
           END-PERFORM.

           IF WS-FOUND = 1 THEN
               PERFORM 0120-DISPLAY-MENU 
           ELSE 
               PERFORM 0115-ERROR-PAGE 
           END-IF. 
       
       0115-ERROR-PAGE.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" THEN 
               PERFORM 0110-DISPLAY-LOGIN
           ELSE IF ERROR-CHOICE = "c" THEN 
               PERFORM 0105-DISPLAY-REGISTER-NEW-USER 
           ELSE IF ERROR-CHOICE = "q" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE 
               PERFORM 0115-ERROR-PAGE 
           END-IF.
       
       0116-ADMIN-LOGIN-PAGE.
           PERFORM 0500-TIME-AND-DATE.
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
           INITIALIZE ADMIN-NAME.
           INITIALIZE ADMIN-PASSWORD.
           DISPLAY ADMIN-LOGIN-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT ADMIN-NAME-FIELD.
           ACCEPT ADMIN-PASSWORD-FIELD.
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

           IF WS-FOUND = 1 THEN
               PERFORM 0118-DISPLAY-ADMIN-MENU 
           ELSE 
               PERFORM 0117-ADMIN-ERROR-PAGE 
           END-IF. 

       0117-ADMIN-ERROR-PAGE.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ADMIN-ERROR.
           DISPLAY ADMIN-ERROR-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT ADMIN-ERROR-FIELD.
           IF ADMIN-ERROR = "a" THEN 
               PERFORM 0116-ADMIN-LOGIN-PAGE 
           ELSE IF ADMIN-ERROR = "q" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE 
               PERFORM 0117-ADMIN-ERROR-PAGE 
           END-IF.

       0118-DISPLAY-ADMIN-MENU.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ADMIN-CHOICE.
           DISPLAY ADMIN-MENU-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT ADMIN-CHOICE-FIELD.
           IF ADMIN-CHOICE = "q" or "Q" THEN
             STOP RUN
           ELSE IF ADMIN-CHOICE = "l" or "L" THEN
             PERFORM 0100-DISPLAY-START
      *     Add other menu options for administrator here *
           ELSE 
             PERFORM 0118-DISPLAY-ADMIN-MENU
           END-IF.


       0120-DISPLAY-MENU.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" or "Q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" or "L" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "m" or "M" THEN
             PERFORM 0130-MSG-MENU
           ELSE IF MENU-CHOICE = "f" or "F" THEN
             PERFORM 0160-GAMES-MENU
           END-IF.

           PERFORM 0120-DISPLAY-MENU.

       0130-MSG-MENU.
           PERFORM 0500-TIME-AND-DATE.
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
           SORT WS-LIST-ENTRY ON ASCENDING LIST-ID.
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           MOVE MSG-MENU-CHOICE TO MSG-SELECT.
         
           IF MSG-SELECT > 0 THEN
             PERFORM 0140-MESSAGE-VIEW
           END-IF. 
           IF MSG-MENU-CHOICE = "g" OR 'G' THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MSG-MENU-CHOICE = "n" OR 'N' THEN
             COMPUTE ID-NUM = ID-NUM + 10
               IF ID-NUM IS GREATER THAN OR EQUAL TO NUM-FILE-LINES
                 COMPUTE ID-NUM = ID-NUM - 10
                 PERFORM 0130-MSG-MENU
               ELSE
                   PERFORM 0130-MSG-MENU
               END-IF               
               
           ELSE IF MSG-MENU-CHOICE = 'p' OR 'P' THEN
             COMPUTE ID-NUM = ID-NUM - 10
               
               IF ID-NUM IS LESS THAN 10
                   MOVE 1 TO ID-NUM
                    PERFORM 0130-MSG-MENU
               ELSE
                    PERFORM 0130-MSG-MENU
               END-IF
           ELSE IF MSG-MENU-CHOICE = 'w' OR 'W'
             PERFORM 0150-MESSAGE-WRITE
              
           ELSE IF MSG-MENU-CHOICE = 'q' OR 'Q' THEN
              STOP RUN  
           END-IF.

           PERFORM 0130-MSG-MENU.

       0140-MESSAGE-VIEW. 
           PERFORM 0500-TIME-AND-DATE.          
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           DISPLAY USER-INFO-SCREEN.
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
           DISPLAY USER-INFO-SCREEN.
           
           ACCEPT WS-TITLE-FIELD.
           ACCEPT LINE-1-FIELD.
           ACCEPT LINE-2-FIELD.
           ACCEPT LINE-3-FIELD.
           ACCEPT LINE-4-FIELD.
           ACCEPT LINE-5-FIELD.
           
           MOVE WS-CONTENT-DISPLAY TO WS-CONTENT.
           MOVE USER-NAME TO WS-USERNAME.

           IF WS-TITLE-FIELD NOT = SPACE AND LOW-VALUE THEN
             CALL 'post-message' USING NEW-MESSAGE
             PERFORM 0130-MSG-MENU
           END-IF.

           PERFORM 0120-DISPLAY-MENU.
       
       0160-GAMES-MENU.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE GAMES-MENU-CHOICE.
           DISPLAY GAMES-MENU-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF GAMES-MENU-CHOICE = "o" OR "O" THEN
               PERFORM 0190-O-AND-X-GAME  
           ELSE IF GAMES-MENU-CHOICE = "n" or "N" THEN 
               PERFORM 0210-RANDOM-NUMBER-GAME           
           END-IF.

           PERFORM 0160-GAMES-MENU.


           *>----- X AND O Procedure Div------    
       0190-O-AND-X-GAME.
           MOVE "X" TO WS-PLAYER
           PERFORM GAME-LOOP-PARAGRAPH
               WITH TEST AFTER UNTIL FINISHED-PLAYING
           PERFORM 0160-GAMES-MENU.

           GAME-LOOP-PARAGRAPH.
               INITIALIZE WS-GAME-GRID
               INITIALIZE WS-STATE
               INITIALIZE WS-MOVES
               MOVE "Make a move like 'A2'" TO WS-OANDXMESSAGE
               PERFORM GAME-FRAME-PARAGRAPH
                   WITH TEST AFTER UNTIL GAME-OVER
               ADD 1 TO WS-GAMES END-ADD
               EVALUATE WS-STATE
               WHEN "WIN"
                   ADD 1 TO WS-WINS END-ADD
                   MOVE WS-COLOR-BLACK TO WS-FG
                   MOVE WS-COLOR-BLACK TO WS-FG-CELL
                   MOVE WS-COLOR-GREEN TO WS-BG
               WHEN "STALE"
                   MOVE WS-COLOR-BLACK TO WS-FG
                   MOVE WS-COLOR-BLACK TO WS-FG-CELL
                   MOVE WS-COLOR-BLUE TO WS-BG
               WHEN OTHER
                   MOVE WS-COLOR-BLACK TO WS-FG
                   MOVE WS-COLOR-BLACK TO WS-FG-CELL
                   MOVE WS-COLOR-RED TO WS-BG
               END-EVALUATE
               MOVE "One more (y/n)? " TO WS-INSTRUCTION
               MOVE "y" TO WS-NEXT-MOVE
               DISPLAY BOARD-SCREEN.
               DISPLAY USER-INFO-SCREEN.
               ACCEPT NEXT-MOVE.
           
           GAME-FRAME-PARAGRAPH.
               MOVE "Move to square: " TO WS-INSTRUCTION
               MOVE WS-COLOR-GREEN TO WS-FG
               MOVE WS-COLOR-WHITE TO WS-FG-CELL
               MOVE WS-COLOR-BLACK TO WS-BG
               INITIALIZE WS-MOVE-OUTCOME
               IF COMPUTER-PLAYER
                   INITIALIZE WS-COMPUTER-MOVED
                   PERFORM UNTIL COMPUTER-MOVED
                       COMPUTE WS-ROW = FUNCTION RANDOM * 3 + 1
                       END-COMPUTE
                       COMPUTE WS-COL = FUNCTION RANDOM * 3 + 1
                       END-COMPUTE
                       IF WS-CELL(WS-ROW,WS-COL) IS EQUAL TO " "
                       THEN
                           SET WS-COMPUTER-MOVED TO 1
                           MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
                       END-IF
                   END-PERFORM
               ELSE
                   INITIALIZE WS-NEXT-MOVE
                   DISPLAY BOARD-SCREEN
                   DISPLAY USER-INFO-SCREEN
                   ACCEPT NEXT-MOVE
                   EVALUATE FUNCTION UPPER-CASE(WS-NEXT-MOVE(1:1))
                       WHEN "A" SET WS-ROW TO 1
                       WHEN "B" SET WS-ROW TO 2
                       WHEN "C" SET WS-ROW TO 3
                       WHEN OTHER MOVE "FAIL" TO WS-MOVE-OUTCOME
                   END-EVALUATE
                   SET WS-COL TO WS-NEXT-MOVE(2:1)
                   IF
                       WS-MOVE-OUTCOME IS NOT EQUAL TO "FAIL"
                       AND WS-COL IS GREATER THAN 0
                       AND WS-COL IS LESS THAN 4
                       AND WS-CELL(WS-ROW,WS-COL) = " "
                   THEN
                       MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
                   ELSE
                       MOVE "FAIL" TO WS-MOVE-OUTCOME
                   END-IF
               END-IF
               MOVE WS-GAME-GRID TO WS-FLAT-GAME-GRID
               IF HUMAN-PLAYER
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "1"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "0"
               ELSE
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "0"
                   INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "1"
               END-IF
               INSPECT WS-FLAT-GAME-GRID REPLACING ALL " " BY "0"
               INITIALIZE WS-EOF
               OPEN INPUT FD-WINMASKS
               PERFORM UNTIL EOF OR MOVE-COMPLETE
                   READ FD-WINMASKS NEXT RECORD
                       AT END
                           SET WS-EOF TO 1
                       NOT AT END
                           PERFORM VALIDATE-WIN-PARAGRAPH
                   END-READ
               END-PERFORM
               CLOSE FD-WINMASKS
               IF NOT MOVE-COMPLETE AND WS-MOVES IS EQUAL TO 8
                   MOVE "STALE" TO WS-MOVE-OUTCOME
               END-IF
               INITIALIZE WS-SWAP-PLAYERS
               EVALUATE WS-MOVE-OUTCOME
               WHEN "WIN"
                   MOVE "WINNER! (^_^)" TO WS-OANDXMESSAGE
                   MOVE "WIN" TO WS-STATE
                   SET WS-SWAP-PLAYERS TO 1
               WHEN "LOSE"
                   MOVE "YOU DIED (x_x)" TO WS-OANDXMESSAGE
                   MOVE "LOSE" TO WS-STATE
                   SET WS-SWAP-PLAYERS TO 1
               WHEN "STALE"
                   MOVE "Stalemate! (>_<)" TO WS-OANDXMESSAGE
                   MOVE "STALE" TO WS-STATE
               WHEN "FAIL"
                   MOVE "Invalid move... (o_O)" TO WS-OANDXMESSAGE
               WHEN OTHER
                   MOVE "Enter a move" TO WS-OANDXMESSAGE
                   SET WS-SWAP-PLAYERS TO 1
                   ADD 1 TO WS-MOVES END-ADD
               END-EVALUATE
               IF SWAP-PLAYERS
                   IF HUMAN-PLAYER
                       MOVE "O" TO WS-PLAYER
                   ELSE
                       MOVE "X" TO WS-PLAYER
                   END-IF
               END-IF.

           VALIDATE-WIN-PARAGRAPH.
               INITIALIZE WS-MASK-DETECTED
               SET WS-DETECT-LOOP-COUNT TO 1
               PERFORM 9 TIMES
                   IF
                       FD-WINMASK(WS-DETECT-LOOP-COUNT:1)
                       IS EQUAL TO
                       WS-FLAT-GAME-GRID(WS-DETECT-LOOP-COUNT:1)
                       AND IS EQUAL TO 1
                   THEN
                       ADD 1 TO WS-MASK-DETECTED END-ADD
                   END-IF
                   ADD 1 TO WS-DETECT-LOOP-COUNT END-ADD
               END-PERFORM
               IF WIN-DETECTED
                   IF HUMAN-PLAYER
                       MOVE "WIN" TO WS-MOVE-OUTCOME
                   ELSE
                       MOVE "LOSE" TO WS-MOVE-OUTCOME
                   END-IF
               END-IF.

       0210-RANDOM-NUMBER-GAME.
           COMPUTE TOTAL-GUESSES = 0.
           ACCEPT SEED FROM TIME
           COMPUTE ANSWER =
               FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1   
           MOVE "Guess a number between 1 and 10!" TO WS-RANDOM-NUM-MSG.
           PERFORM GAME-LOOP.

           GAME-LOOP.
           INITIALIZE GUESS-INPUT.
           DISPLAY GUESS-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT GUESS-FIELD.
           MOVE GUESS-INPUT TO GUESS.
           ADD 1 TO TOTAL-GUESSES.
           IF GUESS > ANSWER
               MOVE "Your guess is too high! Guess again." 
               TO WS-RANDOM-NUM-MSG
               GO TO GAME-LOOP
           ELSE IF GUESS < ANSWER
               MOVE "Your guess is too low! Guess again."
               TO WS-RANDOM-NUM-MSG
               GO TO GAME-LOOP
           ELSE   
               MOVE "You Win! Go Again?(Y/N)"
               TO WS-RANDOM-NUM-MSG
               GO TO WIN-LOOP
           END-IF.
           
           WIN-LOOP.
           INITIALIZE GUESS-INPUT.
           DISPLAY GUESS-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           ACCEPT GUESS-FIELD.
               IF GUESS-INPUT = "y" OR "Y"
                   GO TO 0210-RANDOM-NUMBER-GAME
               ELSE IF GUESS-INPUT = "n" OR "N"
                   PERFORM 0160-GAMES-MENU
               ELSE 
                   MOVE "INVALID ENTRY! Enter Y or N"
                   TO WS-RANDOM-NUM-MSG
                   GO TO WIN-LOOP
               END-IF.     

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME. 
