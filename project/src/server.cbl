       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           *>----- X AND O File Control-----    
             SELECT FD-WINMASKS ASSIGN TO "PLACEMENT.DAT"
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           *>----- X AND O F-Section-----   
           FD FD-WINMASKS.
           01 FD-WINMASK PIC X(9).
           
           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 USER-NAME PIC X(16).
           01 MENU-CHOICE PIC X.
           01 MSG-MENU-CHOICE PIC XXX.
           01 GAMES-MENU-CHOICE PIC X.
           01 MONKEY-MENU-CHOICE PIC X.
           01 HIDDEN-MENU-CHOICE PIC X.
           01 WS-COUNTER PIC 99.
           01 NUM-FILE-LINES PIC 999.
           01 ID-NUM PIC 999 VALUE 1.
           01 WS-DATETIME PIC X(21).
           01 WS-FORMATTED-DT.
             05 WS-FORMATTED-DTE-TME.
               15 WS-FORMATTED-YEAR  PIC  X(4). 
               15 FILLER             PIC X VALUE '-'.
               15 WS-FORMATTED-MONTH PIC  X(2).
               15 FILLER             PIC X VALUE '-'.
               15 WS-FORMATTED-DY    PIC  X(2).
               15 FILLER             PIC X VALUE '-'.
               15 WS-FORMATTED-HOUR  PIC  X(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-MINS  PIC  X(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-SEC   PIC  X(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-MS    PIC  X(2).
                   
           01 WS-LIST-TABLE.
               05 WS-LIST-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON 
                 NUM-FILE-LINES.
                   10 LIST-ID PIC XXX.
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
      *    --------Library Section---------
           01 LIBRARY-CHOICE PIC X.     

           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

           SCREEN SECTION.
           01 LOGIN-SCREEN
                 BACKGROUND-COLOR IS 0.
                 05 BLANK SCREEN.
                 05 LINE 2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
                 05 LINE 2 COL 4 VALUE ":".
                 05 LINE 2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
                 05 LINE 4 COL 12 VALUE "MAKERS BBS" UNDERLINE, BLINK
                 HIGHLIGHT, FOREGROUND-COLOR IS 3.
                 05 LINE 08 COl 12 VALUE
           "COBOL The Barbarian presents:".                       
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
                 05 LINE 27 COL 14 VALUE "What's your name?".
                 05 USER-NAME-FIELD LINE 29 COL 14 PIC X(16)
                    USING USER-NAME.
                              

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 0.
             05 BLANK SCREEN.
             05 BLANK SCREEN.
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.  
             05 LINE  4 COL 10 VALUE "MAKERS BBS" UNDERLINE.
             05 LINE  6 COL 10 VALUE "Hi, ".
             05 LINE  6 COL 14 PIC X(16) USING USER-NAME.
             05 LINE  8 COL 10 VALUE "Welcome to COBOL The Barbarian's s
      -      "tate of the art Bulletin Board.".  
             05 LINE  9 COL 10 VALUE "Feel free to:".
             05 LINE 10 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 26 VALUE "Read our message board.".
             05 LINE 11 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 11 COL 26 VALUE "Play a few games.".
             05 LINE 12 COL 24 VALUE "* " FOREGROUND-COLOR IS 2.
             05 LINE 12 COL 26 VALUE "Leave a message of your own.". 
             05 LINE 13 COL 24 VALUE "* " FOREGROUND-COLOR IS 5.
             05 LINE 13 COL 26 VALUE "Most importantly. HAVE FUN!". 
             
             05 LINE 19 COL 60 VALUE "(b) Library     "
                REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 5.
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
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
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
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
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
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
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
             05 LINE  2 COL 2 PIC X(2) USING WS-FORMATTED-HOUR.
             05 LINE  2 COL 4 VALUE ":".
             05 LINE  2 COL 5 PIC X(2) USING WS-FORMATTED-MINS.
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

           01 MONKEY-MENU-SCREEN
             BACKGROUND-COLOR IS 0  BLINK.
             05 BLANK SCREEN.
             05 LINE  5 COL 10 VALUE "               __,__"
             FOREGROUND-COLOR IS 2.
             05 LINE  6 COL 10 VALUE "      .--.  .-'     '-.  .--."
             FOREGROUND-COLOR IS 2.
             05 LINE  7 COL 10 VALUE "     / .. \/  .-. .-.  \/ .. \"
             FOREGROUND-COLOR IS 2.
             05 LINE  8 COL 10 VALUE "     | |  '|  /   Y   \  |'  | "
             FOREGROUND-COLOR IS 3.
             05 LINE  9 COL 10 VALUE "     | \   \  \ 0 | 0 /  /   / "
             FOREGROUND-COLOR IS 3.
             05 LINE 10 COL 10 VALUE "     \ '- ,\.-'`` ``'-./, -' /"
             FOREGROUND-COLOR IS 3.
             05 LINE 11 COL 10 VALUE "      `'-' /_   ^ ^   _\ '-'`"
             FOREGROUND-COLOR IS 5.
             05 LINE 12 COL 10 VALUE "      .--'|  \._ _ _./  |'--."
             FOREGROUND-COLOR IS 5.
             05 LINE 13 COL 10 VALUE "     /`    \   \.-.  /   /    `\"
             FOREGROUND-COLOR IS 5.
             05 LINE 14 COL 10 VALUE "    /       '._/  |-' _.'       
      -          "\" FOREGROUND-COLOR IS 5.
             05 LINE 15 COL 10 VALUE "   /          ;  /--~'   |       
      -      "\" FOREGROUND-COLOR IS 6.
             05 LINE 16 COL 10 VALUE "  /        .'\|.-\--.     \       
      -      "\" FOREGROUND-COLOR IS 6.
             05 LINE 17 COL 10 VALUE " /   .'-. /.-.;\  |\|'~'-.|\      
      -       "\" FOREGROUND-COLOR IS 6.
             05 LINE 17 COL 10 VALUE " \       `-./`|_\_/ `     `\'.    
      -        "\" FOREGROUND-COLOR IS 2.
             05 LINE 18 COL 10 VALUE "  '.      ;     ___)        '.`;  
      -        "/" FOREGROUND-COLOR IS 2.
             05 LINE 19 COL 10 VALUE "    '-.,_ ;     ___)          \/  
      -       "/" FOREGROUND-COLOR IS 2.
             05 LINE 20 COL 10 VALUE "     \   ``'------'\       \   `  
      -      "/" FOREGROUND-COLOR IS 3.
             05 LINE 21 COL 10 VALUE "      '.    \       '.      |   ;/
      -      "_" FOREGROUND-COLOR IS 3.
             05 LINE 22 COL 10 VALUE "    ___>     '.       \_ _ _/   , 
      -       "'--." FOREGROUND-COLOR IS 3.
             05 LINE 23 COL 10 VALUE "  .'   '.   .-~~~~~-. /     |--'`~
      -      "~-.  \" FOREGROUND-COLOR IS 5.
             05 LINE 24 COL 10 VALUE " // / .---'/  .-~~-._/ / / /---.._
      -      "_.'  /" FOREGROUND-COLOR IS 5.
             05 LINE 25 COL 10 VALUE " (_(_/    /  /      (_(_(_(---.__ 
      -      ".'  /" FOREGROUND-COLOR IS 5.
             05 LINE 26 COL 10 VALUE "          | |     _              `
      -      "~~`" FOREGROUND-COLOR IS 2.
             05 LINE 27 COL 10 VALUE "          | |     \'."
             FOREGROUND-COLOR IS 2.
             05 LINE 28 COL 10 VALUE "           \ '....' |"
             FOREGROUND-COLOR IS 2.
             05 LINE 29 COL 10 VALUE "            '.,___.'"
             FOREGROUND-COLOR IS 2.
     
             05 LINE 34 COL 10 VALUE "(q)    Quit"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 36 COL 10 VALUE "Pick: ".
             05 MONKEY-MENU-CHOICE-FIELD LINE 36 COL 16 PIC X
                USING MONKEY-MENU-CHOICE.   

           01 HIDDEN-MENU-SCREEN
             BACKGROUND-COLOR IS 0  BLINK.
               05 BLANK SCREEN.
               05 LINE 1 COL 10 VALUE "---------------------------------
      -      "---------------------------------" FOREGROUND-COLOR IS 2.
               05 LINE 2 COL 10 VALUE "---------------------------------
      -      "---------------------------------" FOREGROUND-COLOR IS 3.
               05 LINE 3 COl 10 VALUE  " __  __     ______     __  __
      -         "  ______     __  __     ______"  FOREGROUND-COLOR IS 5.
                 05 LINE 4 COl 10 VALUE "/\ \_\ \   /\  __ \   /\ \_\ \
      -           "  /\  __ \   /\ \_\ \   /\  __ \"
                 FOREGROUND-COLOR IS 2.
                 05 LINE 5 COl 10 VALUE "\ \  __ \  \ \  __ \  \ \  __ \
      -           "  \ \  __ \  \ \  __ \  \ \  __ \"
                 FOREGROUND-COLOR IS 3.
                 05 LINE 6 COl 10 VALUE " \ \_\ \_\  \ \_\ \_\  \ \_\ \_
      -           "\  \ \_\ \_\  \ \_\ \_\  \ \_\ \_\"
                 FOREGROUND-COLOR IS 5.
                 05 LINE 7 COl 10 VALUE "  \/_/\/_/   \/_/\/_/   \/_/\/_
      -          "/   \/_/\/_/   \/_/\/_/   \/_/\/_/"
                 FOREGROUND-COLOR IS 6.
                 05 LINE 9 COL 10 VALUE "-------------------------------
      -      "----------------------------------" FOREGROUND-COLOR IS 2.
               05 LINE 10 COL 10 VALUE "--------------------------------
      -      "---------------------------------" FOREGROUND-COLOR IS 3.
               05 LINE 12 COL 15 VALUE "_                         _"
                    FOREGROUND-COLOR IS 2.
               05 LINE 13 COL 10 VALUE "    |_|                       |_
      -        "|" FOREGROUND-COLOR IS 2.
              05 LINE 14 COL 11 VALUE "   | |         /^^^\         | |"
                FOREGROUND-COLOR IS 3.
              05 LINE 15 COL 13 VALUE  "_| |_      (| 'o' |)      _| |_"
                   FOREGROUND-COLOR IS 3.
              05 LINE 16 COL 10 VALUE " _| | | | _    (_---_)    _ | | |
      -         " |_" FOREGROUND-COLOR IS 5.
              05 LINE 17 COL 10 VALUE "| | | | |' |    _| |_    | `| | |
      -        " | |" FOREGROUND-COLOR IS 5.
               05 LINE 18 COL 10 VALUE "\          /   /     \   \
      -         "   /" FOREGROUND-COLOR IS 6.
               05 LINE 19 COL 11 VALUE "\        /  / /(. .)\ \  \
      -         " /" FOREGROUND-COLOR IS 6.
              05 LINE 20 COL 12 VALUE " \    /  / /  | . |  \ \  \    /"
                   FOREGROUND-COLOR IS 2.
              05 LINE 21 COL 10 VALUE "     \  \/ /    ||Y||    \ \/  /"
                   FOREGROUND-COLOR IS 2.
                05 LINE 22 COL 10 VALUE "       \_/      || ||      \_/"
                   FOREGROUND-COLOR IS 3.
                05 LINE 23 COL 10 VALUE "                () ()"
                   FOREGROUND-COLOR IS 3.
                05 LINE 24 COL 10 VALUE "                || ||"
                   FOREGROUND-COLOR IS 5.
                05 LINE 25 COL 10 VALUE "               ooO Ooo"
                   FOREGROUND-COLOR IS 5.

             05 LINE 30 COL 10 VALUE "(q)    Quit"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 32 COL 10 VALUE "Pick: ".
             05 HIDDEN-MENU-CHOICE-FIELD LINE 32 COL 16 PIC X
                USING HIDDEN-MENU-CHOICE.  

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

           01 LIBRARY-SCREEN.
           05 BLANK SCREEN.
              05 LINE 2 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 3 COL 10 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 4 COL 10 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               05 LINE 5 COL 10 VALUE 
               "           __...--~~~~~-._   _.-~~~~~--...__" 
                 FOREGROUND-COLOR IS 3.
               05 LINE 6 COL 10 VALUE 
               "         //               `V'               \\ "
               FOREGROUND-COLOR IS 3.
               05 LINE 7 COL 10 VALUE 
               "        //                 |                 \\ " 
                 FOREGROUND-COLOR IS 3.
               05 LINE 8 COL 10 VALUE
               "       //__...--~~~~~~-._  |  _.-~~~~~~--...__\\ "
                 FOREGROUND-COLOR IS 3.
               05 LINE 9 COL 10 VALUE 
               "      //__.....----~~~~._\ | /_.~~~~----.....__\\"
                 FOREGROUND-COLOR IS 3.
               05 LINE 10 COL 10 VALUE
               "     ====================\\|//===================="
                 FOREGROUND-COLOR IS 3.
               05 LINE 11 COL 10 VALUE 
               "                         `---`"
                 FOREGROUND-COLOR IS 3.
               05 LIBRARY-FIELD LINE 30 COLUMN 14 PIC X 
               USING LIBRARY-CHOICE.
               05 LINE 12 COL 10 VALUE 
           "---------------------------------------------------------".
               05 LINE 13 COL 27 VALUE
               "WELCOME TO THE LIBRARY".
               05 LINE 14 COL 10 VALUE
           "---------------------------------------------------------"
             .
               05 LINE 15 COL 10 VALUE "||   AUTHOR   ||".
               05 LINE 15 COL 24 VALUE 
               "||                  TITLE                ||".
           


       PROCEDURE DIVISION.

       0110-DISPLAY-LOGIN.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0120-DISPLAY-MENU.

       0120-DISPLAY-MENU.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" or "Q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" or "L" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "m" or "M" THEN
             PERFORM 0130-MSG-MENU
           ELSE IF MENU-CHOICE = "f" or "F" THEN
             PERFORM 0160-GAMES-MENU
           ELSE IF MENU-CHOICE = "b" or "B" THEN
             PERFORM 0220-LIBRARY-MENU
           END-IF.
      
           PERFORM 0120-DISPLAY-MENU.

       0130-MSG-MENU.
           PERFORM 0200-TIME-AND-DATE.
           CALL 'number-of-file-lines' USING NUM-FILE-LINES.
           CALL 'get-list-page-alt' USING NUM-FILE-LINES WS-LIST-TABLE.
          *>  CALL 'id-sort' USING WS-LIST-TABLE.
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
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
           PERFORM 0200-TIME-AND-DATE.          
           MOVE LIST-CONTENT(MSG-SELECT) TO WS-CONTENT-DISPLAY.
           INITIALIZE MSG-VIEW-CHOICE.
           DISPLAY MESSAGE-VIEW-SCREEN.
           ACCEPT MSG-VIEW-CHOICE-FIELD.
           IF MSG-VIEW-CHOICE = 'g' OR 'G' THEN
               PERFORM 0130-MSG-MENU
           ELSE IF MSG-VIEW-CHOICE = 'q' OR 'Q' THEN
              STOP RUN  
           END-IF.
           
           PERFORM 0140-MESSAGE-VIEW. 

       0150-MESSAGE-WRITE.
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE WS-TITLE.
           INITIALIZE LS-PART-1.
           INITIALIZE LS-PART-2.
           INITIALIZE LS-PART-3.
           INITIALIZE LS-PART-4.
           INITIALIZE LS-PART-5.
           DISPLAY WRITE-MSG-SCREEN.
           
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
           PERFORM 0200-TIME-AND-DATE.
           INITIALIZE GAMES-MENU-CHOICE.
           DISPLAY GAMES-MENU-SCREEN.
           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF GAMES-MENU-CHOICE = "o" OR "O" THEN
               PERFORM 0190-O-AND-X-GAME  
           ELSE IF GAMES-MENU-CHOICE = "m" or "M" THEN
               PERFORM 0170-MONKEY-MENU
           ELSE IF GAMES-MENU-CHOICE = "n" or "N" THEN 
               PERFORM 0210-RANDOM-NUMBER-GAME           
           END-IF.

           PERFORM 0160-GAMES-MENU.

       0170-MONKEY-MENU.
           INITIALIZE MONKEY-MENU-CHOICE.
           DISPLAY MONKEY-MENU-SCREEN.
           ACCEPT MONKEY-MENU-CHOICE-FIELD.
           IF MONKEY-MENU-CHOICE = "q" or "Q" THEN
             PERFORM 0180-HIDDEN-MENU
           END-IF.

           PERFORM 0170-MONKEY-MENU.

       0180-HIDDEN-MENU.
           INITIALIZE HIDDEN-MENU-CHOICE.
           DISPLAY HIDDEN-MENU-SCREEN.
           ACCEPT HIDDEN-MENU-CHOICE-FIELD.
           IF HIDDEN-MENU-CHOICE = "j" or "J" THEN
             STOP RUN
           ELSE IF HIDDEN-MENU-CHOICE = "q" or "Q" THEN
             PERFORM 0180-HIDDEN-MENU
           END-IF.

           PERFORM 0180-HIDDEN-MENU.

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
               DISPLAY BOARD-SCREEN END-DISPLAY
               ACCEPT BOARD-SCREEN END-ACCEPT.
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
                   DISPLAY BOARD-SCREEN END-DISPLAY
                   ACCEPT BOARD-SCREEN END-ACCEPT
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

       0200-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME. 
           MOVE WS-DATETIME(1:4)  TO WS-FORMATTED-YEAR.
           MOVE WS-DATETIME(5:2)  TO WS-FORMATTED-MONTH.
           MOVE WS-DATETIME(7:2)  TO WS-FORMATTED-DY.
           MOVE WS-DATETIME(9:2)  TO WS-FORMATTED-HOUR.
           MOVE WS-DATETIME(11:2) TO WS-FORMATTED-MINS.
           MOVE WS-DATETIME(13:2) TO WS-FORMATTED-SEC.
           MOVE WS-DATETIME(15:2) TO WS-FORMATTED-MS.

       0210-RANDOM-NUMBER-GAME.
           PERFORM INITIALIZE-RANDOM-NUM-GAME.

           INITIALIZE-RANDOM-NUM-GAME.
           DISPLAY GUESS-SCREEN.
           COMPUTE TOTAL-GUESSES = 0.
           ACCEPT SEED FROM TIME
           COMPUTE ANSWER =
               FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1   
           MOVE "Guess a number between 1 and 10!" TO WS-RANDOM-NUM-MSG    
           PERFORM GAME-LOOP.
       
           GAME-LOOP.
           INITIALIZE GUESS-INPUT.
           DISPLAY GUESS-SCREEN END-DISPLAY
           ACCEPT GUESS-SCREEN END-ACCEPT
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
           DISPLAY GUESS-SCREEN END-DISPLAY
           ACCEPT GUESS-SCREEN END-ACCEPT
               IF GUESS-INPUT = "y" OR "Y"
                   GO TO INITIALIZE-RANDOM-NUM-GAME
               ELSE IF GUESS-INPUT = "n" OR "N"
                   PERFORM 0160-GAMES-MENU
               ELSE 
                   MOVE "INVALID ENTRY! Enter Y or N"
                   TO WS-RANDOM-NUM-MSG
                   GO TO WIN-LOOP
               END-IF.     
           
       0220-LIBRARY-MENU.
           INITIALIZE LIBRARY-CHOICE.
           DISPLAY LIBRARY-SCREEN.
           ACCEPT LIBRARY-CHOICE.
           
