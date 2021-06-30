       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
              *>  FUNCTION DISPLAY-LIBRARY-TITLE
              *>  FUNCTION LIBRARY-CHOICE-TO-NUM
              *>  FUNCTION DISPLAY-BOOK-BODY
              *>  FUNCTION DISPLAY-BOOK-AUTHOR

              *>  FUNCTION DISPLAY-ABOUT-TITLE
              *>  FUNCTION DISPLAY-ABOUT-BODY
              *>  FUNCTION ABOUT-CHOICE-TO-NUM

              *>  FUNCTION HIGH-SCORE-CALCULATOR
              *>  FUNCTION REPLACE-LETTER

               FUNCTION VERIFY-PASSWORD 
               
               FUNCTION CHECK-BALANCE
               FUNCTION CHECK-LIMIT.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           *>----- Hangman file control -----
          *>  SELECT F-WORD-FILE ASSIGN TO 'guessing-words.dat'
          *>    ORGANIZATION IS LINE SEQUENTIAL.
          *>  SELECT F-HIGH-SCORES-FILE ASSIGN TO 'high-scores.dat'
          *>    ORGANIZATION IS LINE SEQUENTIAL.
          
           *>----- X AND O File Control-----    
          *>    SELECT FD-WINMASKS ASSIGN TO "placement.dat"
          *>              ORGANIZATION IS LINE SEQUENTIAL.

           *>------Library Control-----------------------
            *>  SELECT F-LIBRARY-FILE ASSIGN TO "library.dat"
            *>            ORGANIZATION IS LINE SEQUENTIAL.

             SELECT F-USERS-FILE ASSIGN TO 'users.dat'
                 ORGANIZATION IS LINE SEQUENTIAL. 

            *>  SELECT F-ABOUT-FILE ASSIGN TO 'about-page.dat'
            *>      ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION.
           FILE SECTION.
          *>  *>----- Hangman F-Section-----
          *>  FD F-WORD-FILE.
          *>  01 WORD PIC X(20).

          *>  FD F-HIGH-SCORES-FILE.
          *>  01 PLAYER-SCORES.
          *>     05 HIGH-SCORE PIC 99.
          *>     05 PLAYER-NAME PIC X(10).

          *>  *>----- X AND O F-Section-----   
          *>  FD FD-WINMASKS.
          *>  01 FD-WINMASK PIC X(9).

           *>------Library Section------
          *>  FD F-LIBRARY-FILE.
          *>  01 LIBRARY.
          *>      05 FD-BOOK-AUTHOR PIC X(12).
          *>      05 BOOK-TITLE PIC X(30).
          *>      05 BOOK-BODY PIC X(500).
                

           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(8).
              05 FILLER PIC XX VALUE SPACES.  
              05 USER-CREDIT PIC 999. 
      
          *>  FD F-ABOUT-FILE.
          *>  01 ABOUT-INFO.
          *>      05 ABOUT-TITLE PIC X(31).
          *>      05 ABOUT-BODY PIC X(500).

           WORKING-STORAGE SECTION.
           *>----- General Variables -----
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 START-CHOICE PIC X.
           01 WS-COUNTER PIC 99.

           *>----- Login Variables -----

           01 USER-NAME PIC X(16).
           01 WS-PASSWORD PIC X(20).
           01 ACCOUNT-NUM PIC X(8).
           01 CREDIT PIC 999.

           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-USER-NAME
               INDEXED BY USER-IDX.
                   10 WS-USER-NAME PIC X(16).    
                   10 WS-PWORD PIC X(20).
                   10 WS-ACNT-NUM PIC X(8).
                   10 WS-CREDIT PIC 999. 
 
           01 WS-FOUND PIC 9. 
           01 WS-IDX UNSIGNED-INT. 
           01 COUNTER UNSIGNED-INT. 

           01 NEW-USER-NAME PIC X(16).
           01 NEW-PASSWORD PIC X(20).
           01 REGISTER-CHOICE PIC X.
           01 RAISE-ERROR PIC 9.
           01 ERROR-MSG-1 PIC X(50).
           01 ERROR-MSG-2 PIC X(50).
           01 ERROR-MSG-3 PIC X(50).
           01 OK-MSG-1 PIC X(50).
           01 OK-MSG-2 PIC X(50).
           01 OK-MSG-3 PIC X(50).
           01 VALID-CHOICE PIC X.
           01 ERROR-CHOICE PIC X. 

           01 MENU-CHOICE PIC X.

           *>----- User Info Screen Variables ----

           01 USER-INFO-LOGGED-IN PIC X(15) VALUE "Logged in as:".
           01 USER-INFO-NAME PIC X(16).

           01 USER-INFO-CRED-DISPLAY.
               05 USER-INFO-CR-MESSAGE PIC X(9) VALUE "Credits: ".
               05 USER-INFO-CREDITS PIC 999.
         
           *>----- Date and Time-Screen Variables -----

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   


           *>----- Arcade Variables -----

          *>  01 GAMES-MENU-CHOICE PIC X.

           *>-----X AND O WS-SECTION-----  

          *>  01 WS-PLAYER PIC A(1).
          *>          88 HUMAN-PLAYER VALUE "X".
          *>          88 COMPUTER-PLAYER VALUE "O".
          *>      01 WS-STATE PIC A(5).
          *>          88 GAME-OVER VALUES "WIN", "LOSE", "STALE".
          *>      01 WS-MOVE-OUTCOME PIC A(5).
          *>          88 MOVE-COMPLETE VALUES "WIN", "LOSE", "FAIL".
          *>      01 WS-MASK-DETECTED PIC 9(1).
          *>          88 WIN-DETECTED VALUES 3, 4, 5, 6, 7, 8, 9.
          *>      01 WS-COMPUTER-MOVED PIC 9(1).
          *>          88 COMPUTER-MOVED VALUE 1.
          *>      01 WS-EOF PIC 9(1).
          *>          88 EOF VALUE 1.
          *>      01 WS-SWAP-PLAYERS PIC 9(1).
          *>          88 SWAP-PLAYERS VALUE 1.
          *>      01 WS-NEXT-MOVE PIC X(2).
          *>          88 FINISHED-PLAYING VALUES "N", "n".
          *>      01 WS-GAME-GRID.
          *>          05 WS-GAME-GRID-ROW OCCURS 3 TIMES.
          *>              10 WS-GAME-GRID-COL OCCURS 3 TIMES.
          *>                  15 WS-CELL PIC X(1).
          *>      01 WS-COLOR-GREEN PIC 9(1) VALUE 2.
          *>      01 WS-COLOR-BLACK PIC 9(1) VALUE 0.
          *>      01 WS-COLOR-WHITE PIC 9(1) VALUE 7.
          *>      01 WS-COLOR-BLUE PIC 9(1) VALUE 3.
          *>      01 WS-COLOR-RED PIC 9(1) VALUE 4.
          *>      01 WS-FG-CELL PIC 9(1).
          *>      01 WS-FG PIC 9(1).
          *>      01 WS-BG PIC 9(1).
          *>      01 WS-COL PIC 9(1).
          *>      01 WS-ROW PIC 9(1).
          *>      01 WS-WINS PIC 9(2).
          *>      01 WS-MOVES PIC 9(2).
          *>      01 WS-GAMES PIC 9(2).
          *>      01 WS-COMPUTER-MOVE PIC 9(1).
          *>      01 WS-DETECT-LOOP-COUNT PIC 9(1).
          *>      01 WS-OANDXMESSAGE PIC X(128).
          *>      01 WS-INSTRUCTION PIC X(16).
          *>      01 WS-FLAT-GAME-GRID PIC X(9).

          *>-----RANDOM-NUM-GAME WS-SECTION-----

          *>  01 SEED PIC 9(8).
          *>  01 GUESS-INPUT PIC XX.
          *>  01 GUESS PIC 99.
          *>  01 ANSWER PIC 99.
          *>  01 TOTAL-GUESSES PIC 99.
          *>  01 BET-AMOUNT PIC 999.
          *>  01 WINNINGS PIC 999.
          *>  01 RANDOM-NUM-CHOICE PIC X.
          *>  01 RANDOM-NUM-GUESS-CHOICE PIC X.

           *>--------Library Section---------
          *>  01 LIBRARY-CHOICE PIC X(2).
          *>  01 PAGE-NUM PIC 99.
          *>  01 LIBRARY-DISPLAY-MESSAGE PIC X(40).
          *>  01 LIBRARY-NUM UNSIGNED-INT.
          *>  01 TITLE PIC X(30).
          *>  01 BODY PIC X(500).
          *>  01 BOOK-AUTHOR PIC X(12).
          *>  01 WS-BOOKS.
          *>      05 WS-BOOK OCCURS 100 TIMES
          *>      ASCENDING KEY IS WS-BOOK-AUTHOR-NAME
          *>      INDEXED BY BOOK-IDX.
          *>          10 WS-BOOK-AUTHOR-NAME PIC X(12).
          *>          10 WS-BOOK-TITLE PIC X(30).
          *>          10 WS-BODY PIC X(500).
           
          *>  01 OFFSET UNSIGNED-INT.
          *>  01 READ-CHOICE PIC X.     
          *>  01 AUDIOBOOK-MSG PIC X(50).

          *>  01 WS-RANDOM-NUM-MSG PIC X(40).

          *>  01 WS-READ-BODY-SEGMENTS.
          *>      10 WS-READ-BODY-SEGMENT-1 PIC X(60). 
          *>      10 WS-READ-BODY-SEGMENT-2 PIC X(60). 
          *>      10 WS-READ-BODY-SEGMENT-3 PIC X(60). 
          *>      10 WS-READ-BODY-SEGMENT-4 PIC X(60). 
          *>      10 WS-READ-BODY-SEGMENT-5 PIC X(60).
      
           *>----Variables-related-to-guessing-game----
      *>      01 WS-ANSWERWORD PIC X(20).
      *>      01 RANDOMNUMBER PIC 99.
      *>      01 WS-WORD PIC X(20).
      *>      01 WS-GUESSING-CHOICE-WORDS.
      *>          05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
      *>          DESCENDING KEY IS WS-GUESSING-WORDS-WORD
      *>          INDEXED BY WORD-IDX.
      *>              10 WS-GUESSING-WORDS-WORD PIC X(20).
      *>      01 WS-GUESS-CHOICE PIC X(20).

            *>----Variables related to high score screen-----
      *>      01 WS-HIGH-SCORE-CHOICE PIC X.
      *>      01 WS-HIGH-SCORE PIC 99.
      *>      01 WS-HIGH-SCORES.  
      *>         05 WS-TABLE-HIGH-SCORE OCCURS 100 TIMES     
      *>         ASCENDING KEY IS WS-SCORE
      *>         INDEXED BY SCORE-IDX.
      *>             10 WS-SCORE PIC 99.
      *>             10 WS-NAME PIC X(10).

      *    Variables related to checking guesses  
      *>      01 WS-LETTERS-LEFT PIC 99.
      *>      01 WS-GUESSES-LEFT PIC 99.          

      *>     Variables related to winning and losing.
      *>      01 WS-GUESSING-LOSING-CHOICE PIC X.
      *>      01 WS-GUESSING-WINNING-CHOICE PIC X.
      *>      01 WS-WORD-LENGTH PIC 99.

           
           *>----- Weather Variables -----
          *>  01 W1-CHOICE PIC X.
          *>  01 W2-CHOICE PIC X.
          *>  01 W3-CHOICE PIC X.
          *>  01 W4-CHOICE PIC X.

           *>----- Torch Variables -----   
           01 TORCH-CHOICE PIC X. 

           *>----- Buy Credits Variables ----- 
          *>  01 CREDIT-AMOUNT PIC 999.
          *>  01 MON-AMOUNT PIC 999.99.
          *>  01 BUY-CREDITS-CHOICE PIC X.
          *>  01 CONFIRM-CHOICE PIC X.
          *>  01 PAY-CONFIRMATION-CHOICE PIC X.
          *>  01 PASSWORD-ENTRY PIC X(20).
          *>  01 INC-PASSWORD PIC X(20).
          *>  01 CREDIT-LIMIT-MESSAGE PIC X(65).
          *>  01 WS-CURRENT-DATE PIC X(8).

           *>------About Variables-----
          *>  01 ABOUT-PAGE-CHOICE PIC X.
          *>  01 WS-ABOUT. 
          *>      05 WS-ABOUTS OCCURS 100 TIMES 
          *>      ASCENDING KEY IS WS-ABOUT-TITLE
          *>      INDEXED BY ABOUT-IDX.
          *>          10 WS-ABOUT-TITLE PIC X(31).
          *>          10 WS-ABOUT-BODY PIC X(500).

          *>  01 ABOUT-OFFSET PIC 99.
          *>  01 ABOUT-PAGE-NUM PIC 99.
          *>  01 ABOUT-NUM PIC 9.


          *>  01 ABOUT-PAGE-READ-CHOICE PIC X.
          *>  01 ABOUT-TITLE-READ PIC X(31).
          *>  01 ABOUT-BODY-READ PIC X(500).

          *>  01 ABOUT-INVALID-CHOICE-MESSAGE PIC X(15).

           *>------Spending Credits Variables------
           
           01 COST PIC 999.
           01 CREDIT-BALANCE PIC 999.
           01 UPDATED-BALANCE PIC 999.
           01 INSUFFICIENT-FUNDS PIC X(20).

           *>----- Change Password Variables -----  

           01 PWORD-ERR-1 PIC X(50).
           01 PWORD-ERR-2 PIC X(50).
           01 PWORD-ERR-3 PIC X(50).
           01 PWORD-OK-1 PIC X(50).
           01 PWORD-OK-2 PIC X(50).
           01 PWORD-OK-3 PIC X(50).
           01 PWORD-CONFIRM-MSG PIC X(50).
           01 OLD-PASSWORD PIC X(20).
           01 UPDATED-PASSWORD PIC X(20).
           01 CONFIRM-NEW-PASSWORD PIC X(20).
           01 CHANGE-PWORD-CHOICE PIC X. 

           
           *>----- Change Account Num Variables ----- 
           01 PWORD-ERROR PIC X(50).
           01 ACNT-ERR-1 PIC X(50).
           01 ACNT-ERR-2 PIC X(50).
           01 PWORD-OK PIC X(50).
           01 ACNT-OK-1 PIC X(50).
           01 ACNT-OK-2 PIC X(50).
           01 ACNT-CONFIRM-MSG PIC X(50).
           01 CHECK-PASSWORD PIC X(20).
           01 UPDATED-ACNT PIC X(8).
           01 CONFIRM-ACNT PIC X(8).
           01 CHANGE-ACNT-CHOICE PIC X. 


           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

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
                  USER-INFO-CRED-DISPLAY FOREGROUND-COLOR IS 2.
               
           01 START-SCREEN. 
               05 BLANK SCREEN.
               05 LINE 40 COLUMN 30 VALUE "(l) Go to Log-in."
               HIGHLIGHT, FOREGROUND-COLOR IS 3.
               05 LINE 41 COLUMN 30 VALUE "(c) Create an account."
               HIGHLIGHT, FOREGROUND-COLOR IS 3.
               05 LINE 44 COLUMN 30 VALUE "Pick: "
               HIGHLIGHT, FOREGROUND-COLOR IS 2, BLINK.
               05 START-CHOICE-FIELD LINE 44 COLUMN 36 PIC X
               USING START-CHOICE.
               05 LINE 42 COLUMN 30 VALUE "(a) Administrator."
               HIGHLIGHT, FOREGROUND-COLOR IS 3.
               05 LINE 13 COL 30 VALUE
           " ________ ________  ___       ___       ________  ___  ___  
      -    "_________"
           FOREGROUND-COLOR IS 2.
           05 LINE 14 COL 30 VALUE   
           "|\  _____|\   __  \|\  \     |\  \     |\   __  \|\  \|\  \|
      -    "\___   ___\ "
           FOREGROUND-COLOR IS 2.
           05 LINE 15 COL 30 VALUE
           "\ \  \__/\ \  \|\  \ \  \    \ \  \    \ \  \|\  \ \  \\\  \
      -    "|___ \  \_|"
           FOREGROUND-COLOR IS 2.
           05 LINE 16 COL 30 VALUE   
           " \ \   __\\ \   __  \ \  \    \ \  \    \ \  \\\  \ \  \\\  
      -    "\   \ \  \  "
           FOREGROUND-COLOR IS 2.
           05 LINE 17 COL 30 VALUE
           "  \ \  \_| \ \  \ \  \ \  \____\ \  \____\ \  \\\  \ \  \\\ 
      -    " \   \ \  \ "
           FOREGROUND-COLOR IS 2.
           05 LINE 18 COL 30 VALUE
           "   \ \__\   \ \__\ \__\ \_______\ \_______\ \_______\ \_____
      -    "__\   \ \__\"
           FOREGROUND-COLOR IS 2.
           05 LINE 19 COL 30 VALUE
           "    \|__|    \|__|\|__|\|_______|\|_______|\|_______|\|____
      -    "___|    \|__|"
           FOREGROUND-COLOR IS 2.
            05 LINE 44 COLUMN 68 VALUE 
            "                        (` ^''`-' ')"
              FOREGROUND-COLOR IS 2.
            05 LINE 43 COLUMN 68 VALUE 
            "                         <|i::|i|`."
              FOREGROUND-COLOR IS 2.
            05 LINE 42 COLUMN 68 VALUE 
            "                          I;|.|.|"
              FOREGROUND-COLOR IS 2.
            05 LINE 41 COLUMN 68 VALUE 
            "                      ~ `-i' ::>|--'"
              FOREGROUND-COLOR IS 2.
            05 LINE 40 COLUMN 68 VALUE 
            "                   \_( _ <         >_>'"
              FOREGROUND-COLOR IS 2.
            05 LINE 39 COLUMN 68 VALUE 
            "                  (_ (   \  (     >    \)"
              FOREGROUND-COLOR IS 2.
            05 LINE 38 COLUMN 68 VALUE 
            "                  \/  ~'~'~'~'~'~\~'~)~'/"
              FOREGROUND-COLOR IS 2.
            05 LINE 37 COLUMN 68 VALUE 
            "- ------===;;;'====------------------===;;;===----- -  -"
              FOREGROUND-COLOR IS 2.
            05 LINE 36 COLUMN 68 VALUE 
            "               __ /        <    /   )  \___"
              FOREGROUND-COLOR IS 2.
            05 LINE 35 COLUMN 68 VALUE 
            "                   _/_,---(      ,    )"
              FOREGROUND-COLOR IS 2.
            05 LINE 34 COLUMN 68 VALUE 
            "                     __,-~~/~    `---."
              FOREGROUND-COLOR IS 2.
            05 LINE 33 COLUMN 68 VALUE 
            "                            ____"
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

           01 REGISTER-NEW-USER-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.                                           
               05 LINE 11 COL 90 VALUE
               "    _________"
               FOREGROUND-COLOR IS 2.
               05 LINE 10 COL 90 VALUE   
               "    / ======= \"
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 90 VALUE
               "   / __________\"
               FOREGROUND-COLOR IS 2.
               05 LINE 12 COL 90 VALUE   
               "  | ___________ |"
               FOREGROUND-COLOR IS 2.
               05 LINE 13 COL 90 VALUE
               "  | | -       | |"
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 90 VALUE
               "  | |         | |"
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 90 VALUE
               "  | |_________| |"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 90 VALUE
               "  \=____________/"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 90 VALUE
               "  / ''''''''''' \"
                   FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 90 VALUE
               " / ::::::::::::: \"
                   FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 90 VALUE
               "(_________________)"
                   FOREGROUND-COLOR IS 2.
               05 LINE 12 COl 25 VALUE   
               "     _   _             _ _     __  _____ __  " 
                   FOREGROUND-COLOR IS 2.
               05 LINE 13 COl 25 VALUE         
               "    | | | |           | | |   /  ||  _  /  | " 
                   FOREGROUND-COLOR IS 2.
               05 LINE 14 COl 25 VALUE  
               "    | | | | __ _ _   _| | |_  `| || |/' `| | " 
                   FOREGROUND-COLOR IS 2.
               05 LINE 15 COl 25 VALUE    
               "    | | | |/ _` | | | | | __|  | ||  /| || | "
                   FOREGROUND-COLOR IS 2.
               05 LINE 16 COl 25 VALUE   
               "    \ \_/ | (_| | |_| | | |_  _| |\ |_/ _| |_"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COl 25 VALUE  
               "     \___/ \__,_|\__,_|_|\__| \___/\___/\___/" 
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COLUMN 30 VALUE "CREATE AN ACCOUNT" HIGHLIGHT,
               FOREGROUND-COLOR IS 3.
               05 LINE 22 COLUMN 30 VALUE "Creating an account requires 
      -        "a couple of details and then we'll have you all up and "  
               FOREGROUND-COLOR IS 2.
               05 LINE 23 COLUMN 30 VALUE "running on our vault network 
      -        "traveller"  
               FOREGROUND-COLOR IS 2.
               05 LINE 24 COLUMN 30 VALUE "We'll need your account detai
      -        "s for processing credits to explore all features."  
               FOREGROUND-COLOR IS 2.
               05 LINE 26 COLUMN 30 VALUE "Enter a username:"
               FOREGROUND-COLOR IS 2.
               05 LINE 26 COLUMN 50 VALUE " (Usernames must be unique.)"
               FOREGROUND-COLOR IS 2.
               05 LINE 27 COLUMN 30 PIC X(50) USING ERROR-MSG-1 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 NEW-USER-NAME-FIELD LINE 28 COLUMN 30 PIC X(16)
               USING NEW-USER-NAME FOREGROUND-COLOR IS 2.
               05 LINE 29 COLUMN 30 PIC X(50) USING OK-MSG-1 HIGHLIGHT
               FOREGROUND-COLOR is 2.
               05 LINE 30 COLUMN 30 VALUE "Enter a password:"
               FOREGROUND-COLOR IS 2.
               05 LINE 30 COLUMN 50 VALUE " (Your password must be a min
      -        "imum of 6 characters and 1 number.) "
               FOREGROUND-COLOR IS 2.
               05 LINE 31 COLUMN 30 PIC X(50) USING ERROR-MSG-2 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 NEW-PASSWORD-FIELD LINE 32 COLUMN 30 PIC X(20)
               USING NEW-PASSWORD FOREGROUND-COLOR IS 2.
               05 LINE 33 COLUMN 30 PIC X(50) USING OK-MSG-2 HIGHLIGHT
               FOREGROUND-COLOR is 2.
               05 LINE 34 COLUMN 30 VALUE "Enter a valid Bank Account nu
      -        "mber:"
               FOREGROUND-COLOR IS 2.
               05 LINE 35 COLUMN 30 PIC X(50) USING ERROR-MSG-3 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 ACCOUNT-NUM-FIELD LINE 36 COLUMN 30 PIC X(8)
               USING ACCOUNT-NUM FOREGROUND-COLOR IS 2.
               05 LINE 37 COLUMN 30 PIC X(50) USING OK-MSG-3 HIGHLIGHT
               FOREGROUND-COLOR is 2 . 
               05 LINE 39 COLUMN 30 VALUE "(s) Submit" HIGHLIGHT 
               FOREGROUND-COLOR IS 3 .  
               05 LINE 40 COLUMN 30 VALUE "(g) Go Back" HIGHLIGHT 
               FOREGROUND-COLOR IS 3 .    
               05 LINE 42 COLUMN 30 VALUE "Pick:"
               BLINK HIGHLIGHT FOREGROUND-COLOR is 2 . 
               05 REGISTER-CHOICE-FIELD LINE 42 COLUMN 37 PIC X
               USING REGISTER-CHOICE.

           01 LOGIN-SCREEN.
               05 BLANK SCREEN.
               05 LINE 15 COL 90 VALUE
               "    _________"
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 90 VALUE   
               "    / ======= \"
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 90 VALUE
               "   / __________\"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 90 VALUE   
               "  | ___________ |"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 90 VALUE
               "  | | -       | |"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 90 VALUE
               "  | |         | |"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 90 VALUE
               "  | |_________| |"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 90 VALUE
               "  \=____________/"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 90 VALUE
               "  / ''''''''''' \"
               FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 90 VALUE
               " / ::::::::::::: \"
               FOREGROUND-COLOR IS 2.
               05 LINE 23 COL 90 VALUE
               "(_________________)"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COl 25 VALUE   
               "     _   _             _ _     __  _____ __  " 
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COl 25 VALUE         
               "    | | | |           | | |   /  ||  _  /  | " 
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COl 25 VALUE  
               "    | | | | __ _ _   _| | |_  `| || |/' `| | " 
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COl 25 VALUE    
               "    | | | |/ _` | | | | | __|  | ||  /| || | "
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COl 25 VALUE   
               "    \ \_/ | (_| | |_| | | |_  _| |\ |_/ _| |_"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COl 25 VALUE  
               "     \___/ \__,_|\__,_|_|\__| \___/\___/\___/" 
               FOREGROUND-COLOR IS 2.
               05 LINE 27 COL 30 VALUE "Enter your username:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 USER-NAME-FIELD LINE 29 COL 30 PIC X(16)
               USING USER-NAME FOREGROUND-COLOR IS 2.
               05 LINE 31 COL 30 VALUE "Enter your password:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 PASSWORD-FIELD LINE 33 COLUMN 30 PIC X(20)
               USING WS-PASSWORD FOREGROUND-COLOR IS 2.
                              
           01 ERROR-SCREEN.        
               05 LINE 27 COLUMN 30 VALUE "Incorrect Username or Passwor
      -        "d"
               HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 LINE 29 COLUMN 30 VALUE "(l) Back to Log-in."
               FOREGROUND-COLOR is 2 . 
               05 LINE 30 COLUMN 30 VALUE "(c) Create an account."
               FOREGROUND-COLOR is 2 . 
               05 LINE 31 COLUMN 30 VALUE "(g) Go Back.         "
               FOREGROUND-COLOR is 2 . 
               05 LINE 33 COLUMN 30 VALUE "Pick: "
               BLINK HIGHLIGHT FOREGROUND-COLOR is 2 . 
               05 ERROR-CHOICE-FIELD LINE 33 COLUMN 36 PIC X
               USING ERROR-CHOICE BLINK.

           01 MENU-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.
               05 LINE 21 COL 46 VALUE "Hi, "
                  FOREGROUND-COLOR 2.
               05 LINE 21 COL 50 PIC X(16) USING USER-NAME
                  FOREGROUND-COLOR 2.
               05 LINE 23 COL 46 VALUE "Welcome to TMNCT's state of the
      -         "art Bulletin Board."
                  FOREGROUND-COLOR 2.
               05 LINE 24 COL 46 VALUE "Feel free to:"
                  FOREGROUND-COLOR 2.
               05 LINE 26 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 48 VALUE "Read our message board."
                  FOREGROUND-COLOR 2.
               05 LINE 27 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 27 COL 48 VALUE "Play a few games."
                  FOREGROUND-COLOR 2.
               05 LINE 28 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 28 COL 48 VALUE "Leave a message of your own."
                  FOREGROUND-COLOR 2.
               05 LINE 29 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 29 COL 48 VALUE "Most importantly. HAVE FUN!"
                  FOREGROUND-COLOR 2. 
               05 LINE 34 COL 84 VALUE "(b) Library     "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 34 COL 46 VALUE "(m) Messages    "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 34 COL 64 VALUE "(f) Fun & Games "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 34 COL 100 VALUE "(p) Update Password    "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 100 VALUE "(u) Update Account Num "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 46 VALUE "(l) Logout      "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.            
               05 LINE 36 COL 64 VALUE "(c) Buy Credits "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 84 VALUE "(w) Weather     "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 38 COL 46 VALUE "Pick: "
                  BLINK HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 MENU-CHOICE-FIELD LINE 38 COL 53 PIC X
                  USING MENU-CHOICE BLINK.
           

      *>      01 GAMES-MENU-SCREEN
      *>        BACKGROUND-COLOR IS 0.
      *>        05 BLANK SCREEN.

      *>        05 LINE 13 COL 30 VALUE
      *>          " ____"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 14 COL 30 VALUE   
      *>          "|  __ \                         "
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 15 COL 30 VALUE
      *> -        "| |  \/ __ _ _ __ ___   ___ ___ "
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 16 COL 30 VALUE   
      *>          "| | __ / _` | '_ ` _ \ / _ / __|"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 17 COL 30 VALUE
      *>          "| |_\ | (_| | | | | | |  __\__ \"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 18 COL 30 VALUE
      *>          " \____/\__,_|_| |_| |_|\___|___/"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 13 COL 70 VALUE
      *>          "   ,##.                   ,==."
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 14 COL 70 VALUE   
      *>          " ,#    #.                 \ o ',"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 15 COL 70 VALUE
      *> -        "#        #     _     _     \    \"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 16 COL 70 VALUE   
      *>          "#        #    (_)   (_)    /    ; "
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 17 COL 70 VALUE
      *>          " `#    #'                 /   .'  "
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 18 COL 70 VALUE
      *>          "   `##'                   '=='"
      *>          FOREGROUND-COLOR IS 2.

             


      *>        05 LINE 26 COL 40 VALUE "Games cost 5 credits: ".      
      *>        05 LINE 28 COL 43 VALUE "(h) Hangman"
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 30 COL 43 VALUE "(n) Guess The Number" 
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 32 COL 43 VALUE "(o) O and X         "  
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.          
      *>        05 LINE 36 COL 36 VALUE "(g) Go back "
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 36 COL 54 VALUE "(q) Quit    "
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 38 COL 36 VALUE "Pick: "
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3 BLINK.
      *>        05 GAMES-MENU-CHOICE-FIELD LINE 38 COL 41 PIC X
      *>           USING GAMES-MENU-CHOICE.              
      *>         05 LINE 40 COL 36 PIC X(20) USING INSUFFICIENT-FUNDS
      *>           HIGHLIGHT, FOREGROUND-COLOR IS 4.
      
      *>      01 BOARD-SCREEN.
      *>          05 BLANK SCREEN.
      *>          05 LINE 16 COL 50 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>          05 LINE 17 COL 50 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>          05 LINE 18 COL 50 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
      *>          05 LINE 19 COl 60 VALUE  "  ___       _    _   _ ____   __
      *> -        "  __" FOREGROUND-COLOR IS 3.
      *>          05 LINE 20 COl 60 VALUE " / _ \     / \  | \ | |  _ \  \ \
      *> -        "/ /" FOREGROUND-COLOR IS 5.
      *>          05 LINE 21 COl 60 VALUE "| | | |   / _ \ |  \| | | | |  \  
      *> -        " /" FOREGROUND-COLOR IS 3.
      *>          05 LINE 22 COl 60 VALUE "| |_| |  / ___ \| |\  | |_| |  /  
      *> -         " \" FOREGROUND-COLOR IS 2.
      *>          05 LINE 23 COl 60 VALUE " \___/  /_/   \_\_| \_|____/  /_/
      *> -        "\_\" FOREGROUND-COLOR IS 5.
      *>          05 LINE 25 COL 50 VALUE "---------------------------------
      *> -      "----------------------" FOREGROUND-COLOR IS 2.
      *>          05 LINE 26 COL 50 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>          05 LINE 27 COL 50 VALUE "--------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>          05 LINE 28 COLUMN 49 VALUE IS "   +---+---+---+   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 29 COLUMN 49 VALUE IS " A |   |   |   |   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 30 COLUMN 49 VALUE IS "   +---+---+---+   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 31 COLUMN 49 VALUE IS " B |   |   |   |   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 32 COLUMN 49 VALUE IS "   +---+---+---+   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 33 COLUMN 49 VALUE IS " C |   |   |   |   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 34 COLUMN 49 VALUE IS "   +---+---+---+   "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 35 COLUMN 49 VALUE IS "     1   2   3     "
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
      *>          05 LINE 29 COLUMN 54 PIC A(1) FROM WS-CELL(1,1)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 29 COLUMN 58 PIC A(1) FROM WS-CELL(1,2)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 29 COLUMN 62 PIC A(1) FROM WS-CELL(1,3)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 31 COLUMN 54 PIC A(1) FROM WS-CELL(2,1)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 31 COLUMN 58 PIC A(1) FROM WS-CELL(2,2)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 31 COLUMN 62 PIC A(1) FROM WS-CELL(2,3)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 33 COLUMN 54 PIC A(1) FROM WS-CELL(3,1)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 33 COLUMN 58 PIC A(1) FROM WS-CELL(3,2)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
      *>          05 LINE 33 COLUMN 62 PIC A(1) FROM WS-CELL(3,3)
      *>              BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.

      *>          05 LINE 35 COLUMN 49 VALUE IS "Message: "
      *>              FOREGROUND-COLOR IS 6.
      *>              05 MSG PIC X(128) FROM WS-OANDXMESSAGE.
      *>          05 LINE 36 COLUMN 49 PIC X(16) FROM WS-INSTRUCTION.
      *>              05 NEXT-MOVE PIC X(2) USING WS-NEXT-MOVE.
      *>          05 LINE 37 COLUMN 49 VALUE IS "Stats: "
      *>              FOREGROUND-COLOR IS 6.
      *>          05 LINE 38 COLUMN 49 VALUE IS "Moves played = "
      *>              FOREGROUND-COLOR IS 2.
      *>              05 MOVES PIC 9(1) FROM WS-MOVES.
      *>          05 LINE 39 COLUMN 49 VALUE IS "Games won = "
      *>              FOREGROUND-COLOR IS 5.
      *>              05 WINS PIC 9(2) FROM WS-WINS.
      *>          05 LINE 39 COLUMN 63 VALUE IS "/".
      *>              05 GAMES PIC 9(2) FROM WS-GAMES. 
      *>         05 LINE 41 COL 50 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>         05 LINE 42 COL 50 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>         05 LINE 43 COL 50 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
      
      *>      01 IN-GAME-SCREEN
      *>          BACKGROUND-COLOR IS 8.
      *>        05 BLANK SCREEN. 
      *>        05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 31 COLUMN 30 VALUE "You wander into a small settleme
      *> -      "nt, seeking shelter from the pounding sun of The Wasteland
      *> -      "."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>       05 LINE 32 COLUMN 30 VALUE "The local Lawman mistakes you fo
      *> -      "r a bandit. You're tied up and on the gallows faster"
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 33 COLUMN 30 VALUE "than you can wish the townsfolk 
      *> -      "a friendly wasteland hello."

      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 34 COLUMN 30 VALUE "You've Yee'd your last Haw."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 35 COLUMN 30 VALUE "Guess this word to break free:"
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 36 COLUMN 30 PIC X(20) USING WS-WORD
      *>        HIGHLIGHT FOREGROUND-COLOR 2.
      *>        05 LINE 37 COLUMN 30 VALUE "Guesses left: "
      *>         HIGHLIGHT FOREGROUND-COLOR 3.
      *>        05 LINE 37 COLUMN 60 PIC 99 USING WS-GUESSES-LEFT.
      *>        05 LINE 38 COLUMN 30 VALUE "( ) Enter a letter to guess"
      *>          HIGHLIGHT FOREGROUND-COLOR 3.
      *>        05 LINE 39 COLUMN 30 VALUE "(!) Quit game"
      *>          HIGHLIGHT FOREGROUND-COLOR 3.
      *>        05 LINE 40 COLUMN 30 VALUE "Pick: "
      *>          HIGHLIGHT FOREGROUND-COLOR 3 BLINK.
      *>        05 WS-GUESS-CHOICE-FIELD LINE 40 COLUMN 36 PIC X

      *>          USING WS-GUESS-CHOICE.
      *>           05 LINE 13 COL 70 VALUE
      *>          " ___________.._______"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 14 COL 70 VALUE   
      *>          "| .__________))______|"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 15 COL 70 VALUE
      *> -        "| | / /      ||"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 16 COL 70 VALUE   
      *>          "| |/ /       ||"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 17 COL 70 VALUE
      *>          "| | /        ||.-''."
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 18 COL 70 VALUE
      *>          "| |/         |/  _  \"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 19 COL 70 VALUE   
      *>          "| |          ||  `/,|"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 20 COL 70 VALUE
      *> -        "| |          (\\`_.'"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 21 COL 70 VALUE   
      *>          "| |         .-`--'."
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 22 COL 70 VALUE
      *>          "| |        /Y . . Y\"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 23 COL 70 VALUE
      *>          "| |       // |   | \\"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 24 COL 70 VALUE   
      *>          "| |      //  | . |  \\"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 25 COL 70 VALUE
      *> -        "| |     ')   |   |   (`"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 26 COL 70 VALUE   
      *>          "| |          ||'||"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 27 COL 70 VALUE
      *>          "| |          || ||"
      *>          FOREGROUND-COLOR IS 2.

      *>      01 WORD-GUESSING-WINNING-SCREEN
      *>          BACKGROUND-COLOR IS 8.
      *>        05 BLANK SCREEN.
      *>        05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>       05 LINE 31 COLUMN 30 VALUE "You broke free and escaped to Th
      *> -      "e Wasteland!"
      *>        HIGHLIGHT, FOREGROUND-COLOR 6.
      *>        05 LINE 40 COLUMN 30 VALUE "You guessed the word!".
      *>        05 LINE 42 COLUMN 30 PIC X(20) USING WS-ANSWERWORD.
      *>        05 LINE 48 COLUMN 30 PIC 99 USING WS-GUESSES-LEFT.
      *>        05 LINE 50 COLUMN 30 VALUE "You scored: ".
      *>        05 LINE 48 COLUMN 90 PIC 99 USING WS-HIGH-SCORE.
      *>        05 LINE 52 COLUMN 30 VALUE "(p) Play Again"
      *>        REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 5.
      *>        05 LINE 53 COLUMN 30 VALUE "(h) See High Scores"
      *>        REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 6.
      *>        05 LINE 54 COLUMN 30 VALUE "(!) Quit game"
      *>        REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 7.
      *>        05 LINE 55 COLUMN 30 VALUE "Pick: ".
      *>        05 WS-GUESSING-CHOICE-WINNING-FIELD LINE 55 COLUMN 36 PIC X
      *>          USING WS-GUESSING-WINNING-CHOICE.
      *>              05 LINE 15 COL 70 VALUE
      *>              "               ,'-',"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 14 COL 70 VALUE   
      *>              "              :-----:"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 15 COL 70 VALUE
      *> -            "          (''' , - , ''')"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 16 COL 70 VALUE   
      *>              "          \   ' .  , `  /"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 17 COL 70 VALUE
      *>              "           \  '   ^  ? /"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 18 COL 70 VALUE
      *>              "            \ `   -  ,'"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 19 COL 70 VALUE   
      *>              "             `j_ _,'"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 20 COL 70 VALUE
      *> -            "        ,- -`\ \  /f"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 21 COL 70 VALUE   
      *>              "      ,-      \_\/_/'-"
      *>              FOREGROUND-COLOR IS 2.
      *>              05 LINE 22 COL 70 VALUE
      *>              "     ,                 `,"
      *>              FOREGROUND-COLOR IS 2.

      *>      01 WORD-GUESSING-LOSE-SCREEN
      *>          BACKGROUND-COLOR IS 8.
      *>        05 BLANK SCREEN.
      *>          05 LINE 13 COL 30 VALUE
      *>          "       ______"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 14 COL 30 VALUE   
      *>          "    .-'      '-."
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 15 COL 30 VALUE
      *> -        "   /            \"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 16 COL 30 VALUE   
      *>          "  |              |"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 17 COL 30 VALUE
      *>          "  |,  .-.  .-.  ,|"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 18 COL 30 VALUE
      *>          "  | )(__/  \__)( |"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 19 COL 30 VALUE   
      *>          "  |/     /\     \|"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 20 COL 30 VALUE
      *> -        "  (_     ^^     _)"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 21 COL 30 VALUE   
      *>          "   \__|IIIIII|__/"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 22 COL 30 VALUE
      *>          "    | \IIIIII/ |"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 23 COL 30 VALUE
      *>          "    \          /|"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 24 COL 30 VALUE
      *>          "     `--------`"
      *>          FOREGROUND-COLOR IS 2.
      *>        05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>       05 LINE 26 COLUMN 30 VALUE "You've been fed to the mudcrabs"
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 30 COLUMN 30 PIC X(20) USING WS-WORD
      *>        HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>        05 LINE 28 COLUMN 30 VALUE "The correct word was:".
      *>        05 LINE 30 COLUMN 45 PIC X(20) USING WS-ANSWERWORD 
      *>        HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>        05 LINE 32 COLUMN 30 VALUE "Guesses left: ".
      *>        05 LINE 32 COLUMN 50 PIC 99 USING WS-GUESSES-LEFT.
      *>        05 LINE 33 COLUMN 30 VALUE "(p) Play again" 
      *>          HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 34 COLUMN 30 VALUE "(h) See high scores"
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 35 COLUMN 30 VALUE "(!) Quit game"
      *>        HIGHLIGHT FOREGROUND-COLOR IS 3.
      *>        05 LINE 36 COLUMN 30 VALUE "Pick: " 
      *>          HIGHLIGHT FOREGROUND-COLOR IS 3 BLINK.
      *>        05 WS-GUESSING-CHOICE-LOSE-FIELD LINE 36 COLUMN 36 PIC X
      *>          USING WS-GUESSING-LOSING-CHOICE.

      *>      01 HIGH-SCORE-SCREEN
      *>          BACKGROUND-COLOR IS 8.
      *>        05 BLANK SCREEN.
      *>          05 LINE 13 COL 70 VALUE
      *>          "   .-=========-."
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 14 COL 70 VALUE   
      *>          "   \'-=======-'/"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 15 COL 70 VALUE
      *> -        "   _|   .=.   |_"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 16 COL 70 VALUE   
      *>          "  ((|  {{1}}  |))"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 17 COL 70 VALUE
      *>          "   \|   /|\   |/"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 18 COL 70 VALUE
      *>          "    \__ '`' __/"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 19 COL 70 VALUE   
      *>          "      _`) (`_"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 20 COL 70 VALUE
      *> -        "    _/_______\_"
      *>          FOREGROUND-COLOR IS 2.
      *>          05 LINE 21 COL 70 VALUE   
      *>          "   /___________\"
      *>          FOREGROUND-COLOR IS 2.          
      *>        05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 31 COLUMN 30 VALUE "WASTELAND LEGENDS:"
      *>        HIGHLIGHT, FOREGROUND-COLOR 2.
      *>        05 LINE 34 COLUMN 30 VALUE "High Scores:".
      *>        05 LINE 36 COLUMN 30 PIC XX USING WS-SCORE(1).
      *>        05 LINE 36 COLUMN 34 PIC X(10) USING WS-NAME(1).
      *>        05 LINE 38 COLUMN 30 PIC XX USING WS-SCORE(2).
      *>        05 LINE 38 COLUMN 34 PIC X(10) USING WS-NAME(2).
      *>        05 LINE 40 COLUMN 30 PIC XX USING WS-SCORE(3).
      *>        05 LINE 40 COLUMN 34 PIC X(10) USING WS-NAME(3).
      *>        05 LINE 42 COLUMN 30 VALUE "(b) Go back".
      *>        05 LINE 44 COLUMN 30 VALUE "Pick: ".
      *>        05 WS-HIGH-SCORE-FIELD LINE 44 COLUMN 36 PIC X
      *>          USING WS-HIGH-SCORE-CHOICE.
           
      *>      01 RANDOM-NUM-GAME-SCREEN.
      *>       05 BLANK SCREEN.
      *>        05 LINE 12 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>        05 LINE 13 COL 30 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>        05 LINE 14 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
             
      *>       05 LINE 18 COLUMN 34 VALUE "Place a bet and guess a number b
      *> -       "etween 1 and 10"
      *>       FOREGROUND-COLOR IS 6.
      *>       05 LINE 20 COLUMN 34 VALUE "Bet: ".
      *>       05 BET-FIELD LINE 20 COLUMN 39 PIC 999 USING BET-AMOUNT.
      *>       05 LINE 21 COLUMN 34 PIC X(20) USING INSUFFICIENT-FUNDS. 
         
      *>       05 LINE 24 COL 34 VALUE "(s) Submit "
      *>           REVERSE-VIDEO, HIGHLIGHT. 
      *>        05 LINE 25 COL 34 VALUE "(g) Go back"
      *>               REVERSE-VIDEO , HIGHLIGHT.            
      *>        05 LINE 26 COL 34 VALUE "(q) Quit   "
      *>               REVERSE-VIDEO, HIGHLIGHT.  
      *>        05 LINE 28 COL 34 VALUE "Pick: ".
      *>        05 RANDOM-NUM-CHOICE-FIELD LINE 28 COL 40 PIC X 
      *>              USING RANDOM-NUM-CHOICE.
      *>        05 LINE 29 COLUMN 34 PIC X(65) USING CREDIT-LIMIT-MESSAGE.
      *>        05 LINE 30 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>        05 LINE 31 COL 30 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>        05 LINE 32 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.

      *>      01 GUESS-SCREEN.
      *>      05 BLANK SCREEN.
      *>        05 LINE 12 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>        05 LINE 13 COL 30 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>        05 LINE 14 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
      *>        05 LINE 16 COLUMN 34 VALUE IS "Potential winnings: "
      *>          FOREGROUND-COLOR IS 5.
      *>        05 LINE 16 COL 54 PIC 999 USING WINNINGS.
      *>        05 GUESS-FIELD LINE 19 COLUMN 34 PIC XX USING GUESS-INPUT.
      *>        05 LINE 20 COLUMN 34 PIC X(40) USING WS-RANDOM-NUM-MSG.
      *>        05 LINE 24 COL 34 VALUE "(y) Play again "
      *>           REVERSE-VIDEO, HIGHLIGHT. 
      *>        05 LINE 25 COL 34 VALUE "(g) Go back"
      *>               REVERSE-VIDEO , HIGHLIGHT.            
      *>        05 LINE 26 COL 34 VALUE "(q) Quit   "
      *>               REVERSE-VIDEO, HIGHLIGHT.  
      *>        05 LINE 28 COL 34 VALUE "Pick: ".
      *>        05 RANDOM-NUM-GUESS-CHOICE-FIELD LINE 28 COL 40 PIC X 
      *>              USING RANDOM-NUM-GUESS-CHOICE.
      *>        05 LINE 34 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 3.
      *>        05 LINE 35 COL 30 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>        05 LINE 36 COL 30 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.


      *>      01 LIBRARY-SCREEN.
      *>      05 BLANK SCREEN.
      *>         05 LINE 09 COL 49 VALUE "---------------------------------
      *> -      "----------------------" FOREGROUND-COLOR IS 3.
      *>          05 LINE 10 COL 49 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>          05 LINE 11 COL 49 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
               
               
      *>          05 LINE 12 COL 49 VALUE 
      *>      "-------------------------------------------------------".
      *>          05 LINE 13 COL 65 VALUE
      *>          "WELCOME TO THE LIBRARY".

      *>          05 LINE 14 COL 49 VALUE
      *>      "Please Choose Below which book you would like to have in"
      *>        .
      *>          05 LINE 15 COL 49 VALUE
      *>      "AudioBook Format, the charge will be 5 credits"
      *>        .
                   
      *>          05 LINE 18 COL 45 VALUE "||   AUTHOR   ||".
      *>          05 LINE 18 COL 66 VALUE 



      *>          "||                  TITLE                ||".
      *>          05 LINE 19 COL 43 VALUE '1.'.
      *>          05 LINE 19 COL 49 PIC X(12) 
      *>          USING WS-BOOK-AUTHOR-NAME(OFFSET).
      *>          05 LINE 19 COL 69 PIC X(30) USING WS-BOOK-TITLE(OFFSET).
      *>          05 LINE 20 COL 49 VALUE
      *>      "---------------------------------------------------------".
      *>          05 LINE 21 COL 43 VALUE '2.'.
      *>          05 LINE 21 COL 49 PIC X(12) 
      *>          USING WS-BOOK-AUTHOR-NAME(OFFSET - 1)
      *>          .
      *>          05 LINE 21 COL 69 PIC X(30) 
      *>          USING WS-BOOK-TITLE(OFFSET - 1)
      *>          .
      *>          05 LINE 22 COL 49 VALUE
      *>      "---------------------------------------------------------".
      *>          05 LINE 23 COL 43 VALUE '3.'.
      *>          05 LINE 23 COL 49 PIC X(12) 
      *>          USING WS-BOOK-AUTHOR-NAME(OFFSET - 2)
      *>          .
      *>          05 LINE 23 COL 69 PIC X(30) 
      *>          USING WS-BOOK-TITLE(OFFSET - 2)
      *>          .
      *>          05 LINE 24 COL 49 VALUE
      *>      "---------------------------------------------------------".
      *>          05 LINE 25 COL 43 VALUE '4.'.
      *>          05 LINE 25 COL 49 PIC X(12) 
      *>          USING WS-BOOK-AUTHOR-NAME(OFFSET - 3)
      *>          .
      *>          05 LINE 25 COL 69 PIC X(30) 
      *>          USING WS-BOOK-TITLE(OFFSET - 3)
      *>          .
      *>          05 LINE 26 COL 49 VALUE
      *>      "---------------------------------------------------------".
      *>          05 LINE 27 COL 43 VALUE '5.'.
      *>          05 LINE 27 COL 49 PIC X(12) 
      *>          USING WS-BOOK-AUTHOR-NAME(OFFSET - 4)
      *>          .
      *>          05 LINE 27 COL 69 PIC X(30) 
      *>          USING WS-BOOK-TITLE(OFFSET - 4)
      *>          .
      *>          05 LINE 28 COL 49 VALUE
      *>      "---------------------------------------------------------".

                
           
      *>          05 LINE 31 COL 43 PIC X(40) USING LIBRARY-DISPLAY-MESSAGE
      *>          .
      *>          05 LINE 31 COL 77 VALUE 'Page No.'.
      *>          05 LINE 31 COL 86 PIC 99 USING PAGE-NUM.
      *>          05 LINE 35 COL 43 VALUE "( )Read the book by number".
      *>          05 LINE 35 COL 77 VALUE "(n) Next Page".
      *>          05 LINE 36 COL 43 VALUE "(p) Previous Page".
      *>          05 LINE 36 COL 77 VALUE "(q) Go back".
      *>          05 LINE 38 COL 78 VALUE "Pick: ".
      *>          05 LIBRARY-FIELD LINE 38 COLUMN 86 PIC X 
      *>          USING LIBRARY-CHOICE.
      *>          05 LINE 40 COL 78 PIC X(20) USING INSUFFICIENT-FUNDS.
               
               
      *>      01 READ-BOOK-SCREEN
      *>          BACKGROUND-COLOR IS 8.
      *>           05 BLANK SCREEN.
      *>      05 LINE 09 COL 49 VALUE "---------------------------------
      *> -      "----------------------" FOREGROUND-COLOR IS 3.
      *>         05 LINE 10 COL 49 VALUE "*********************************
      *> -      "***********************" FOREGROUND-COLOR IS 5.
      *>         05 LINE 11 COL 49 VALUE "---------------------------------
      *> -      "-----------------------" FOREGROUND-COLOR IS 2.
               
               
      *>          05 LINE 12 COL 49 VALUE 
      *>      "-------------------------------------------------------".
      *>          05 LINE 13 COL 65 VALUE
      *>          "WELCOME TO THE LIBRARY".
      *>          05 LINE 14 COL 49 VALUE
      *>      "Please Choose Below which book you would like to have in"
      *>        .

      *>          05 LINE 15 COL 49 VALUE
      *>      "AudioBook Format, the charge will be 5 credits"
      *>        .
      *>          05 LINE 16 COL 49 PIC X(50) USING AUDIOBOOK-MSG
      *>               HIGHLIGHT, FOREGROUND-COLOR IS 4.
      *>          05 LINE 18 COL 60 VALUE 'Title:'.
      *>          05 LINE 18 COL 69 PIC X(50) USING TITLE.
      *>          05 LINE 22 COLUMN 40 PIC X(60) USING 
      *>          WS-READ-BODY-SEGMENT-1.
      *>          05 LINE 23 COLUMN 40 PIC X(60) USING 
      *>          WS-READ-BODY-SEGMENT-2.
      *>          05 LINE 24 COLUMN 40 PIC X(60) USING 
      *>          WS-READ-BODY-SEGMENT-3.
               

      *>      01 WEATHER-SCREEN-1
      *>          BACKGROUND-COLOR IS 0.
      *>          05 BLANK SCREEN.
      *>          05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
      *>          UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
      *>          05 LINE 18 COL 69 VALUE "WEATHER REPORT: " UNDERLINE, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 2.
      *>          05 LINE 22 COL 45 VALUE "MORNING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 59 VALUE "OVERCAST" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 59 VALUE "10 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 59 VALUE "<- 3-4 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 59 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 48 VALUE     ".--.".
      *>          05 LINE 26 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 27 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 22 COL 80 VALUE "NOON: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 94 VALUE "HEAVY RAIN" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 94 VALUE "16 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 94 VALUE "<- 6-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 94 VALUE "2.4 mm | 87%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 24 COL 83 VALUE     ".--.".
      *>          05 LINE 25 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 26 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 27 COL 79 VALUE " , , , , ," FOREGROUND-COLOR 3.
      *>          05 LINE 28 COL 79 VALUE ", , , , ," FOREGROUND-COLOR 3.
      *>          05 LINE 31 COL 45 VALUE "EVENING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 59 VALUE "PATCHY RAIN" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 59 VALUE "18 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 59 VALUE "<- 4-6 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 59 VALUE "1.7 mm | 68%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 44 VALUE  "_`/''" FOREGROUND-COLOR 6 . 
      *>          05 LINE 33 COL 49 VALUE    ".-.".
      *>          05 LINE 34 COL 45 VALUE    ",\_" FOREGROUND-COLOR 6 .
      *>          05 LINE 34 COL 48 VALUE      "(   ).". 
      *>          05 LINE 35 COL 46 VALUE    "/" FOREGROUND-COLOR 6 .
      *>          05 LINE 35 COL 47 VALUE   "(___(__)".
      *>          05 LINE 36 COL 47 VALUE " , , , " FOREGROUND-COLOR 3.
      *>          05 LINE 37 COL 47 VALUE "  , , " FOREGROUND-COLOR 3.
      *>          05 LINE 31 COL 80 VALUE "NIGHT: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 94 VALUE "PATCHY RAIN" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 94 VALUE "12 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 94 VALUE "<- 2-4 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 94 VALUE "1.4 mm | 64%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 79 VALUE  "_`/''" FOREGROUND-COLOR 6 . 
      *>          05 LINE 33 COL 84 VALUE    ".-.".
      *>          05 LINE 34 COL 80 VALUE    ",\_" FOREGROUND-COLOR 6 .
      *>          05 LINE 34 COL 83 VALUE      "(   ).". 
      *>          05 LINE 35 COL 81 VALUE    "/" FOREGROUND-COLOR 6 .
      *>          05 LINE 35 COL 82 VALUE   "(___(__)".
      *>          05 LINE 36 COL 82 VALUE " , , , " FOREGROUND-COLOR 3.
      *>          05 LINE 37 COL 82 VALUE "  , , " FOREGROUND-COLOR 3.
      *>          05 LINE 40 COL 69 VALUE "(g) Go back"
      *>          HIGHLIGHT, FOREGROUND-COLOR 3 .            
      *>          05 LINE 42 COL 69 VALUE "Pick: " FOREGROUND-COLOR 2.
      *>          05 W1-CHOICE-FIELD LINE 42 COL 75 PIC X 
      *>          USING W1-CHOICE BLINK, FOREGROUND-COLOR 2.
      *>          05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      *> -        "LIVERY SERVICE" FOREGROUND-COLOR 2.

      *>      01 WEATHER-SCREEN-2.
      *>          05 BLANK SCREEN.
      *>          05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
      *>          UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
      *>          05 LINE 18 COL 69 VALUE "WEATHER REPORT: " UNDERLINE, 
      *>          HIGHLIGHT FOREGROUND-COLOR 2.
      *>          05 LINE 22 COL 45 VALUE "MORNING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 59 VALUE "OVERCAST" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 59 VALUE "9 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 59 VALUE "-> 3-4 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 59 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 48 VALUE     ".--.".
      *>          05 LINE 26 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 27 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 22 COL 80 VALUE "NOON: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 94 VALUE "PARTLY CLOUDY" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 94 VALUE "14 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 94 VALUE "-> 2-4 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 94 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 81 VALUE    "\  / " FOREGROUND-COLOR 6
      *>          . 
      *>          05 LINE 25 COL 79 VALUE  "_ /''" FOREGROUND-COLOR 6 . 
      *>          05 LINE 25 COL 84 VALUE    ".-.".
      *>          05 LINE 26 COL 81 VALUE    "\_" FOREGROUND-COLOR 6 .
      *>          05 LINE 26 COL 83 VALUE      "(   ).". 
      *>          05 LINE 27 COL 81 VALUE    "/" FOREGROUND-COLOR 6 .
      *>          05 LINE 27 COL 82 VALUE   "(___(__)".
      *>          05 LINE 31 COL 45 VALUE "EVENING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 59 VALUE "PARTLY CLOUDY" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 59 VALUE "12 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 59 VALUE "-> 4-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 59 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 46 VALUE    "\  / " FOREGROUND-COLOR 6
      *>          . 
      *>          05 LINE 34 COL 44 VALUE  "_ /''" FOREGROUND-COLOR 6 . 
      *>          05 LINE 34 COL 49 VALUE    ".-.".
      *>          05 LINE 35 COL 46 VALUE    "\_" FOREGROUND-COLOR 6 .
      *>          05 LINE 35 COL 48 VALUE      "(   ).". 
      *>          05 LINE 36 COL 46 VALUE    "/" FOREGROUND-COLOR 6 .
      *>          05 LINE 36 COL 47 VALUE   "(___(__)".
      *>          05 LINE 31 COL 80 VALUE "NIGHT: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 94 VALUE "OVERCAST" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 94 VALUE "10 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 94 VALUE "-> 6-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 94 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 83 VALUE     ".--.".
      *>          05 LINE 35 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 36 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 40 COL 69 VALUE "(g) Go back"
      *>          HIGHLIGHT, FOREGROUND-COLOR 3 .            
      *>          05 LINE 42 COL 69 VALUE "Pick: " FOREGROUND-COLOR 2.
      *>          05 W2-CHOICE-FIELD LINE 42 COL 75 PIC X 
      *>          USING W2-CHOICE BLINK, FOREGROUND-COLOR 2.
      *>          05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      *> -        "LIVERY SERVICE" FOREGROUND-COLOR 2.


      *>      01 WEATHER-SCREEN-3.
      *>          05 BLANK SCREEN.
      *>          05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
      *>          UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
      *>          05 LINE 18 COL 69 VALUE "WEATHER REPORT: " UNDERLINE, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 2.
      *>          05 LINE 22 COL 45 VALUE "MORNING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 59 VALUE "OVERCAST" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 59 VALUE "14 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 59 VALUE "-> 1-4 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 59 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 48 VALUE     ".--.".
      *>          05 LINE 26 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 27 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 22 COL 80 VALUE "NOON: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 23 COL 80 VALUE "SEVERE WEATHER: " BLINK, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 4.
      *>          05 LINE 24 COL 94 VALUE "RADSTORM" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 94 VALUE "26 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 94 VALUE "<- 14-20 km/h " 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 27 COL 94 VALUE "3.8 mm | 87%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 24 COL 83 VALUE     ".--.".
      *>          05 LINE 25 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 26 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 27 COL 79 VALUE " , * , * ," FOREGROUND-COLOR 2.
      *>          05 LINE 28 COL 79 VALUE "* , * , *" FOREGROUND-COLOR 2.
      *>          05 LINE 31 COL 45 VALUE "EVENING: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 32 COL 45 VALUE "SEVERE WEATHER: " BLINK, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 4.
      *>          05 LINE 33 COL 59 VALUE "RADSTORM" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 59 VALUE "32 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 59 VALUE "<- 12-18 km/h " 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 36 COL 59 VALUE "4.1 mm | 81%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 48 VALUE     ".--.".
      *>          05 LINE 34 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 35 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 36 COL 44 VALUE " , * , * ," FOREGROUND-COLOR 2.
      *>          05 LINE 37 COL 44 VALUE "* , * , *" FOREGROUND-COLOR 2.
      *>          05 LINE 31 COL 80 VALUE "NIGHT: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 94 VALUE "LIGHT RAIN" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 94 VALUE "18 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 94 VALUE "-> 4-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 94 VALUE "1.4 mm | 62%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 83 VALUE     ".--.".
      *>          05 LINE 34 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 35 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 36 COL 79 VALUE " ` ` ` ` `" FOREGROUND-COLOR 3.
      *>          05 LINE 37 COL 79 VALUE "` ` ` ` `" FOREGROUND-COLOR 3.
      *>          05 LINE 40 COL 69 VALUE "(g) Go back" HIGHLIGHT, 
      *>          FOREGROUND-COLOR 3 .            
      *>          05 LINE 42 COL 69 VALUE "Pick: " FOREGROUND-COLOR 2.
      *>          05 W3-CHOICE-FIELD LINE 42 COL 75 PIC X 
      *>          USING W3-CHOICE FOREGROUND-COLOR 2.
      *>          05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      *> -        "LIVERY SERVICE" FOREGROUND-COLOR 2.


      *>      01 WEATHER-SCREEN-4.
      *>          05 BLANK SCREEN.
      *>          05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
      *>          UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
      *>          05 LINE 18 COL 69 VALUE "WEATHER REPORT: " UNDERLINE, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 2.
      *>          05 LINE 22 COL 45 VALUE "MORNING: " HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 59 VALUE "OVERCAST" HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 59 VALUE "2 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 59 VALUE "-> 4-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 27 COL 59 VALUE "0.0 mm | 0%" FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 48 VALUE     ".--.".
      *>          05 LINE 26 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 27 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 22 COL 80 VALUE "NOON: " HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 24 COL 94 VALUE "SLEET SHOWERS" HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 25 COL 94 VALUE "1 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 26 COL 94 VALUE "<- 11-14 km/h " FOREGROUND-COLOR
      *>           2.
      *>          05 LINE 27 COL 94 VALUE "2.8 mm | 82%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 24 COL 83 VALUE     ".--.".
      *>          05 LINE 25 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 26 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 27 COL 79 VALUE " ,   ,   ," FOREGROUND-COLOR 3.
      *>          05 LINE 28 COL 79 VALUE "   *   *  " .
      *>          05 LINE 31 COL 45 VALUE "EVENING: " HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 32 COL 45 VALUE "CAUTION: " BLINK, 
      *>          HIGHLIGHT, FOREGROUND-COLOR 6.
      *>          05 LINE 33 COL 59 VALUE "HEAVY SNOW" HIGHLIGHT, 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 59 VALUE "2 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 59 VALUE "<- 12-18 km/h " 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 36 COL 59 VALUE "4.1 mm | 81%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 48 VALUE     ".--.".
      *>          05 LINE 34 COL 45 VALUE  ".-(    ).". 
      *>          05 LINE 35 COL 44 VALUE "(___.__)__)".
      *>          05 LINE 36 COL 44 VALUE " * * * * *".
      *>          05 LINE 37 COL 44 VALUE "* * * * *".
      *>          05 LINE 31 COL 80 VALUE "NIGHT: " HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 33 COL 94 VALUE "LIGHT SNOW" HIGHLIGHT 
      *>          FOREGROUND-COLOR 2.
      *>          05 LINE 34 COL 94 VALUE "-1 'C" FOREGROUND-COLOR 2.
      *>          05 LINE 35 COL 94 VALUE "<- 4-8 km/h " FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 36 COL 94 VALUE "1.4 mm | 62%" FOREGROUND-COLOR 
      *>          2.
      *>          05 LINE 33 COL 83 VALUE     ".--.".
      *>          05 LINE 34 COL 80 VALUE  ".-(    ).". 
      *>          05 LINE 35 COL 79 VALUE "(___.__)__)".
      *>          05 LINE 36 COL 79 VALUE " *   *   *".
      *>          05 LINE 37 COL 79 VALUE "  *   *  ".
      *>          05 LINE 40 COL 69 VALUE "(g) Go back" HIGHLIGHT, 
      *>          FOREGROUND-COLOR 3 .            
      *>          05 LINE 42 COL 69 VALUE "Pick: " FOREGROUND-COLOR 2.
      *>          05 W4-CHOICE-FIELD LINE 42 COL 75 PIC X 
      *>          USING W4-CHOICE BLINK, FOREGROUND-COLOR 2.
      *>          05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      *> -        "LIVERY SERVICE" FOREGROUND-COLOR 2.


        


           01 TORCH-SCREEN
               BACKGROUND-COLOR IS 6 . 
               05 BLANK SCREEN.
               05 TORCH-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING TORCH-CHOICE BLINK.

      *>      01 BUY-CREDITS-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
      *>      FOREGROUND-COLOR IS 2.
      *>      05 LINE 28 COL 34 VALUE 
      *>      "Please enter the amount of credits you would like to purchas
      *> -    "e: " FOREGROUND-COLOR IS 2.
          
      *>      05 CREDIT-FIELD LINE 29 COLUMN 34 PIC 999 USING CREDIT-AMOUNT
      *>      .
      *>      05 LINE 31 COL 34 VALUE "(s) Submit "
      *>           HIGHLIGHT FOREGROUND-COLOR IS 3. 
      *>      05 LINE 32 COL 45 VALUE "(g) Go back"
      *>           HIGHLIGHT FOREGROUND-COLOR IS 3.            
      *>      05 LINE 32 COL 34 VALUE "(q) Quit   "
      *>          HIGHLIGHT FOREGROUND-COLOR IS 3.  
      *>      05 LINE 34 COL 34 VALUE "Pick: " FOREGROUND-COLOR 2 HIGHLIGHT
      *>      .
      *>      05 BUY-CREDITS-CHOICE-FIELD LINE 34 COL 40 PIC X 
      *>          USING BUY-CREDITS-CHOICE.

      *>      05 LINE 36 COL 34 PIC X(50) USING CREDIT-LIMIT-MESSAGE
      *>      HIGHLIGHT, FOREGROUND-COLOR IS 3.



      *>      01 CONFIRM-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE.
      *>      05 LINE 26 COL 50 PIC 999 USING CREDIT-AMOUNT.
      *>      05 LINE 27 COL 34 VALUE "Credits will cost:".
      *>      05 LINE 28 COL 34 PIC 999 USING CREDIT-AMOUNT.
      *>      05 LINE 28 COL 38 VALUE "bottle caps".
      *>      05 LINE 29 COL 34 VALUE 
      *>      "Please enter your password to confirm payment.".
          
      *>      05 LINE 30 COL 34 VALUE "Password: ".
      *>      05 BUY-PASSWORD-FIELD LINE 30 COL 34 PIC X(20) 
      *>          USING PASSWORD-ENTRY.
      *>      05 LINE 32 COL 34 PIC X(20) USING INC-PASSWORD 
      *>           HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>      05 LINE 36 COL 34 VALUE "(s) Submit "
      *>           HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>      05 LINE 37 COL 34 VALUE "(g) Go back"
      *>           HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>      05 LINE 38 COL 34 VALUE "(q) Quit   "
      *>           HIGHLIGHT, FOREGROUND-COLOR IS 2.
      *>      05 LINE 39 COL 34 VALUE "Pick: ".
      *>      05 CONFIRM-CHOICE-FIELD LINE 39 COL 40 PIC X 
      *>          USING CONFIRM-CHOICE.
          

      *>  01 PAYMENT-PROCESS-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE.
      *>      05 LINE 28 COL 34 VALUE "Processing payment of bottle caps: "
      *>        .
      *>      05 LINE 30 COL 47 PIC 999 USING CREDIT-AMOUNT.
      *>      05 LINE 31 COL 34 VALUE "Confirming payment with your bank ".
      *>      05 LINE 35 COL 34 VALUE "This page will redirect in a few sec
      *> -    "seconds".
       

      *>  01 PAY-CONFIRMATION-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 26 COL 34 VALUE "Buy Credits" UNDERLINE
      *>      FOREGROUND-COLOR 2.
      *>      05 LINE 28 COL 34 VALUE "Thank you for your purchase "
      *>        FOREGROUND-COLOR 2.
      *>      05 LINE 30 COL 34 VALUE "Your transaction is pending"
      *>        FOREGROUND-COLOR 2.
      *>      05 LINE 31 COL 34 PIC 999 USING CREDIT-AMOUNT.
      *>      05 LINE 31 COL 38 VALUE "credits will be added to your "
      *>        FOREGROUND-COLOR 2.
      *>      05 LINE 31 COL 68 VALUE "account within 24 hours"
      *>        FOREGROUND-COLOR 2.
      *>      05 LINE 36 COL 61 VALUE "(g) Go back"
      *>           FOREGROUND-COLOR 2, HIGHLIGHT.
      *>      05 LINE 36 COL 75 VALUE "(q) Quit   "
      *>           FOREGROUND-COLOR 2, HIGHLIGHT.  
      *>      05 LINE 37 COL 47 VALUE "Pick: "
      *>        FOREGROUND-COLOR 2.
      *>      05 PAY-CONFIRMATION-FIELD LINE 37 COL 54 PIC X 
      *>          USING PAY-CONFIRMATION-CHOICE. 
       

      *>  01 ABOUT-PAGE-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 6 COL 10 VALUE
      *>      "           _                 _     _____                 ".
      *>      05 LINE 7 COL 10 VALUE
      *>      "     /\   | |               | |   |  __ \                ".
      *>      05 LINE 8 COL 10 VALUE
      *>      "    /  \  | |__   ___  _   _| |_  | |__) |_ _  __ _  ___ ".
      *>      05 LINE 9 COL 10 VALUE
      *>      "   / /\ \ | '_ \ / _ \| | | | __| |  ___/ _` |/ _` |/ _ \".
      *>      05 LINE 10 COL 10 VALUE
      *>      "  / ____ \| |_) | (_) | |_| | |_  | |  | (_| | (_| |  __/".
      *>      05 LINE 11 COL 10 VALUE
      *>      " /_/    \_\_.__/ \___/ \__,_|\__| |_|   \__,_|\__, |\___|".
      *>      05 LINE 12 COL 10 VALUE
      *>      "                                               __/ |     ".
      *>      05 LINE 13 COL 10 VALUE
      *>      "                                              |___/      ".
      *>      05 LINE 18 COL 10 VALUE 
      *>      "Welcome to the BBS System, after extensive user feedback ".
      *>      05 line 19 col 10 value
      *>      "and the mass influx of users we have extended our ".
      *>      05 LINE 20 COL 10 VALUE
      *>      "functionality of the system, this has meant however we've ".
      *>      05 LINE 21 COL 10 VALUE
      *>      "had to implement a monetary payment system for upkeep ".
      *>      05 LINE 22 COL 10 VALUE
      *>      "below is a few bits of advice for using our credits ".
      *>      05 LINE 23 COL 10 VALUE 
      *>      "system and in general, the program itself.".
      *>      05 LINE 26 COL 10 VALUE '1.'.
      *>      05 LINE 26 COL 13 PIC X(31) USING 
      *>      WS-ABOUT-TITLE(ABOUT-OFFSET).
      *>      05 LINE 28 COL 10 VALUE '2.'.
      *>      05 LINE 28 COL 13 PIC X(31) USING 
      *>      WS-ABOUT-TITLE(ABOUT-OFFSET - 1).
      *>      05 LINE 30 COL 10 VALUE '3.'.
      *>      05 LINE 30 COL 13 PIC X(31) USING 
      *>      WS-ABOUT-TITLE(ABOUT-OFFSET - 2).
      *>      05 LINE 32 COL 10 VALUE '4.'.
      *>      05 LINE 32 COL 13 PIC X(31) USING 
      *>      WS-ABOUT-TITLE(ABOUT-OFFSET - 3).
      *>      05 LINE 34 COL 10 VALUE '5.'.
      *>      05 LINE 34 COL 13 PIC X(31) USING 
      *>      WS-ABOUT-TITLE(ABOUT-OFFSET - 4).
      *>      05 LINE 40 COL 10 VALUE "( ) What number to read".
      *>      05 LINE 41 COL 10 VALUE "(n) Next Page".
      *>      05 LINE 42 COL 10 VALUE "(p) Previous Page".
      *>      05 LINE 43 COL 10 VALUE "(q) Go back".
      *>      05 Line 38 COL 10 PIC X(15) USING
      *>      ABOUT-INVALID-CHOICE-MESSAGE
      *>          HIGHLIGHT, FOREGROUND-COLOR IS 4.
      *>      05 ABOUT-PAGE-FIELD LINE 44 COL 10 PIC X USING 
      *>      ABOUT-PAGE-CHOICE.

      *>  01 ABOUT-PAGE-READ-SCREEN.
      *>      05 BLANK SCREEN.
      *>      05 LINE 30 COL 30 PIC X(31) USING ABOUT-TITLE-READ.
      *>      05 LINE 32 COL 30 PIC X(500) USING ABOUT-BODY-READ.
      *>      05 LINE 50 COL 30 VALUE "(q) Go Back".
      *>      05 Line 46 COL 30 PIC X(15) USING 
      *>      ABOUT-INVALID-CHOICE-MESSAGE
      *>          HIGHLIGHT, FOREGROUND-COLOR IS 4.
      *>      05 ABOUT-PAGE-READ-FIELD LINE 48 COL 30 PIC X USING
      *>      ABOUT-PAGE-READ-CHOICE.
           

           01 CHANGE-PASSWORD-SCREEN.
               05 BLANK SCREEN.  
               05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
               UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
               05 LINE 17 COLUMN 30 VALUE "CHANGE YOUR PASSWORD" 
               HIGHLIGHT, FOREGROUND-COLOR 2 .
               05 LINE 19 COLUMN 30 VALUE "Update your account password,
      -        " periodically changing your password will improve accoun
      -        "t security" FOREGROUND-COLOR 2.
               05 LINE 21 COLUMN 30 VALUE "Enter current password:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 22 COLUMN 30 PIC X(50) USING PWORD-ERR-1 
               HIGHLIGHT FOREGROUND-COLOR 4.
               05 OLD-PASSWORD-FIELD LINE 23 COLUMN 30 PIC X(16)
                USING OLD-PASSWORD FOREGROUND-COLOR 2.
               05 LINE 24 COLUMN 30 PIC X(50) USING PWORD-OK-1 HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 25 COLUMN 30 VALUE "Enter new password:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 25 COLUMN 52 VALUE " (Your password must be a min
      -        "imum of 6 characters and 1 number.) " 
               FOREGROUND-COLOR 2.
               05 LINE 26 COLUMN 30 PIC X(50) USING PWORD-ERR-2 
               HIGHLIGHT FOREGROUND-COLOR 4.
               05 UPDATED-PASSWORD-FIELD LINE 27 COLUMN 30 PIC X(20)
               USING UPDATED-PASSWORD FOREGROUND-COLOR 2.
               05 LINE 28 COLUMN 30 PIC X(50) USING PWORD-OK-2 HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 29 COLUMN 30 VALUE "Re-enter your new password:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 30 COLUMN 30 PIC X(50) USING PWORD-ERR-3 
               HIGHLIGHT FOREGROUND-COLOR 4.
               05 CONFIRM-NEW-PASSWORD-FIELD LINE 31 COLUMN 30 PIC X(20)
               USING CONFIRM-NEW-PASSWORD FOREGROUND-COLOR 2.
               05 LINE 32 COLUMN 30 PIC X(50) USING PWORD-OK-3 HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 34 COLUMN 30 PIC X(50) USING PWORD-CONFIRM-MSG
               HIGHLIGHT FOREGROUND-COLOR 2 . 
               05 LINE 37 COL 66 VALUE "(s) Submit "
               HIGHLIGHT FOREGROUND-COLOR 3 . 
               05 LINE 38 COL 66 VALUE "(g) Go Back   "
               HIGHLIGHT FOREGROUND-COLOR 3.
               05 LINE 40 COLUMN 66 VALUE "Pick: " 
               BLINK HIGHLIGHT FOREGROUND-COLOR 2.
               05 CHANGE-PWORD-FIELD LINE 40 COLUMN 73 PIC X
               USING CHANGE-PWORD-CHOICE.
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE" FOREGROUND-COLOR 2.

           01 CHANGE-ACCOUNT-NUM-SCREEN.
               05 BLANK SCREEN.  
               05 LINE 8 COLUMN 30 VALUE "Connected to Vault" 
               UNDERLINE, BLINK, HIGHLIGHT, FOREGROUND-COLOR 3.
               05 LINE 17 COLUMN 30 VALUE "CHANGE YOUR ACCOUNT DETAILS" 
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 19 COLUMN 30 VALUE "Change the bank account detai
      -        "ls registered to this account."  FOREGROUND-COLOR 2.
               05 LINE 21 COLUMN 30 VALUE "Enter your password:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 22 COLUMN 30 PIC X(50) USING PWORD-ERROR 
               HIGHLIGHT FOREGROUND-COLOR 4.
               05 CHECK-PASSWORD-FIELD LINE 23 COLUMN 30 PIC X(16)
               USING CHECK-PASSWORD FOREGROUND-COLOR 2.
               05 LINE 24 COLUMN 30 PIC X(50) USING PWORD-OK HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 25 COLUMN 30 VALUE "Enter new account number:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 26 COLUMN 30 PIC X(50) USING ACNT-ERR-1 HIGHLIGHT
               FOREGROUND-COLOR 4.
               05 UPDATED-ACNT-FIELD LINE 27 COLUMN 30 PIC X(8)
               USING UPDATED-ACNT FOREGROUND-COLOR 2.
               05 LINE 28 COLUMN 30 PIC X(50) USING ACNT-OK-1 HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 29 COLUMN 30 VALUE "Re-enter account number:" 
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 30 COLUMN 30 PIC X(50) USING ACNT-ERR-2 HIGHLIGHT
               FOREGROUND-COLOR 4.
               05 CONFIRM-ACNT-FIELD LINE 31 COLUMN 30 PIC X(8)
               USING CONFIRM-ACNT FOREGROUND-COLOR 2.
               05 LINE 32 COLUMN 30 PIC X(50) USING ACNT-OK-2 HIGHLIGHT
               FOREGROUND-COLOR 2.
               05 LINE 34 COLUMN 30 PIC X(50) USING ACNT-CONFIRM-MSG
               HIGHLIGHT FOREGROUND-COLOR 2 . 
               05 LINE 37 COL 66 VALUE "(s) Submit "
               HIGHLIGHT FOREGROUND-COLOR 3 . 
               05 LINE 38 COL 66 VALUE "(g) Go Back   "
               HIGHLIGHT FOREGROUND-COLOR 3.
               05 LINE 40 COLUMN 66 VALUE "Pick: " 
               BLINK HIGHLIGHT FOREGROUND-COLOR 2.
               05 CHANGE-ACNT-FIELD LINE 40 COLUMN 73 PIC X
               USING CHANGE-ACNT-CHOICE.
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE" FOREGROUND-COLOR 2.

       PROCEDURE DIVISION.

       0100-DISPLAY-START.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE START-CHOICE.
           DISPLAY START-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.

           ACCEPT START-CHOICE-FIELD.
           IF START-CHOICE = "l" THEN 
               PERFORM 0110-DISPLAY-LOGIN 
           ELSE IF START-CHOICE = "c" THEN 
               PERFORM 0105-DISPLAY-REGISTER-NEW-USER
           ELSE IF START-CHOICE = "x" THEN 
               STOP RUN
           ELSE IF START-CHOICE = "a" THEN 
               CALL 'admin-server'
               PERFORM 0100-DISPLAY-START
           ELSE 
               PERFORM 0100-DISPLAY-START
           END-IF.

       0101-GENERATE-USER-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-USERS-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE USERNAME TO WS-USER-NAME(COUNTER)
                       MOVE USER-PASSWORD TO WS-PWORD(COUNTER)
                       MOVE USER-CREDIT TO WS-CREDIT(COUNTER)
                       MOVE USER-ACNT-NUM TO WS-ACNT-NUM(COUNTER)

                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-USERS-FILE.
   
       0105-DISPLAY-REGISTER-NEW-USER SECTION.
           PERFORM 0500-TIME-AND-DATE.
           PERFORM 0101-GENERATE-USER-TABLE.
           MOVE SPACES TO ERROR-MSG-1.
           MOVE SPACES TO ERROR-MSG-2.
           MOVE SPACES TO ERROR-MSG-3.
           MOVE SPACES TO OK-MSG-1.
           MOVE SPACES TO OK-MSG-2.
           MOVE SPACES TO OK-MSG-3.
           
           VALIDATE-USERNAME.
           INITIALIZE NEW-USER-NAME. 
           INITIALIZE NEW-PASSWORD.
           INITIALIZE ACCOUNT-NUM.
           INITIALIZE REGISTER-CHOICE.
           DISPLAY REGISTER-NEW-USER-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.
           ACCEPT NEW-USER-NAME-FIELD.
           MOVE 0 TO RAISE-ERROR.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF NEW-USER-NAME = WS-USER-NAME(WS-IDX) 
                   ADD 1 TO RAISE-ERROR
               END-IF
                   ADD 1 TO WS-IDX
           END-PERFORM.
           IF RAISE-ERROR > 0 
               MOVE 'USER NAME IN USE' TO ERROR-MSG-1
               PERFORM VALIDATE-USERNAME
           ELSE 
               MOVE 'USER NAME OK' TO OK-MSG-1
               MOVE SPACES TO ERROR-MSG-1
               PERFORM VALIDATE-PASSWORD
           END-IF. 

           VALIDATE-PASSWORD.
           INITIALIZE NEW-PASSWORD.
           DISPLAY REGISTER-NEW-USER-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.          
           DISPLAY CONNECTED-SCREEN.
           ACCEPT NEW-PASSWORD-FIELD.
           CALL 'validate-password' USING NEW-PASSWORD ERROR-MSG-2 
           RAISE-ERROR OK-MSG-2.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-PASSWORD
           ELSE 
               PERFORM VALIDATE-BANK-ACCOUNT
           END-IF. 

           VALIDATE-BANK-ACCOUNT.
           INITIALIZE ACCOUNT-NUM.
           DISPLAY REGISTER-NEW-USER-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.
           ACCEPT ACCOUNT-NUM-FIELD.
           CALL 'validate-bank-details' USING ACCOUNT-NUM ERROR-MSG-3
           RAISE-ERROR OK-MSG-3.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-BANK-ACCOUNT
           END-IF. 

           DISPLAY REGISTER-NEW-USER-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.
           ACCEPT REGISTER-CHOICE-FIELD.
           IF REGISTER-CHOICE = "g" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE IF REGISTER-CHOICE = "s" 
               OPEN EXTEND F-USERS-FILE
               MOVE NEW-USER-NAME TO USERNAME
               MOVE NEW-PASSWORD TO USER-PASSWORD
               MOVE ACCOUNT-NUM TO USER-ACNT-NUM
               MOVE "000" TO USER-CREDIT
               WRITE USERS
               END-WRITE 
           ELSE 
               PERFORM 0100-DISPLAY-START
           END-IF.
           CLOSE F-USERS-FILE.
           PERFORM 0100-DISPLAY-START.
          
       0110-DISPLAY-LOGIN.
           PERFORM 0500-TIME-AND-DATE.
           PERFORM 0101-GENERATE-USER-TABLE
           INITIALIZE USER-NAME.
           INITIALIZE WS-PASSWORD.
           DISPLAY LOGIN-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN.

           ACCEPT USER-NAME-FIELD.
           ACCEPT PASSWORD-FIELD.
           MOVE 0 TO WS-FOUND.
           MOVE 1 TO WS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL WS-IDX = COUNTER
               IF USER-NAME = WS-USER-NAME(WS-IDX) AND 
               WS-PASSWORD = WS-PWORD(WS-IDX) THEN
                   MOVE 1 TO WS-FOUND 
                   PERFORM 0111-USER-INFO
                   PERFORM 0113-DISPLAY-TIME-USER-INFO
               END-IF
               ADD 1 TO WS-IDX 
           END-PERFORM.

           IF WS-FOUND = 1 THEN
               PERFORM 0120-DISPLAY-MENU 
           ELSE 
               PERFORM 0115-ERROR-PAGE 
           END-IF.     

       0111-USER-INFO.
           MOVE WS-USER-NAME(WS-IDX) TO USER-INFO-NAME.
           MOVE WS-CREDIT(WS-IDX) TO USER-INFO-CREDITS.

       0112-UPDATE-CREDITS.
           MOVE UPDATED-BALANCE TO USER-INFO-CREDITS.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0115-ERROR-PAGE.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ERROR-CHOICE.
           DISPLAY ERROR-SCREEN.

           DISPLAY PIP-BOY-SCREEN.

           DISPLAY TIME-SCREEN.
           DISPLAY CONNECTED-SCREEN     

           ACCEPT ERROR-CHOICE-FIELD.
           IF ERROR-CHOICE = "l" THEN 
               PERFORM 0110-DISPLAY-LOGIN
           ELSE IF ERROR-CHOICE = "c" THEN 
               PERFORM 0105-DISPLAY-REGISTER-NEW-USER 
           ELSE IF ERROR-CHOICE = "g" THEN 
               PERFORM 0100-DISPLAY-START
           ELSE 
               PERFORM 0115-ERROR-PAGE 
           END-IF.
       
       0120-DISPLAY-MENU.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.

           DISPLAY PIP-BOY-SCREEN.

           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT MENU-CHOICE-FIELD.
           
           IF MENU-CHOICE = "x" or "X" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" or "L" THEN
             PERFORM 0100-DISPLAY-START
           ELSE IF MENU-CHOICE = "m" or "M" THEN
             CALL "msg-board-server" USING USER-INFO-NAME, 
             USER-INFO-CRED-DISPLAY
             PERFORM 0120-DISPLAY-MENU
            *>  PERFORM 0130-MSG-MENU
           ELSE IF MENU-CHOICE = "f" or "F" THEN
                CALL "games-server" USING USER-INFO-NAME, 
                USER-INFO-CRED-DISPLAY
                PERFORM 0120-DISPLAY-MENU
                *> PERFORM 0160-GAMES-MENU
           ELSE IF MENU-CHOICE = "b" or "B" THEN
               CALL "library-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY
               PERFORM 0120-DISPLAY-MENU
              *>  PERFORM 0220-GENERATE-LIBRARY-TABLE
           ELSE IF MENU-CHOICE = 'c' or 'C' THEN 
               CALL "buy-credits-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY, WS-USERS, USER-NAME, WS-PASSWORD,
               ACCOUNT-NUM
               PERFORM 0120-DISPLAY-MENU
              *>  PERFORM 0400-BUY-CREDITS
           ELSE IF MENU-CHOICE = 'a' or 'A' THEN 
               CALL "about-page-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY
               PERFORM 0120-DISPLAY-MENU
              *>  PERFORM 0470-ABOUT-PAGE-TABLE
           ELSE IF (MENU-CHOICE = 'w' or 'W')
               MOVE '2' TO COST
               CALL "weather-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY COST
               PERFORM 0120-DISPLAY-MENU 
              *>  PERFORM 0300-CHECK-WEATHER
           ELSE IF MENU-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF MENU-CHOICE = 'u' or 'U' THEN 
               PERFORM 0650-CHANGE-ACCOUNT-NUM
           ELSE IF MENU-CHOICE = 'p' or 'P' THEN 
               PERFORM 0600-CHANGE-PASSWORD
           END-IF.
      
           PERFORM 0120-DISPLAY-MENU.

       
      *>  0160-GAMES-MENU.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      INITIALIZE GAMES-MENU-CHOICE.

      *>      MOVE "5" TO COST.

      *>      DISPLAY GAMES-MENU-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT GAMES-MENU-CHOICE-FIELD
      *>      IF GAMES-MENU-CHOICE = "q" or "Q" THEN
      *>          STOP RUN
      *>      ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
      *>          PERFORM 0120-DISPLAY-MENU
      *>      ELSE IF (GAMES-MENU-CHOICE = "o" OR "O" )
      *>        AND (CHECK-BALANCE (COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          PERFORM 0190-O-AND-X-GAME  
      *>      ELSE IF (GAMES-MENU-CHOICE = "h" or "H") 
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          PERFORM 0170-DISPLAY-GUESSING-GAME
      *>      ELSE IF (GAMES-MENU-CHOICE = "n" or "N")  
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          PERFORM 0210-RANDOM-NUMBER-GAME           
      *>      END-IF.

      *>      IF CHECK-BALANCE(COST, USER-INFO-CREDITS) = "FALSE"
      *>          MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
      *>      PERFORM 0160-GAMES-MENU.


      *>  0170-DISPLAY-GUESSING-GAME.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      SET WS-HIGH-SCORE TO 0.
      *>      SET WS-WORD-LENGTH TO 0.
      *>      MOVE 15 TO WS-GUESSES-LEFT.
      *>      SET WORD-IDX TO 0.
      *>      OPEN INPUT F-WORD-FILE.
      *>      MOVE 0 TO WS-FILE-IS-ENDED.
      *>      PERFORM UNTIL WS-FILE-IS-ENDED = 1
      *>          READ F-WORD-FILE
      *>              NOT AT END
      *>                  ADD 1 TO WORD-IDX
      *>                  MOVE WORD TO WS-GUESSING-WORDS-WORD(WORD-IDX)
      *>              AT END
      *>                  MOVE 1 TO WS-FILE-IS-ENDED
      *>          END-READ
      *>      END-PERFORM.
      *>      CLOSE F-WORD-FILE.
      *>      MOVE FUNCTION CURRENT-DATE(14:3) TO RANDOMNUMBER.
      *>      MOVE WS-GUESSING-WORDS-WORD(RANDOMNUMBER) TO WS-WORD.
      *>      MOVE WS-WORD TO WS-ANSWERWORD.
      *>      MOVE REPLACE-LETTER(WS-WORD) TO WS-WORD. 
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.
      *>      MOVE 1 TO COUNTER.
      *>      PERFORM UNTIL COUNTER = 20
      *>        IF '*' EQUALS WS-WORD(COUNTER:1) 
      *>         THEN ADD 1 TO WS-WORD-LENGTH
      *>        END-IF
      *>        ADD 1 TO COUNTER
      *>      END-PERFORM.
      *>      PERFORM 0175-IN-GAME-SCREEN.

      *>  0175-IN-GAME-SCREEN.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      INITIALIZE WS-GUESS-CHOICE.
      *>      DISPLAY IN-GAME-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.
      
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT WS-GUESS-CHOICE-FIELD.
      *>      IF WS-GUESS-CHOICE = '!' THEN 
      *>          PERFORM 0160-GAMES-MENU
      *>      ELSE
      *>          PERFORM 0180-CHECK-GUESS
      *>      END-IF.

      *>  0180-CHECK-GUESS.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      MOVE 1 TO COUNTER.
      *>      PERFORM UNTIL COUNTER = 20
      *>            IF WS-GUESS-CHOICE = WS-ANSWERWORD(COUNTER:1) 
      *>            THEN
      *>                 MOVE WS-GUESS-CHOICE TO WS-WORD(COUNTER:1) 
      *>            END-IF
      *>            ADD 1 TO COUNTER     
      *>      END-PERFORM.
      *>      SUBTRACT 1 FROM WS-GUESSES-LEFT.
      *>      MOVE 1 TO COUNTER.
      *>      MOVE 0 TO WS-LETTERS-LEFT.
      *>      PERFORM UNTIL COUNTER = 20
      *>        IF '*' EQUALS WS-WORD(COUNTER:1) 
      *>         THEN ADD 1 TO WS-LETTERS-LEFT
      *>        END-IF
      *>        ADD 1 TO COUNTER
      *>      END-PERFORM.
      *>        IF WS-LETTERS-LEFT = 0
      *>         THEN 
      *>         PERFORM 0185-WINNING-SCREEN
      *>        ELSE IF WS-GUESSES-LEFT = 0
      *>         THEN 
      *>         PERFORM 0186-LOSING-SCREEN
      *>        ELSE
      *>         PERFORM 0175-IN-GAME-SCREEN
      *>        END-IF.

      *>  0185-WINNING-SCREEN.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      INITIALIZE WS-GUESSING-WINNING-CHOICE.
      *>      DISPLAY WS-WORD-LENGTH.
      *>      DISPLAY WS-GUESSES-LEFT.
      *>      DISPLAY WS-HIGH-SCORE.
      *>      MOVE HIGH-SCORE-CALCULATOR(WS-WORD-LENGTH WS-GUESSES-LEFT)
      *>      TO WS-HIGH-SCORE.
      *>      DISPLAY WS-WORD-LENGTH.
      *>      DISPLAY WS-GUESSES-LEFT.
      *>      DISPLAY WS-HIGH-SCORE.
      *>      DISPLAY WORD-GUESSING-WINNING-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.
      *>      *> DISPLAY USER-INFO-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      OPEN EXTEND F-HIGH-SCORES-FILE
      *>          MOVE WS-HIGH-SCORE TO HIGH-SCORE
      *>          MOVE USER-NAME TO PLAYER-NAME
      *>          WRITE PLAYER-SCORES 
      *>          END-WRITE.
      *>      CLOSE F-HIGH-SCORES-FILE.

      *>      ACCEPT WS-GUESSING-CHOICE-WINNING-FIELD.
      *>      IF WS-GUESSING-WINNING-CHOICE = 'p'
      *>          THEN PERFORM 0170-DISPLAY-GUESSING-GAME
      *>      ELSE IF WS-GUESSING-WINNING-CHOICE = 'h'
      *>        THEN PERFORM 0187-HIGH-SCORE-TABLE
      *>      ELSE IF WS-GUESSING-WINNING-CHOICE = '!'
      *>        THEN PERFORM 0160-GAMES-MENU
      *>      ELSE
      *>        PERFORM 0185-WINNING-SCREEN
      *>      END-IF.

      *>  0186-LOSING-SCREEN.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      INITIALIZE WS-GUESSING-LOSING-CHOICE.
      *>      DISPLAY WORD-GUESSING-LOSE-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.
            *> DISPLAY USER-INFO-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT WS-GUESSING-LOSING-CHOICE.
      *>      IF WS-GUESSING-LOSING-CHOICE = 'p'
      *>          THEN PERFORM 0170-DISPLAY-GUESSING-GAME
      *>      ELSE IF WS-GUESSING-LOSING-CHOICE = 'h'
      *>        THEN PERFORM 0187-HIGH-SCORE-TABLE
      *>      ELSE IF WS-GUESSING-LOSING-CHOICE = '!'
      *>        THEN PERFORM 0160-GAMES-MENU
      *>      ELSE
      *>        PERFORM 0186-LOSING-SCREEN
      *>      END-IF.

      *>  0187-HIGH-SCORE-TABLE.
      *>      SET COUNTER TO 0.
      *>      OPEN INPUT F-HIGH-SCORES-FILE.
      *>      MOVE 0 TO WS-FILE-IS-ENDED.
      *>      PERFORM UNTIL WS-FILE-IS-ENDED = 1
      *>          READ F-HIGH-SCORES-FILE
      *>              NOT AT END
      *>                  ADD 1 TO COUNTER
      *>                  MOVE HIGH-SCORE TO WS-SCORE(COUNTER)
      *>                  MOVE PLAYER-NAME TO WS-NAME(COUNTER)
      *>              AT END 
      *>                  MOVE 1 TO WS-FILE-IS-ENDED
      *>          END-READ 
      *>      END-PERFORM.
      *>      CLOSE F-HIGH-SCORES-FILE.
      *>      PERFORM 0188-HIGH-SCORE-SCREEN.

      *>  0188-HIGH-SCORE-SCREEN.
      *>      PERFORM 0500-TIME-AND-DATE.
      *>      INITIALIZE WS-HIGH-SCORE-CHOICE.
      *>      SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
      *>      DISPLAY HIGH-SCORE-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.
      *>      *> DISPLAY USER-INFO-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT WS-HIGH-SCORE-FIELD.
      *>      IF WS-HIGH-SCORE-CHOICE = 'b'
      *>        PERFORM 0120-DISPLAY-MENU
      *>      ELSE 
      *>          PERFORM 0188-HIGH-SCORE-SCREEN
      *>      END-IF.

            *>----- X AND O Procedure Div------    
      *>  0190-O-AND-X-GAME.
      *>      MOVE "X" TO WS-PLAYER
      *>      PERFORM GAME-LOOP-PARAGRAPH
      *>          WITH TEST AFTER UNTIL FINISHED-PLAYING
      *>      PERFORM 0160-GAMES-MENU.

      *>      GAME-LOOP-PARAGRAPH.
      *>          INITIALIZE WS-GAME-GRID
      *>          INITIALIZE WS-STATE
      *>          INITIALIZE WS-MOVES
      *>          MOVE "Make a move like 'A2'" TO WS-OANDXMESSAGE
      *>          PERFORM GAME-FRAME-PARAGRAPH
      *>              WITH TEST AFTER UNTIL GAME-OVER
      *>          ADD 1 TO WS-GAMES END-ADD
      *>          EVALUATE WS-STATE
      *>          WHEN "WIN"
      *>              ADD 1 TO WS-WINS END-ADD
      *>              MOVE WS-COLOR-BLACK TO WS-FG
      *>              MOVE WS-COLOR-BLACK TO WS-FG-CELL
      *>              MOVE WS-COLOR-GREEN TO WS-BG
      *>          WHEN "STALE"
      *>              MOVE WS-COLOR-BLACK TO WS-FG
      *>              MOVE WS-COLOR-BLACK TO WS-FG-CELL
      *>              MOVE WS-COLOR-BLUE TO WS-BG
      *>          WHEN OTHER
      *>              MOVE WS-COLOR-BLACK TO WS-FG
      *>              MOVE WS-COLOR-BLACK TO WS-FG-CELL
      *>              MOVE WS-COLOR-RED TO WS-BG
      *>          END-EVALUATE
      *>          MOVE "One more (y/n)? " TO WS-INSTRUCTION
      *>          MOVE "y" TO WS-NEXT-MOVE
      *>          DISPLAY BOARD-SCREEN.

      *>          DISPLAY PIP-BOY-SCREEN.
      *>          ACCEPT NEXT-MOVE.

      *>          PERFORM 0113-DISPLAY-TIME-USER-INFO
      *>          ACCEPT NEXT-MOVE.
           

      *>      GAME-FRAME-PARAGRAPH.
      *>          MOVE "Move to square: " TO WS-INSTRUCTION
      *>          MOVE WS-COLOR-GREEN TO WS-FG
      *>          MOVE WS-COLOR-WHITE TO WS-FG-CELL
      *>          MOVE WS-COLOR-BLACK TO WS-BG
      *>          INITIALIZE WS-MOVE-OUTCOME
      *>          IF COMPUTER-PLAYER
      *>              INITIALIZE WS-COMPUTER-MOVED
      *>              PERFORM UNTIL COMPUTER-MOVED
      *>                  COMPUTE WS-ROW = FUNCTION RANDOM * 3 + 1
      *>                  END-COMPUTE
      *>                  COMPUTE WS-COL = FUNCTION RANDOM * 3 + 1
      *>                  END-COMPUTE
      *>                  IF WS-CELL(WS-ROW,WS-COL) IS EQUAL TO " "
      *>                  THEN
      *>                      SET WS-COMPUTER-MOVED TO 1
      *>                      MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
      *>                  END-IF
      *>              END-PERFORM
      *>          ELSE
      *>              INITIALIZE WS-NEXT-MOVE
      *>              DISPLAY BOARD-SCREEN

      *>              DISPLAY PIP-BOY-SCREEN
      *>              ACCEPT NEXT-MOVE 

      *>              PERFORM 0113-DISPLAY-TIME-USER-INFO
      *>              ACCEPT NEXT-MOVE

      *>              EVALUATE FUNCTION UPPER-CASE(WS-NEXT-MOVE(1:1))
      *>                  WHEN "A" SET WS-ROW TO 1
      *>                  WHEN "B" SET WS-ROW TO 2
      *>                  WHEN "C" SET WS-ROW TO 3
      *>                  WHEN OTHER MOVE "FAIL" TO WS-MOVE-OUTCOME
      *>              END-EVALUATE
      *>              SET WS-COL TO WS-NEXT-MOVE(2:1)
      *>              IF
      *>                  WS-MOVE-OUTCOME IS NOT EQUAL TO "FAIL"
      *>                  AND WS-COL IS GREATER THAN 0
      *>                  AND WS-COL IS LESS THAN 4
      *>                  AND WS-CELL(WS-ROW,WS-COL) = " "
      *>              THEN
      *>                  MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
      *>              ELSE
      *>                  MOVE "FAIL" TO WS-MOVE-OUTCOME
      *>              END-IF
      *>          END-IF
      *>          MOVE WS-GAME-GRID TO WS-FLAT-GAME-GRID
      *>          IF HUMAN-PLAYER
      *>              INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "1"
      *>              INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "0"
      *>          ELSE
      *>              INSPECT WS-FLAT-GAME-GRID REPLACING ALL "X" BY "0"
      *>              INSPECT WS-FLAT-GAME-GRID REPLACING ALL "O" BY "1"
      *>          END-IF
      *>          INSPECT WS-FLAT-GAME-GRID REPLACING ALL " " BY "0"
      *>          INITIALIZE WS-EOF
      *>          OPEN INPUT FD-WINMASKS
      *>          PERFORM UNTIL EOF OR MOVE-COMPLETE
      *>              READ FD-WINMASKS NEXT RECORD
      *>                  AT END
      *>                      SET WS-EOF TO 1
      *>                  NOT AT END
      *>                      PERFORM VALIDATE-WIN-PARAGRAPH
      *>              END-READ
      *>          END-PERFORM
      *>          CLOSE FD-WINMASKS
      *>          IF NOT MOVE-COMPLETE AND WS-MOVES IS EQUAL TO 8
      *>              MOVE "STALE" TO WS-MOVE-OUTCOME
      *>          END-IF
      *>          INITIALIZE WS-SWAP-PLAYERS
      *>          EVALUATE WS-MOVE-OUTCOME
      *>          WHEN "WIN"
      *>              MOVE "WINNER! (^_^)" TO WS-OANDXMESSAGE
      *>              MOVE "WIN" TO WS-STATE
      *>              SET WS-SWAP-PLAYERS TO 1
      *>          WHEN "LOSE"
      *>              MOVE "YOU DIED (x_x)" TO WS-OANDXMESSAGE
      *>              MOVE "LOSE" TO WS-STATE
      *>              SET WS-SWAP-PLAYERS TO 1
      *>          WHEN "STALE"
      *>              MOVE "Stalemate! (>_<)" TO WS-OANDXMESSAGE
      *>              MOVE "STALE" TO WS-STATE
      *>          WHEN "FAIL"
      *>              MOVE "Invalid move... (o_O)" TO WS-OANDXMESSAGE
      *>          WHEN OTHER
      *>              MOVE "Enter a move" TO WS-OANDXMESSAGE
      *>              SET WS-SWAP-PLAYERS TO 1
      *>              ADD 1 TO WS-MOVES END-ADD
      *>          END-EVALUATE
      *>          IF SWAP-PLAYERS
      *>              IF HUMAN-PLAYER
      *>                  MOVE "O" TO WS-PLAYER
      *>              ELSE
      *>                  MOVE "X" TO WS-PLAYER
      *>              END-IF
      *>          END-IF.

      *>      VALIDATE-WIN-PARAGRAPH.
      *>          INITIALIZE WS-MASK-DETECTED
      *>          SET WS-DETECT-LOOP-COUNT TO 1
      *>          PERFORM 9 TIMES
      *>              IF
      *>                  FD-WINMASK(WS-DETECT-LOOP-COUNT:1)
      *>                  IS EQUAL TO
      *>                  WS-FLAT-GAME-GRID(WS-DETECT-LOOP-COUNT:1)
      *>                  AND IS EQUAL TO 1
      *>              THEN
      *>                  ADD 1 TO WS-MASK-DETECTED END-ADD
      *>              END-IF
      *>              ADD 1 TO WS-DETECT-LOOP-COUNT END-ADD
      *>          END-PERFORM
      *>          IF WIN-DETECTED
      *>              IF HUMAN-PLAYER
      *>                  MOVE "WIN" TO WS-MOVE-OUTCOME
      *>              ELSE
      *>                  MOVE "LOSE" TO WS-MOVE-OUTCOME
      *>              END-IF
      *>          END-IF.

      *>  0210-RANDOM-NUMBER-GAME.
      *>      INITIALIZE RANDOM-NUM-CHOICE.
      *>      INITIALIZE BET-AMOUNT.
      *>      DISPLAY RANDOM-NUM-GAME-SCREEN.
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT BET-FIELD.
      *>      MOVE BET-AMOUNT TO COST.
      *>      COMPUTE WINNINGS = BET-AMOUNT * 2.
      *>      IF WINNINGS = "000" 
      *>      OR (CHECK-LIMIT(WINNINGS, USER-INFO-CREDITS) = "FAIL")
      *>        MOVE "WINNINGS EXCEEDING MAX CREDIT AMOUNT, ACTION ABORTED"
      *>        TO CREDIT-LIMIT-MESSAGE
      *>          PERFORM 0210-RANDOM-NUMBER-GAME
      *>      END-IF.
       
      *>      ACCEPT RANDOM-NUM-CHOICE-FIELD.
      *>      IF (RANDOM-NUM-CHOICE = 's' OR 'S')
      *>      AND (CHECK-BALANCE(BET-AMOUNT, USER-INFO-CREDITS) = "TRUE")
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST,
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          ACCEPT SEED FROM TIME
      *>          COMPUTE ANSWER =
      *>              FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1  
      *>          PERFORM 0211-GAME-LOOP
      *>      ELSE IF (RANDOM-NUM-CHOICE = 's' OR 'S')
      *>      AND (CHECK-BALANCE(BET-AMOUNT, USER-INFO-CREDITS) = "FALSE")
      *>          MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
      *>          PERFORM 0210-RANDOM-NUMBER-GAME
      *>      ELSE IF RANDOM-NUM-CHOICE = 'g' OR 'G'
      *>          PERFORM 0160-GAMES-MENU
      *>      ELSE IF RANDOM-NUM-CHOICE = 'q' OR 'Q'
      *>          STOP RUN
      *>      ELSE
      *>          PERFORM 0210-RANDOM-NUMBER-GAME
      *>      END-IF.
              
      *>  0211-GAME-LOOP.
      *>      INITIALIZE GUESS-INPUT.
      *>      INITIALIZE WS-RANDOM-NUM-MSG.
      *>      INITIALIZE RANDOM-NUM-GUESS-CHOICE.
      *>      DISPLAY GUESS-SCREEN.
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT GUESS-FIELD.
      *>      MOVE GUESS-INPUT TO GUESS
      *>      IF GUESS NOT = ANSWER
      *>          MOVE "Incorrect, you lose."
      *>          TO WS-RANDOM-NUM-MSG
      *>          PERFORM 0212-RESULT-PAGE
      *>      ELSE  
      *>          MOVE "You Win!" TO WS-RANDOM-NUM-MSG
      *>          CALL 'add-credits' USING USER-INFO-NAME, WINNINGS,
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          PERFORM 0212-RESULT-PAGE
      *>      END-IF.
       
      *>  0212-RESULT-PAGE.
      *>      INITIALIZE RANDOM-NUM-GUESS-CHOICE.
      *>      DISPLAY GUESS-SCREEN.
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT RANDOM-NUM-GUESS-CHOICE-FIELD
      *>      IF RANDOM-NUM-GUESS-CHOICE = 'y' OR 'Y'
      *>        PERFORM 0210-RANDOM-NUMBER-GAME
      *>      ELSE IF RANDOM-NUM-GUESS-CHOICE = 'g' OR 'G'
      *>          PERFORM 0160-GAMES-MENU
      *>      ELSE IF RANDOM-NUM-GUESS-CHOICE = 'q' OR 'Q'
      *>          STOP RUN
      *>      END-IF.
 
      *>  0220-GENERATE-LIBRARY-TABLE.
      *>      call 'generate-library-table' USING WS-BOOKS 
      *>      LIBRARY-DISPLAY-MESSAGE OFFSET PAGE-NUM.
      *>      PERFORM 0230-LIBRARY-MENU.

      *>  0230-LIBRARY-MENU.
      *>      INITIALIZE LIBRARY-CHOICE.

      *>      MOVE "10" TO COST.
    
      *>      DISPLAY LIBRARY-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT LIBRARY-FIELD.
      *>      IF LIBRARY-CHOICE = 'q' THEN 
      *>          PERFORM 0120-DISPLAY-MENU
      *>      ELSE IF LIBRARY-CHOICE = 'n' THEN
      *>          IF OFFSET > 10
      *>              COMPUTE OFFSET = OFFSET - 5
      *>              COMPUTE PAGE-NUM = PAGE-NUM + 1
      *>              MOVE 'Here are the next 5 books' TO
      *>                  LIBRARY-DISPLAY-MESSAGE
      *>          END-IF
      *>          PERFORM 0230-LIBRARY-MENU
      *>      ELSE IF LIBRARY-CHOICE = 'p' THEN
      *>          IF PAGE-NUM = '01'
      *>            PERFORM 0230-LIBRARY-MENU
      *>          ELSE IF PAGE-NUM = '02'
      *>            COMPUTE OFFSET = OFFSET + 5
      *>            COMPUTE PAGE-NUM = PAGE-NUM - 1
      *>            MOVE 'Here are the previous 5 books' TO
      *>              LIBRARY-DISPLAY-MESSAGE
      *>            PERFORM 0230-LIBRARY-MENU
      *>          ELSE
      *>            COMPUTE OFFSET = OFFSET + 5
      *>            COMPUTE PAGE-NUM = PAGE-NUM - 1
      *>              PERFORM 0230-LIBRARY-MENU
      *>          END-IF
      *>      ELSE IF (LIBRARY-CHOICE = '1' OR '2' OR '3' OR '4' OR '5')
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          SET LIBRARY-NUM TO LIBRARY-CHOICE-TO-NUM(LIBRARY-CHOICE)
      *>          PERFORM 0240-READ-BOOK
      *>      ELSE IF (LIBRARY-CHOICE = '1' OR '2' OR '3' OR '4' OR '5')
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "FALSE") THEN
      *>          MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
      *>          PERFORM 0230-LIBRARY-MENU
      *>      ELSE
      *>          PERFORM 0230-LIBRARY-MENU
      *>      END-IF. 

      *>  0240-READ-BOOK.
      *>      INITIALIZE READ-CHOICE.
      *>      MOVE "5" TO COST.
      *>      IF LIBRARY-NUM = 1 OR 2 OR 3 OR 4 OR 5
      *>          MOVE DISPLAY-LIBRARY-TITLE(OFFSET LIBRARY-NUM WS-BOOKS)
      *>          TO TITLE
      *>          MOVE DISPLAY-BOOK-BODY(OFFSET LIBRARY-NUM WS-BOOKS)
      *>          TO BODY
      *>          MOVE DISPLAY-BOOK-AUTHOR(OFFSET LIBRARY-NUM WS-BOOKS)
      *>          TO BOOK-AUTHOR
      *>      END-IF.
      *>      MOVE BODY TO WS-READ-BODY-SEGMENTS.
      *>      DISPLAY READ-BOOK-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.
           

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.
      *>      ACCEPT READ-CHOICE.


      *>      IF READ-CHOICE = 'q' THEN
      *>          PERFORM 0230-LIBRARY-MENU
      *>      ELSE IF (READ-CHOICE = 'a' )
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
      *>          UPDATED-BALANCE
      *>          MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
      *>          MOVE "To enable the audiobook feature, please read aloud"
      *>          TO AUDIOBOOK-MSG
      *>          PERFORM 0240-READ-BOOK
      *>      ELSE IF (READ-CHOICE = 'a' )
      *>      AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
      *>          MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
      *>          PERFORM 0230-LIBRARY-MENU
      *>      END-IF.

      *>  0300-CHECK-WEATHER SECTION.
      *>      ACCEPT SEED FROM TIME.
      *>      COMPUTE ANSWER =
      *>          FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1.
      *>      IF ANSWER > 0 AND ANSWER <= 3 
      *>          PERFORM WEATHER-ENVIRONMENT-1
      *>      ELSE IF ANSWER > 3 AND ANSWER <= 6
      *>          PERFORM WEATHER-ENVIRONMENT-2
      *>      ELSE IF ANSWER = 7 OR ANSWER = 8 
      *>          PERFORM WEATHER-ENVIRONMENT-3
      *>      ELSE 
      *>          PERFORM WEATHER-ENVIRONMENT-4
      *>      END-IF. 
           

          *>  WEATHER-ENVIRONMENT-1.
          *>  INITIALIZE W1-CHOICE.
          *>  DISPLAY WEATHER-SCREEN-1.
          *>  DISPLAY PIP-BOY-SCREEN.
          *>  PERFORM 0113-DISPLAY-TIME-USER-INFO.
          *>  ACCEPT W1-CHOICE-FIELD.
          *>  IF W1-CHOICE = 'g' OR 'G' THEN 
          *>      PERFORM 0120-DISPLAY-MENU
          *>  ELSE 
          *>      PERFORM WEATHER-ENVIRONMENT-1 
          *>  END-IF. 

          *>  WEATHER-ENVIRONMENT-2.
          *>  INITIALIZE W2-CHOICE.
          *>  DISPLAY WEATHER-SCREEN-2.
          *>  DISPLAY PIP-BOY-SCREEN.
          *>  PERFORM 0113-DISPLAY-TIME-USER-INFO.
          *>  ACCEPT W2-CHOICE-FIELD.
          *>  IF W2-CHOICE = 'g' OR 'G' THEN 
          *>      PERFORM 0120-DISPLAY-MENU
          *>  ELSE 
          *>      PERFORM WEATHER-ENVIRONMENT-2 
          *>  END-IF. 

          *>  WEATHER-ENVIRONMENT-3.
          *>  INITIALIZE W3-CHOICE.
          *>  DISPLAY WEATHER-SCREEN-3.
          *>  DISPLAY PIP-BOY-SCREEN.
          *>  PERFORM 0113-DISPLAY-TIME-USER-INFO.
          *>  ACCEPT W3-CHOICE-FIELD.
          *>  IF W3-CHOICE = 'g' OR 'G' THEN 
          *>      PERFORM 0120-DISPLAY-MENU
          *>  ELSE 
          *>      PERFORM WEATHER-ENVIRONMENT-3
          *>  END-IF. 

          *>  WEATHER-ENVIRONMENT-4.
          *>  INITIALIZE W4-CHOICE.
          *>  DISPLAY WEATHER-SCREEN-4.
          *>  DISPLAY PIP-BOY-SCREEN.
          *>  PERFORM 0113-DISPLAY-TIME-USER-INFO.
          *>  ACCEPT W4-CHOICE-FIELD.
          *>  IF W4-CHOICE = 'g' OR 'G' THEN 
          *>      PERFORM 0120-DISPLAY-MENU
          *>  ELSE 
          *>      PERFORM WEATHER-ENVIRONMENT-4
          *>  END-IF. 


       0350-TORCH.
           INITIALIZE TORCH-CHOICE.
           DISPLAY TORCH-SCREEN. 
           ACCEPT TORCH-CHOICE-FIELD.
           IF TORCH-CHOICE = 'x' OR 'X' OR 't' OR 'T' THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE 
               PERFORM 0350-TORCH
           END-IF. 

      *>  0400-BUY-CREDITS.
      *>      INITIALIZE CREDIT-AMOUNT.
      *>      INITIALIZE BUY-CREDITS-CHOICE.
      *>      DISPLAY BUY-CREDITS-SCREEN.
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT CREDIT-FIELD.
      *>      ACCEPT BUY-CREDITS-CHOICE-FIELD.
      *>      IF (BUY-CREDITS-CHOICE = 's'or 'S') 
      *>      AND (CHECK-LIMIT(CREDIT-AMOUNT, USER-INFO-CREDITS) = "PASS")
      *>         PERFORM 0450-CONFIRM
      *>      ELSE IF (BUY-CREDITS-CHOICE = 's'or 'S') 
      *>      AND (CHECK-LIMIT(CREDIT-AMOUNT, USER-INFO-CREDITS) = "FAIL")
      *>          MOVE "CREDITS EXCEEDING MAX AMOUNT, TRANSACTION ABORTED"
      *>          TO CREDIT-LIMIT-MESSAGE
      *>          PERFORM 0400-BUY-CREDITS
      *>      ELSE IF BUY-CREDITS-CHOICE = 'g' OR 'G'
      *>          PERFORM 0120-DISPLAY-MENU
      *>      ELSE IF BUY-CREDITS-CHOICE = 'q' OR 'Q' THEN
      *>         STOP RUN  
      *>      ELSE
      *>         PERFORM 0400-BUY-CREDITS
      *>      END-IF.
              
      *>  0450-CONFIRM.
      *>      INITIALIZE CONFIRM-CHOICE
      *>      INITIALIZE PASSWORD-ENTRY
      *>      DISPLAY CONFIRM-SCREEN
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT BUY-PASSWORD-FIELD
      *>      ACCEPT CONFIRM-CHOICE-FIELD
      *>      MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           
      *>       SEARCH WS-USER
      *>           WHEN WS-USER-NAME(USER-IDX) = USER-NAME
      *>               MOVE WS-ACNT-NUM(USER-IDX) TO ACCOUNT-NUM
      *>       END-SEARCH

      *>      IF CONFIRM-CHOICE = ('s' OR 'S') AND 
      *>           VERIFY-PASSWORD(WS-PASSWORD, PASSWORD-ENTRY) = 'TRUE' 
      *>          CALL 'add-to-transactions' USING USER-NAME, 
      *>           ACCOUNT-NUM, CREDIT-AMOUNT, WS-CURRENT-DATE
      *>          PERFORM 0460-PAYMENT-PROCESS
      *>      ELSE IF CONFIRM-CHOICE = ('s' OR 'S') 
      *>        AND VERIFY-PASSWORD(WS-PASSWORD, PASSWORD-ENTRY) = 'FALSE'
      *>        MOVE "INCORRECT PASSWORD" TO INC-PASSWORD
      *>        PERFORM 0450-CONFIRM
      *>      ELSE IF CONFIRM-CHOICE = 'g' OR 'G'
      *>          PERFORM 0400-BUY-CREDITS
      *>      ELSE IF BUY-CREDITS-CHOICE = 'q' OR 'Q' THEN
      *>         STOP RUN 
      *>      ELSE
      *>          PERFORM 0450-CONFIRM
      *>      END-IF.

      *>  0460-PAYMENT-PROCESS.
      *>      INITIALIZE PAY-CONFIRMATION-CHOICE
      *>      DISPLAY PAYMENT-PROCESS-SCREEN
      *>      DISPLAY PIP-BOY-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.
      *>      CALL "CBL_GC_NANOSLEEP" USING 5000000000
      *>      DISPLAY PAY-CONFIRMATION-SCREEN

      *>      DISPLAY PIP-BOY-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT PAY-CONFIRMATION-FIELD
      *>      IF PAY-CONFIRMATION-CHOICE = 'g' OR 'G'
      *>        PERFORM 0120-DISPLAY-MENU
      *>      ELSE IF PAY-CONFIRMATION-CHOICE = 'q' OR 'Q' then
      *>          STOP RUN 
      *>      ELSE 
      *>          DISPLAY PAY-CONFIRMATION-SCREEN
      *>          PERFORM 0113-DISPLAY-TIME-USER-INFO
      *>      END-IF.

      *>  0470-ABOUT-PAGE-TABLE.
      *>      SET COUNTER TO 0. 
      *>      OPEN INPUT F-ABOUT-FILE.
      *>      MOVE 0 TO WS-FILE-IS-ENDED.
      *>      PERFORM UNTIL WS-FILE-IS-ENDED = 1
      *>          READ F-ABOUT-FILE
      *>              NOT AT END
      *>                  ADD 1 TO COUNTER
      *>                  MOVE ABOUT-TITLE TO WS-ABOUT-TITLE(COUNTER)
      *>                  MOVE ABOUT-BODY TO WS-ABOUT-BODY(COUNTER)
      *>              AT END
      *>                  MOVE 1 TO WS-FILE-IS-ENDED
      *>                  MOVE COUNTER TO ABOUT-OFFSET
      *>                  MOVE 1 TO ABOUT-PAGE-NUM
      *>                  MOVE 1 TO ABOUT-NUM
      *>          END-READ
      *>      END-PERFORM.
      *>      CLOSE F-ABOUT-FILE.
      *>      PERFORM 0480-ABOUT-PAGE.

      *>  0480-ABOUT-PAGE.
      *>      INITIALIZE ABOUT-PAGE-CHOICE.
      *>      DISPLAY ABOUT-PAGE-SCREEN.

      *>      DISPLAY PIP-BOY-SCREEN.

      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.

      *>      ACCEPT ABOUT-PAGE-FIELD.
      *>      IF ABOUT-PAGE-CHOICE = 'q' OR 'Q' THEN
      *>          PERFORM 0120-DISPLAY-MENU 
      *>      ELSE IF ABOUT-PAGE-CHOICE = 'n' OR 'N' THEN
      *>          IF ABOUT-OFFSET > 20
      *>              COMPUTE ABOUT-OFFSET = ABOUT-OFFSET - 10
      *>              COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM + 1
      *>          END-IF
      *>          PERFORM 0480-ABOUT-PAGE
      *>      ELSE IF ABOUT-PAGE-CHOICE = 'p' THEN
      *>          IF ABOUT-PAGE-NUM = '01'
      *>              PERFORM 0480-ABOUT-PAGE
      *>          ELSE IF ABOUT-PAGE-NUM = '02'
      *>              COMPUTE ABOUT-OFFSET = ABOUT-OFFSET + 10
      *>              COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM - 1
      *>              PERFORM 0480-ABOUT-PAGE
      *>          ELSE 
      *>              COMPUTE ABOUT-OFFSET = ABOUT-OFFSET + 10
      *>              COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM - 1
      *>              PERFORM 0480-ABOUT-PAGE
      *>          END-IF
      *>      ELSE IF ABOUT-PAGE-CHOICE = "1" OR "2" OR "3" OR "4" OR "5"
      *>        SET ABOUT-NUM TO ABOUT-CHOICE-TO-NUM(ABOUT-PAGE-CHOICE)
      *>        MOVE SPACES TO ABOUT-INVALID-CHOICE-MESSAGE
      *>        PERFORM 0490-ABOUT-PAGE-READ
      *>      ELSE
      *>        MOVE "Invalid Choice!" TO ABOUT-INVALID-CHOICE-MESSAGE
      *>        PERFORM 0480-ABOUT-PAGE 
      *>      END-IF.
           

      *>  0490-ABOUT-PAGE-READ.
      *>      INITIALIZE ABOUT-PAGE-READ-CHOICE.
      *>      IF ABOUT-NUM = 1 OR 2 OR 3 OR 4 OR 5
      *>          MOVE DISPLAY-ABOUT-TITLE(ABOUT-OFFSET ABOUT-NUM WS-ABOUT) 
      *>          TO ABOUT-TITLE-READ
      *>          MOVE DISPLAY-ABOUT-BODY(ABOUT-OFFSET ABOUT-NUM WS-ABOUT)
      *>          TO ABOUT-BODY-READ
      *>      END-IF.
      *>      DISPLAY ABOUT-PAGE-READ-SCREEN.
      *>      PERFORM 0113-DISPLAY-TIME-USER-INFO.
      *>      ACCEPT ABOUT-PAGE-READ-FIELD.
      *>      IF ABOUT-PAGE-READ-CHOICE = "q" or "Q"
      *>          MOVE SPACES TO ABOUT-INVALID-CHOICE-MESSAGE
      *>          PERFORM 0480-ABOUT-PAGE
      *>      ELSE 
      *>          MOVE "Invalid Choice!" TO ABOUT-INVALID-CHOICE-MESSAGE
      *>          PERFORM 0490-ABOUT-PAGE-READ              
      *>      END-IF.
           
       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.

       0600-CHANGE-PASSWORD SECTION.
           MOVE SPACES TO PWORD-ERR-1.
           MOVE SPACES TO PWORD-ERR-2.
           MOVE SPACES TO PWORD-ERR-3.
           MOVE SPACES TO PWORD-OK-1.
           MOVE SPACES TO PWORD-OK-2.
           MOVE SPACES TO PWORD-OK-3.
           MOVE SPACES TO PWORD-CONFIRM-MSG.

           VALIDATE-CURRENT-PASSWORD. 
           INITIALIZE OLD-PASSWORD.
           INITIALIZE UPDATED-PASSWORD.
           INITIALIZE CONFIRM-NEW-PASSWORD.
           INITIALIZE CHANGE-PWORD-CHOICE.
           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT OLD-PASSWORD-FIELD.
           IF OLD-PASSWORD = WS-PASSWORD THEN 
               MOVE "PASSWORD ACCEPTED" TO PWORD-OK-1
               MOVE SPACES TO PWORD-ERR-1
               PERFORM VALIDATE-NEW-PASSWORD
           ELSE 
               MOVE "INCORRECT PASSWORD" TO PWORD-ERR-1
               PERFORM VALIDATE-CURRENT-PASSWORD
           END-IF. 

           VALIDATE-NEW-PASSWORD. 
           INITIALIZE UPDATED-PASSWORD.
           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT UPDATED-PASSWORD-FIELD. 
           CALL 'validate-password' USING UPDATED-PASSWORD PWORD-ERR-2 
           RAISE-ERROR PWORD-OK-2.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-NEW-PASSWORD
           ELSE 
               PERFORM SECOND-VALIDATION-NEW-PASSWORD
           END-IF. 

           SECOND-VALIDATION-NEW-PASSWORD.
           INITIALIZE CONFIRM-NEW-PASSWORD.
           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CONFIRM-NEW-PASSWORD-FIELD.
           IF UPDATED-PASSWORD = CONFIRM-NEW-PASSWORD
               MOVE "PASSWORD MATCH" TO PWORD-OK-3
               MOVE SPACES TO PWORD-ERR-3
           ELSE 
               MOVE "PASSWORDS DO NOT MATCH" TO PWORD-ERR-3
               PERFORM SECOND-VALIDATION-NEW-PASSWORD
           END-IF. 

           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-PWORD-FIELD. 
           IF CHANGE-PWORD-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE IF CHANGE-PWORD-CHOICE = "s" OR "S" THEN 
               CALL 'update-password' USING USER-NAME 
               UPDATED-PASSWORD
               MOVE 'PASSWORD SUCCESSFULLY UPDATED' TO PWORD-CONFIRM-MSG
           END-IF. 
           
           INITIALIZE CHANGE-PWORD-CHOICE.
           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-PWORD-FIELD.
           IF CHANGE-PWORD-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE 
               PERFORM 0120-DISPLAY-MENU
           END-IF. 

       0650-CHANGE-ACCOUNT-NUM SECTION.
           MOVE SPACES TO PWORD-ERROR.
           MOVE SPACES TO PWORD-ERR-1.
           MOVE SPACES TO PWORD-ERR-2.
           MOVE SPACES TO PWORD-OK.
           MOVE SPACES TO ACNT-OK-1.
           MOVE SPACES TO ACNT-OK-2.
           MOVE SPACES TO ACNT-CONFIRM-MSG.

           VALIDATE-USER-PASSWORD. 
           INITIALIZE CHECK-PASSWORD.
           INITIALIZE UPDATED-ACNT.
           INITIALIZE CONFIRM-ACNT.
           INITIALIZE CHANGE-ACNT-CHOICE.
           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHECK-PASSWORD-FIELD.
           IF CHECK-PASSWORD = WS-PASSWORD THEN 
               MOVE "PASSWORD ACCEPTED" TO PWORD-OK
               MOVE SPACES TO PWORD-ERROR
               PERFORM VALIDATE-NEW-ACCOUNT
           ELSE 
               MOVE "INCORRECT PASSWORD" TO PWORD-ERROR
               PERFORM VALIDATE-USER-PASSWORD
           END-IF. 

           VALIDATE-NEW-ACCOUNT. 
           INITIALIZE UPDATED-ACNT.
           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT UPDATED-ACNT-FIELD. 
           CALL 'validate-bank-details' USING UPDATED-ACNT ACNT-ERR-1 
           RAISE-ERROR ACNT-OK-1.
           IF RAISE-ERROR > 0 
               PERFORM VALIDATE-NEW-ACCOUNT
           ELSE 
               PERFORM SECOND-VALIDATION-ACCOUNT
           END-IF. 

           SECOND-VALIDATION-ACCOUNT.
           INITIALIZE CONFIRM-ACNT.
           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CONFIRM-ACNT-FIELD.
           IF UPDATED-ACNT = CONFIRM-ACNT
               MOVE "ACCOUNT NUMBERS MATCH" TO ACNT-OK-2
               MOVE SPACES TO ACNT-ERR-2
           ELSE 
               MOVE "ACCOUNT NUMBER DOES NOT MATCH" TO ACNT-ERR-2
               PERFORM SECOND-VALIDATION-ACCOUNT
           END-IF. 

           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-ACNT-FIELD. 
           IF CHANGE-ACNT-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE IF CHANGE-ACNT-CHOICE = "s" OR "S" THEN 
               CALL 'update-account-num' USING USER-NAME 
               UPDATED-ACNT
               MOVE 'ACCOUNT NUMBER SUCCESSFULLY UPDATED' TO 
               ACNT-CONFIRM-MSG
           END-IF. 
           
           INITIALIZE CHANGE-ACNT-CHOICE.
           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-ACNT-FIELD.
           IF CHANGE-ACNT-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE 
               PERFORM 0120-DISPLAY-MENU
           END-IF. 
