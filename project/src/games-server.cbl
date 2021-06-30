       IDENTIFICATION DIVISION.
       PROGRAM-ID. games-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.

           FUNCTION HIGH-SCORE-CALCULATOR
           FUNCTION REPLACE-LETTER

           FUNCTION CHECK-BALANCE
           FUNCTION CHECK-LIMIT.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
            *>----- Hangman file control -----
           SELECT F-WORD-FILE ASSIGN TO 'guessing-words.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F-HIGH-SCORES-FILE ASSIGN TO 'high-scores.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
          
           *>----- X AND O File Control-----    
             SELECT FD-WINMASKS ASSIGN TO "placement.dat"
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
          *>----- Hangman F-Section-----
           FD F-WORD-FILE.
           01 WORD PIC X(20).

           FD F-HIGH-SCORES-FILE.
           01 PLAYER-SCORES.
              05 HIGH-SCORE PIC 99.
              05 PLAYER-NAME PIC X(10).

           *>----- X AND O F-Section-----   
           FD FD-WINMASKS.
           01 FD-WINMASK PIC X(9).

                
           WORKING-STORAGE SECTION.

           01 COST PIC 999.
           01 UPDATED-BALANCE PIC 999.
           01 INSUFFICIENT-FUNDS PIC X(20).
           01 USER-INFO-LOGGED-IN PIC X(15) VALUE "Logged in as:".
           01 CREDIT-LIMIT-MESSAGE PIC X(65).
           01 COUNTER UNSIGNED-INT.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   
           
           *>----- Arcade Variables -----

           01 GAMES-MENU-CHOICE PIC X.

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
           01 BET-AMOUNT PIC 999.
           01 WINNINGS PIC 999.
           01 RANDOM-NUM-CHOICE PIC X.
           01 RANDOM-NUM-GUESS-CHOICE PIC X.
           01 WS-RANDOM-NUM-MSG PIC X(40).


           *>---- Hangman Variables ----
           01 WS-ANSWERWORD PIC X(20).
           01 RANDOMNUMBER PIC 99.
           01 WS-WORD PIC X(20).
           01 WS-GUESSING-CHOICE-WORDS.
               05 WS-GUESSING-CHOICE-WORD OCCURS 213 TIMES
               DESCENDING KEY IS WS-GUESSING-WORDS-WORD
               INDEXED BY WORD-IDX.
                   10 WS-GUESSING-WORDS-WORD PIC X(20).
           01 WS-GUESS-CHOICE PIC X(20).

          *> High score screen
           01 WS-HIGH-SCORE-CHOICE PIC X.
           01 WS-HIGH-SCORE PIC 99.
           01 WS-HIGH-SCORES.  
              05 WS-TABLE-HIGH-SCORE OCCURS 100 TIMES     
              ASCENDING KEY IS WS-SCORE
              INDEXED BY SCORE-IDX.
                  10 WS-SCORE PIC 99.
                  10 WS-NAME PIC X(10).

          *> Checking guesses  
           01 WS-LETTERS-LEFT PIC 99.
           01 WS-GUESSES-LEFT PIC 99.          

          *> Winning and losing.
           01 WS-GUESSING-LOSING-CHOICE PIC X.
           01 WS-GUESSING-WINNING-CHOICE PIC X.
           01 WS-WORD-LENGTH PIC 99.

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
           "|   |-------|   ||".
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
      -    "____|-----------|_______________________________________/".
                 05 LINE 47 COL 10 VALUE
           "   \_________/     |===|                                    
      -    "    |-----------|                                      /".
                 05 LINE 48 COL 10 VALUE
           "             \_____|___/____________________________________
      -    "____|||||||||||||_____________________________________/".
                 05 LINE 50 COL 10 VALUE
           "============================================================
      -    "==========================================================="
           .  

   

           01 GAMES-MENU-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.
               05 LINE 15 COL 30 VALUE " ____" FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 70 VALUE "   ,##.                   ,==."
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 30 VALUE   
               "|  __ \                         " FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 70 VALUE   
               " ,#    #.                 \ o '," FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 30 VALUE
              "| |  \/ __ _ _ __ ___   ___ ___ " FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 70 VALUE
               "#        #     _     _     \    \" 
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 30 VALUE 
               "| | __ / _` | '_ ` _ \ / _ / __|"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 70 VALUE 
               "#        #    (_)   (_)    /    ; "
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 30 VALUE
               "| |_\ | (_| | | | | | |  __\__ \"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 70 VALUE
               " `#    #'                 /   .'  "
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 30 VALUE
               " \____/\__,_|_| |_| |_|\___|___/"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 70 VALUE
               "   `##'                   '=='"
               FOREGROUND-COLOR IS 2.
               05 LINE 26 COL 43 VALUE "Games cost 5 credits: "
               FOREGROUND-COLOR IS 2.
               05 LINE 28 COL 43 VALUE "(h) Hangman"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 30 COL 43 VALUE "(n) Guess The Number" 
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 32 COL 43 VALUE "(o) O and X         "  
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 36 VALUE "(g) Go back "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 54 VALUE "(q) Quit    "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 38 COL 36 VALUE "Pick: "
               FOREGROUND-COLOR IS 2.
               05 GAMES-MENU-CHOICE-FIELD LINE 38 COL 41 PIC X
               USING GAMES-MENU-CHOICE.              
               05 LINE 40 COL 36 PIC X(20) USING INSUFFICIENT-FUNDS
               HIGHLIGHT, FOREGROUND-COLOR IS 4.
      
           01 BOARD-SCREEN.
               05 BLANK SCREEN.
               05 LINE 14 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 15 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 16 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.
               05 LINE 18 COl 60 VALUE  "  ___       _    _   _ ____   _
      -        "_  __" FOREGROUND-COLOR IS 3.
               05 LINE 19 COl 60 VALUE " / _ \     / \  | \ | |  _ \  \ 
      -        "\/ /" FOREGROUND-COLOR IS 5.
               05 LINE 20 COl 60 VALUE "| | | |   / _ \ |  \| | | | |  \
      -        "  /" FOREGROUND-COLOR IS 3.
               05 LINE 21 COl 60 VALUE "| |_| |  / ___ \| |\  | |_| |  /
      -         "  \" FOREGROUND-COLOR IS 2.
               05 LINE 22 COl 60 VALUE " \___/  /_/   \_\_| \_|____/  /_
      -        "/\_\" FOREGROUND-COLOR IS 5.
               05 LINE 25 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 26 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 27 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.
               05 LINE 28 COLUMN 67 VALUE IS "   +---+---+---+   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 29 COLUMN 67 VALUE IS " A |   |   |   |   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 30 COLUMN 67 VALUE IS "   +---+---+---+   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 31 COLUMN 67 VALUE IS " B |   |   |   |   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 32 COLUMN 67 VALUE IS "   +---+---+---+   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 33 COLUMN 67 VALUE IS " C |   |   |   |   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 34 COLUMN 67 VALUE IS "   +---+---+---+   "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 35 COLUMN 67 VALUE IS "     1   2   3     "
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG.
               05 LINE 29 COLUMN 72 PIC A(1) FROM WS-CELL(1,1)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 29 COLUMN 76 PIC A(1) FROM WS-CELL(1,2)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 29 COLUMN 80 PIC A(1) FROM WS-CELL(1,3)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 31 COLUMN 72 PIC A(1) FROM WS-CELL(2,1)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 31 COLUMN 76 PIC A(1) FROM WS-CELL(2,2)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 31 COLUMN 80 PIC A(1) FROM WS-CELL(2,3)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 33 COLUMN 72 PIC A(1) FROM WS-CELL(3,1)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 33 COLUMN 80 PIC A(1) FROM WS-CELL(3,2)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 33 COLUMN 80 PIC A(1) FROM WS-CELL(3,3)
               BACKGROUND-COLOR WS-BG FOREGROUND-COLOR WS-FG-CELL.
               05 LINE 35 COLUMN 67 VALUE IS "Message: "
               FOREGROUND-COLOR IS 2.
               05 MSG PIC X(128) FROM WS-OANDXMESSAGE
               FOREGROUND-COLOR IS 2.
               05 LINE 36 COLUMN 67 PIC X(16) FROM WS-INSTRUCTION
               FOREGROUND-COLOR IS 2.
               05 NEXT-MOVE PIC X(2) USING WS-NEXT-MOVE.
               05 LINE 38 COLUMN 67 VALUE IS "Moves played = "
               FOREGROUND-COLOR IS 2.
               05 MOVES PIC 9(1) FROM WS-MOVES.
               05 LINE 39 COLUMN 67 VALUE IS "Games won = "
               FOREGROUND-COLOR IS 2.
               05 WINS PIC 9(2) FROM WS-WINS.
               05 LINE 39 COLUMN 81 VALUE IS "/" FOREGROUND-COLOR IS 2.
               05 GAMES PIC 9(2) FROM WS-GAMES. 
               05 LINE 41 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 42 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 43 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.

           01 IN-GAME-SCREEN
               BACKGROUND-COLOR IS 8.
               05 BLANK SCREEN. 
               05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 31 COLUMN 30 VALUE "You wander into a small settl
      -        "ement, seeking shelter from the pounding sun of The Wast
      -        "eland."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 32 COLUMN 30 VALUE "The local Lawman mistakes you
      -        " for a bandit. You're tied up and on the gallows faster"
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 33 COLUMN 30 VALUE "than you can wish the townsfo
      -        "lk a friendly wasteland hello."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 34 COLUMN 30 VALUE "You've Yee'd your last Haw."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 35 COLUMN 30 VALUE "Guess this word to break free
      -        ":" HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 36 COLUMN 30 PIC X(20) USING WS-WORD
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 LINE 37 COLUMN 30 VALUE "Guesses left: "
               HIGHLIGHT FOREGROUND-COLOR 3.
               05 LINE 37 COLUMN 60 PIC 99 USING WS-GUESSES-LEFT.
               05 LINE 38 COLUMN 30 VALUE "( ) Enter a letter to guess"
               HIGHLIGHT FOREGROUND-COLOR 3.
               05 LINE 39 COLUMN 30 VALUE "(!) Quit game"
               HIGHLIGHT FOREGROUND-COLOR 3.
               05 LINE 40 COLUMN 30 VALUE "Pick: " FOREGROUND-COLOR 2.
               05 WS-GUESS-CHOICE-FIELD LINE 40 COLUMN 36 PIC X
               USING WS-GUESS-CHOICE.
               05 LINE 13 COL 70 VALUE " ___________.._______" 
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 70 VALUE "| .__________))______|"
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 70 VALUE "| | / /      ||"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 70 VALUE "| |/ /       ||"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 70 VALUE "| | /        ||.-''."
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 70 VALUE "| |/         |/  _  \"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 70 VALUE "| |          ||  `/,|"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 70 VALUE "| |          (\\`_.'"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 70 VALUE "| |         .-`--'."
               FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 70 VALUE "| |        /Y . . Y\"
               FOREGROUND-COLOR IS 2.
               05 LINE 23 COL 70 VALUE "| |       // |   | \\"
               FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 70 VALUE "| |      //  | . |  \\"
               FOREGROUND-COLOR IS 2.
               05 LINE 25 COL 70 VALUE "| |     ')   |   |   (`"
               FOREGROUND-COLOR IS 2.
               05 LINE 26 COL 70 VALUE "| |          ||'||"
               FOREGROUND-COLOR IS 2.
               05 LINE 27 COL 70 VALUE "| |          || ||"
               FOREGROUND-COLOR IS 2.

           01 WORD-GUESSING-WINNING-SCREEN
               BACKGROUND-COLOR IS 8.
               05 BLANK SCREEN.
               05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 31 COLUMN 30 VALUE "You broke free and escaped to
      -       " The Wasteland!"
               HIGHLIGHT, FOREGROUND-COLOR 6.
               05 LINE 40 COLUMN 30 VALUE "You guessed the word!".
               05 LINE 42 COLUMN 30 PIC X(20) USING WS-ANSWERWORD.
               05 LINE 48 COLUMN 30 PIC 99 USING WS-GUESSES-LEFT.
               05 LINE 50 COLUMN 30 VALUE "You scored: ".
               05 LINE 48 COLUMN 90 PIC 99 USING WS-HIGH-SCORE.
               05 LINE 52 COLUMN 30 VALUE "(p) Play Again"
               HIGHLIGHT FOREGROUND-COLOR IS 6.
               05 LINE 53 COLUMN 30 VALUE "(h) See High Scores"
               HIGHLIGHT FOREGROUND-COLOR IS 6.
               05 LINE 54 COLUMN 30 VALUE "(!) Quit game"
               HIGHLIGHT FOREGROUND-COLOR IS 6.
               05 LINE 55 COLUMN 30 VALUE "Pick: " 
               FOREGROUND-COLOR IS 2.
               05 WS-GUESSING-CHOICE-WINNING-FIELD LINE 55 COLUMN 36 
               PIC X USING WS-GUESSING-WINNING-CHOICE.
               05 LINE 15 COL 70 VALUE "               ,'-',"
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 70 VALUE  "              :-----:"
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 70 VALUE "          (''' , - , ''')"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 70 VALUE "          \   ' .  , `  /"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 70 VALUE "           \  '   ^  ? /"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 70 VALUE "            \ `   -  ,'"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 70 VALUE "             `j_ _,'"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 70 VALUE "        ,- -`\ \  /f"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 70 VALUE "      ,-      \_\/_/'-"
               FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 70 VALUE "     ,                 `,"
               FOREGROUND-COLOR IS 2.

           01 WORD-GUESSING-LOSE-SCREEN
               BACKGROUND-COLOR IS 8.
               05 BLANK SCREEN.
               05 LINE 13 COL 30 VALUE "       ______"
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 30 VALUE "    .-'      '-."
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 30 VALUE "   /            \"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 30 VALUE "  |              |"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 30 VALUE "  |,  .-.  .-.  ,|"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 30 VALUE "  | )(__/  \__)( |"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 30 VALUE "  |/     /\     \|"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 30 VALUE "  (_     ^^     _)"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 30 VALUE "   \__|IIIIII|__/"
               FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 30 VALUE "    | \IIIIII/ |"
               FOREGROUND-COLOR IS 2.
               05 LINE 23 COL 30 VALUE "    \          /|"
               FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 30 VALUE "     `--------`"
               FOREGROUND-COLOR IS 2.
               05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 26 COLUMN 30 VALUE "You've been fed to the mudcra
      -        "bs" HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 30 COLUMN 30 PIC X(20) USING WS-WORD
               HIGHLIGHT, FOREGROUND-COLOR IS 2.
               05 LINE 28 COLUMN 30 VALUE "The correct word was:".
               05 LINE 30 COLUMN 45 PIC X(20) USING WS-ANSWERWORD 
               HIGHLIGHT, FOREGROUND-COLOR IS 2.
               05 LINE 32 COLUMN 30 VALUE "Guesses left: ".
               05 LINE 32 COLUMN 50 PIC 99 USING WS-GUESSES-LEFT.
               05 LINE 33 COLUMN 30 VALUE "(p) Play again" 
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 34 COLUMN 30 VALUE "(h) See high scores"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 35 COLUMN 30 VALUE "(!) Quit game"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 36 COLUMN 30 VALUE "Pick: " 
               FOREGROUND-COLOR IS 3.
               05 WS-GUESSING-CHOICE-LOSE-FIELD LINE 36 COLUMN 36 PIC X
               USING WS-GUESSING-LOSING-CHOICE.

           01 HIGH-SCORE-SCREEN
               BACKGROUND-COLOR IS 8.
               05 BLANK SCREEN.
               05 LINE 13 COL 70 VALUE "   .-=========-."
               FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 70 VALUE "   \'-=======-'/"
               FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 70 VALUE "   _|   .=.   |_"
               FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 70 VALUE "  ((|  {{1}}  |))"
               FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 70 VALUE "   \|   /|\   |/"
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 70 VALUE "    \__ '`' __/"
               FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 70 VALUE "      _`) (`_"
               FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 70 VALUE "    _/_______\_"
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 70 VALUE "   /___________\"
               FOREGROUND-COLOR IS 2.
               05 LINE 30 COLUMN 30 VALUE "HANGMAN..."
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 31 COLUMN 30 VALUE "WASTELAND LEGENDS:"
               HIGHLIGHT, FOREGROUND-COLOR 2.
               05 LINE 34 COLUMN 30 VALUE "High Scores:".
               05 LINE 36 COLUMN 30 PIC XX USING WS-SCORE(1).
               05 LINE 36 COLUMN 34 PIC X(10) USING WS-NAME(1).
               05 LINE 38 COLUMN 30 PIC XX USING WS-SCORE(2).
               05 LINE 38 COLUMN 34 PIC X(10) USING WS-NAME(2).
               05 LINE 40 COLUMN 30 PIC XX USING WS-SCORE(3).
               05 LINE 40 COLUMN 34 PIC X(10) USING WS-NAME(3).
               05 LINE 42 COLUMN 30 VALUE "(g) Go back"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 44 COLUMN 30 VALUE "Pick: ".
               05 WS-HIGH-SCORE-FIELD LINE 44 COLUMN 36 PIC X
               USING WS-HIGH-SCORE-CHOICE.
           
           01 RANDOM-NUM-GAME-SCREEN.
               05 BLANK SCREEN.
               05 LINE 14 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 15 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 16 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.
               05 LINE 18 COLUMN 34 VALUE "Place a bet and guess a numbe
      -        "r between 1 and 10" FOREGROUND-COLOR IS 2.
               05 LINE 20 COLUMN 34 VALUE "Bet: " FOREGROUND-COLOR IS 2.
               05 BET-FIELD LINE 20 COLUMN 39 PIC 999 USING BET-AMOUNT
               FOREGROUND-COLOR IS 2.
               05 LINE 21 COLUMN 34 PIC X(20) USING INSUFFICIENT-FUNDS
               HIGHLIGHT FOREGROUND-COLOR IS 4.
               05 LINE 24 COL 34 VALUE "(s) Submit " 
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 34 VALUE "(g) Go back"
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 26 COL 34 VALUE "(q) Quit   "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 28 COL 34 VALUE "Pick: " FOREGROUND-COLOR IS 2.
               05 RANDOM-NUM-CHOICE-FIELD LINE 28 COL 40 PIC X 
               USING RANDOM-NUM-CHOICE.
               05 LINE 29 COLUMN 34 PIC X(65) 
               USING CREDIT-LIMIT-MESSAGE 
               HIGHLIGHT FOREGROUND-COLOR IS 4.
               05 LINE 35 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 37 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.

           01 GUESS-SCREEN.
               05 BLANK SCREEN.
               05 LINE 14 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 3.
               05 LINE 15 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 16 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.
               05 LINE 18 COLUMN 34 VALUE IS "Potential winnings: "
               FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 54 PIC 999 USING WINNINGS.
               05 GUESS-FIELD LINE 20 COLUMN 34 PIC XX 
               USING GUESS-INPUT.
               05 LINE 22 COLUMN 34 PIC X(40) USING WS-RANDOM-NUM-MSG
               FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 34 VALUE "(y) Play again "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 25 COL 34 VALUE "(g) Go back "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 26 COL 34 VALUE "(q) Quit   "
               HIGHLIGHT FOREGROUND-COLOR IS 3.
               05 LINE 28 COL 34 VALUE "Pick: " FOREGROUND-COLOR IS 2.
               05 RANDOM-NUM-GUESS-CHOICE-FIELD LINE 28 COL 40 PIC X 
               USING RANDOM-NUM-GUESS-CHOICE.
               05 LINE 35 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -         "----" FOREGROUND-COLOR IS 3.
               05 LINE 36 COL 30 VALUE "********************************
      -        "********************************************************
      -        "****" FOREGROUND-COLOR IS 5.
               05 LINE 37 COL 30 VALUE "--------------------------------
      -        "--------------------------------------------------------
      -        "----" FOREGROUND-COLOR IS 2.
               
       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0160-GAMES-MENU.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE GAMES-MENU-CHOICE.
           MOVE "5" TO COST.
           DISPLAY GAMES-MENU-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT GAMES-MENU-CHOICE-FIELD
           IF GAMES-MENU-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF GAMES-MENU-CHOICE = "g" or "G" THEN
               GOBACK
           ELSE IF (GAMES-MENU-CHOICE = "o" OR "O" )
           AND (CHECK-BALANCE (COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               MOVE SPACES TO INSUFFICIENT-FUNDS
               PERFORM 0190-O-AND-X-GAME  
           ELSE IF (GAMES-MENU-CHOICE = "h" or "H") 
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               MOVE SPACES TO INSUFFICIENT-FUNDS
               PERFORM 0170-DISPLAY-GUESSING-GAME
           ELSE IF (GAMES-MENU-CHOICE = "n" or "N")  
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               MOVE SPACES TO INSUFFICIENT-FUNDS
               PERFORM 0200-RANDOM-NUMBER-GAME           
           END-IF.

           IF CHECK-BALANCE(COST, USER-INFO-CREDITS) = "FALSE"
               MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
           PERFORM 0160-GAMES-MENU.


       0170-DISPLAY-GUESSING-GAME.
           PERFORM 0500-TIME-AND-DATE.
           SET WS-HIGH-SCORE TO 0.
           SET WS-WORD-LENGTH TO 0.
           MOVE 15 TO WS-GUESSES-LEFT.
           SET WORD-IDX TO 0.
           OPEN INPUT F-WORD-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-WORD-FILE
                   NOT AT END
                       ADD 1 TO WORD-IDX
                       MOVE WORD TO WS-GUESSING-WORDS-WORD(WORD-IDX)
                   AT END
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ
           END-PERFORM.
           CLOSE F-WORD-FILE.

           MOVE FUNCTION CURRENT-DATE(14:3) TO RANDOMNUMBER.
           MOVE WS-GUESSING-WORDS-WORD(RANDOMNUMBER) TO WS-WORD.
           MOVE WS-WORD TO WS-ANSWERWORD.
           MOVE REPLACE-LETTER(WS-WORD) TO WS-WORD. 
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
               IF '*' EQUALS WS-WORD(COUNTER:1) 
                   THEN ADD 1 TO WS-WORD-LENGTH
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM.
           PERFORM 0175-IN-GAME-SCREEN.

       0175-IN-GAME-SCREEN.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE WS-GUESS-CHOICE.
           DISPLAY IN-GAME-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT WS-GUESS-CHOICE-FIELD.
           IF WS-GUESS-CHOICE = '!' THEN 
               PERFORM 0160-GAMES-MENU
           ELSE
               PERFORM 0180-CHECK-GUESS
           END-IF.

       0180-CHECK-GUESS.
           PERFORM 0500-TIME-AND-DATE.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER = 20
                 IF WS-GUESS-CHOICE = WS-ANSWERWORD(COUNTER:1) 
                 THEN
                      MOVE WS-GUESS-CHOICE TO WS-WORD(COUNTER:1) 
                 END-IF
                 ADD 1 TO COUNTER     
           END-PERFORM.

           SUBTRACT 1 FROM WS-GUESSES-LEFT.
           MOVE 1 TO COUNTER.
           MOVE 0 TO WS-LETTERS-LEFT.
           PERFORM UNTIL COUNTER = 20
               IF '*' EQUALS WS-WORD(COUNTER:1) 
                   THEN ADD 1 TO WS-LETTERS-LEFT
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM.

           IF WS-LETTERS-LEFT = 0
              PERFORM 0185-WINNING-SCREEN
           ELSE IF WS-GUESSES-LEFT = 0
              PERFORM 0186-LOSING-SCREEN
           ELSE
              PERFORM 0175-IN-GAME-SCREEN
           END-IF.

       0185-WINNING-SCREEN.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE WS-GUESSING-WINNING-CHOICE.
           DISPLAY WS-WORD-LENGTH.
           DISPLAY WS-GUESSES-LEFT.
           DISPLAY WS-HIGH-SCORE.
           MOVE HIGH-SCORE-CALCULATOR(WS-WORD-LENGTH WS-GUESSES-LEFT)
           TO WS-HIGH-SCORE.
           DISPLAY WS-WORD-LENGTH.
           DISPLAY WS-GUESSES-LEFT.
           DISPLAY WS-HIGH-SCORE.
           DISPLAY WORD-GUESSING-WINNING-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           OPEN EXTEND F-HIGH-SCORES-FILE
               MOVE WS-HIGH-SCORE TO HIGH-SCORE
               MOVE USER-INFO-NAME TO PLAYER-NAME
               WRITE PLAYER-SCORES 
               END-WRITE.
           CLOSE F-HIGH-SCORES-FILE.

           ACCEPT WS-GUESSING-CHOICE-WINNING-FIELD.
           IF WS-GUESSING-WINNING-CHOICE = 'p'
               PERFORM 0170-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-WINNING-CHOICE = 'h'
               PERFORM 0187-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-WINNING-CHOICE = '!'
               PERFORM 0160-GAMES-MENU
           ELSE
             PERFORM 0185-WINNING-SCREEN
           END-IF.

       0186-LOSING-SCREEN.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE WS-GUESSING-LOSING-CHOICE.
           DISPLAY WORD-GUESSING-LOSE-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT WS-GUESSING-LOSING-CHOICE.
           IF WS-GUESSING-LOSING-CHOICE = 'p'
               PERFORM 0170-DISPLAY-GUESSING-GAME
           ELSE IF WS-GUESSING-LOSING-CHOICE = 'h'
               PERFORM 0187-HIGH-SCORE-TABLE
           ELSE IF WS-GUESSING-LOSING-CHOICE = '!'
               PERFORM 0160-GAMES-MENU
           ELSE
             PERFORM 0186-LOSING-SCREEN
           END-IF.

       0187-HIGH-SCORE-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-HIGH-SCORES-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-HIGH-SCORES-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE HIGH-SCORE TO WS-SCORE(COUNTER)
                       MOVE PLAYER-NAME TO WS-NAME(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-HIGH-SCORES-FILE.
           PERFORM 0188-HIGH-SCORE-SCREEN.

       0188-HIGH-SCORE-SCREEN.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE WS-HIGH-SCORE-CHOICE.
           SORT WS-TABLE-HIGH-SCORE ON DESCENDING WS-SCORE.
           DISPLAY HIGH-SCORE-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT WS-HIGH-SCORE-FIELD.
           IF WS-HIGH-SCORE-CHOICE = 'g'
               GOBACK
           ELSE 
               PERFORM 0188-HIGH-SCORE-SCREEN
           END-IF.

           *>----- X AND O Procedure Div------    
       0190-O-AND-X-GAME.
           MOVE "X" TO WS-PLAYER
           PERFORM 0191-GAME-LOOP-PARAGRAPH
               WITH TEST AFTER UNTIL FINISHED-PLAYING
           PERFORM 0160-GAMES-MENU.

       0191-GAME-LOOP-PARAGRAPH.
           INITIALIZE WS-GAME-GRID
           INITIALIZE WS-STATE
           INITIALIZE WS-MOVES
           MOVE "Make a move like 'A2'" TO WS-OANDXMESSAGE
           PERFORM 0192-GAME-FRAME-PARAGRAPH
           WITH TEST AFTER UNTIL GAME-OVER
           ADD 1 TO WS-GAMES 
           EVALUATE WS-STATE
               WHEN "WIN"
                   ADD 1 TO WS-WINS 
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
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO
           ACCEPT NEXT-MOVE.
           

       0192-GAME-FRAME-PARAGRAPH.
           MOVE "Move to square: " TO WS-INSTRUCTION
           MOVE WS-COLOR-GREEN TO WS-FG
           MOVE WS-COLOR-WHITE TO WS-FG-CELL
           MOVE WS-COLOR-BLACK TO WS-BG
           INITIALIZE WS-MOVE-OUTCOME
           IF COMPUTER-PLAYER
               INITIALIZE WS-COMPUTER-MOVED
               PERFORM UNTIL COMPUTER-MOVED
                   COMPUTE WS-ROW = FUNCTION RANDOM * 3 + 1
                   COMPUTE WS-COL = FUNCTION RANDOM * 3 + 1
                   IF WS-CELL(WS-ROW,WS-COL) IS EQUAL TO " "
                       SET WS-COMPUTER-MOVED TO 1
                       MOVE WS-PLAYER TO WS-CELL(WS-ROW,WS-COL)
                   END-IF
               END-PERFORM
           ELSE
               INITIALIZE WS-NEXT-MOVE
               DISPLAY BOARD-SCREEN
               DISPLAY PIP-BOY-SCREEN 
               PERFORM 0113-DISPLAY-TIME-USER-INFO
               ACCEPT NEXT-MOVE
               EVALUATE FUNCTION UPPER-CASE(WS-NEXT-MOVE(1:1))
                   WHEN "A" SET WS-ROW TO 1
                   WHEN "B" SET WS-ROW TO 2
                   WHEN "C" SET WS-ROW TO 3
                   WHEN OTHER MOVE "FAIL" TO WS-MOVE-OUTCOME
               END-EVALUATE
                   SET WS-COL TO WS-NEXT-MOVE(2:1)
                   IF WS-MOVE-OUTCOME IS NOT EQUAL TO "FAIL"
                   AND WS-COL IS GREATER THAN 0
                   AND WS-COL IS LESS THAN 4
                   AND WS-CELL(WS-ROW,WS-COL) = " "
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
                           PERFORM 0193-VALIDATE-WIN-PARAGRAPH
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
                   ADD 1 TO WS-MOVES 
           END-EVALUATE

           IF SWAP-PLAYERS
               IF HUMAN-PLAYER
                   MOVE "O" TO WS-PLAYER
               ELSE
                   MOVE "X" TO WS-PLAYER
               END-IF
           END-IF.

       0193-VALIDATE-WIN-PARAGRAPH.
           INITIALIZE WS-MASK-DETECTED
           SET WS-DETECT-LOOP-COUNT TO 1
           PERFORM 9 TIMES
               IF FD-WINMASK(WS-DETECT-LOOP-COUNT:1) IS EQUAL TO
               WS-FLAT-GAME-GRID(WS-DETECT-LOOP-COUNT:1) 
               AND IS EQUAL TO 1
                   ADD 1 TO WS-MASK-DETECTED 
               END-IF
               ADD 1 TO WS-DETECT-LOOP-COUNT 
           END-PERFORM

           IF WIN-DETECTED
               IF HUMAN-PLAYER
                   MOVE "WIN" TO WS-MOVE-OUTCOME
               ELSE
                   MOVE "LOSE" TO WS-MOVE-OUTCOME
               END-IF
           END-IF.

       0200-RANDOM-NUMBER-GAME.
           INITIALIZE RANDOM-NUM-CHOICE.
           INITIALIZE BET-AMOUNT.
           DISPLAY RANDOM-NUM-GAME-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT BET-FIELD.
           MOVE BET-AMOUNT TO COST.
           COMPUTE WINNINGS = BET-AMOUNT * 2.
           IF WINNINGS = "000" 
           OR (CHECK-LIMIT(WINNINGS, USER-INFO-CREDITS) = "FAIL")
               MOVE "WINNINGS EXCEEDING MAX CREDIT AMOUNT, ACTION ABORTE
      -         "D"
               TO CREDIT-LIMIT-MESSAGE
               PERFORM 0200-RANDOM-NUMBER-GAME
           END-IF.
       
           ACCEPT RANDOM-NUM-CHOICE-FIELD.
           IF (RANDOM-NUM-CHOICE = 's' OR 'S')
           AND (CHECK-BALANCE(BET-AMOUNT, USER-INFO-CREDITS) = "TRUE")
               CALL 'deduct-credits' USING USER-INFO-NAME, COST,
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               ACCEPT SEED FROM TIME
               COMPUTE ANSWER =
                   FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1  
               PERFORM 0201-GAME-LOOP
           ELSE IF (RANDOM-NUM-CHOICE = 's' OR 'S')
           AND (CHECK-BALANCE(BET-AMOUNT, USER-INFO-CREDITS) = "FALSE")
               MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
               PERFORM 0200-RANDOM-NUMBER-GAME
           ELSE IF RANDOM-NUM-CHOICE = 'g' OR 'G'
               PERFORM 0160-GAMES-MENU
           ELSE IF RANDOM-NUM-CHOICE = 'q' OR 'Q'
               STOP RUN
           ELSE
               PERFORM 0200-RANDOM-NUMBER-GAME
           END-IF.
              
       0201-GAME-LOOP.
           INITIALIZE GUESS-INPUT.
           INITIALIZE WS-RANDOM-NUM-MSG.
           INITIALIZE RANDOM-NUM-GUESS-CHOICE.
           DISPLAY GUESS-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT GUESS-FIELD.
           MOVE GUESS-INPUT TO GUESS
           IF GUESS NOT = ANSWER
               MOVE "Incorrect, you lose."
               TO WS-RANDOM-NUM-MSG
               PERFORM 0202-RESULT-PAGE
           ELSE  
               MOVE "You Win!" TO WS-RANDOM-NUM-MSG
               CALL 'add-credits' USING USER-INFO-NAME, WINNINGS,
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               PERFORM 0202-RESULT-PAGE
           END-IF.
       
       0202-RESULT-PAGE.
           INITIALIZE RANDOM-NUM-GUESS-CHOICE.
           DISPLAY GUESS-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT RANDOM-NUM-GUESS-CHOICE-FIELD
           IF RANDOM-NUM-GUESS-CHOICE = 'y' OR 'Y'
             PERFORM 0200-RANDOM-NUMBER-GAME
           ELSE IF RANDOM-NUM-GUESS-CHOICE = 'g' OR 'G'
               PERFORM 0160-GAMES-MENU
           ELSE IF RANDOM-NUM-GUESS-CHOICE = 'q' OR 'Q'
               STOP RUN
           END-IF.

           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           