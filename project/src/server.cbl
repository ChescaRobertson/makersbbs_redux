       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
    
               FUNCTION VERIFY-PASSWORD 
               FUNCTION CHECK-BALANCE
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

           *>----- Torch Variables -----   
           01 TORCH-CHOICE PIC X. 

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
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -           "LIVERY SERVICE" FOREGROUND-COLOR 2.

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
               HIGHLIGHT, FOREGROUND-COLOR IS 2.
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
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 LINE 26 COLUMN 50 VALUE " (Usernames must be unique.)"
               FOREGROUND-COLOR IS 2.
               05 LINE 27 COLUMN 30 PIC X(50) USING ERROR-MSG-1 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 NEW-USER-NAME-FIELD LINE 28 COLUMN 30 PIC X(16)
               USING NEW-USER-NAME FOREGROUND-COLOR IS 2.
               05 LINE 29 COLUMN 30 PIC X(50) USING OK-MSG-1 HIGHLIGHT
               FOREGROUND-COLOR is 2.
               05 LINE 31 COLUMN 30 VALUE "Enter a password:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 LINE 31 COLUMN 50 VALUE " (Your password must be a min
      -        "imum of 6 characters and 1 number.) "
               FOREGROUND-COLOR IS 2.
               05 LINE 32 COLUMN 30 PIC X(50) USING ERROR-MSG-2 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 NEW-PASSWORD-FIELD LINE 33 COLUMN 30 PIC X(20)
               USING NEW-PASSWORD FOREGROUND-COLOR IS 2.
               05 LINE 34 COLUMN 30 PIC X(50) USING OK-MSG-2 HIGHLIGHT
               FOREGROUND-COLOR is 2.
               05 LINE 36 COLUMN 30 VALUE "Enter a valid Bank Account nu
      -        "mber:"
               HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 LINE 37 COLUMN 30 PIC X(50) USING ERROR-MSG-3 
               HIGHLIGHT FOREGROUND-COLOR is 4.
               05 ACCOUNT-NUM-FIELD LINE 38 COLUMN 30 PIC X(8)
               USING ACCOUNT-NUM FOREGROUND-COLOR IS 2.
               05 LINE 39 COLUMN 30 PIC X(50) USING OK-MSG-3 HIGHLIGHT
               FOREGROUND-COLOR is 2 . 
               05 LINE 41 COLUMN 30 VALUE "(s) Submit" HIGHLIGHT 
               FOREGROUND-COLOR IS 3 .  
               05 LINE 42 COLUMN 30 VALUE "(g) Go Back" HIGHLIGHT 
               FOREGROUND-COLOR IS 3 .    
               05 LINE 44 COLUMN 30 VALUE "Pick:"
               HIGHLIGHT FOREGROUND-COLOR is 2 . 
               05 REGISTER-CHOICE-FIELD LINE 44 COLUMN 37 PIC X
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
               HIGHLIGHT FOREGROUND-COLOR is 2 . 
               05 LINE 33 COLUMN 37 VALUE "                       ".
               05 ERROR-CHOICE-FIELD LINE 33 COLUMN 36 PIC X
               USING ERROR-CHOICE BLINK.

           01 MENU-SCREEN
               BACKGROUND-COLOR IS 0.
               05 BLANK SCREEN.
               05 LINE 21 COL 46 VALUE "Hi, "
                  FOREGROUND-COLOR 2.
               05 LINE 21 COL 50 PIC X(16) USING USER-NAME
                  FOREGROUND-COLOR 2.
               05 LINE 23 COL 46 VALUE "Welcome to Vaultecs interactive 
      -    "pip boy application!"    
                  FOREGROUND-COLOR 2.
               05 LINE 24 COL 46 VALUE "Feel free to:"
                  FOREGROUND-COLOR 2.
               05 LINE 26 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 26 COL 48 VALUE "Read our message board."
                  FOREGROUND-COLOR 2.
               05 LINE 27 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 27 COL 48 VALUE "Leave a message of your own for 
      -        "fellow travellers." FOREGROUND-COLOR 2.
               05 LINE 28 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 28 COL 48 VALUE "Play a few games & win some cred
      -        "its! " FOREGROUND-COLOR 2.
               05 LINE 29 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 29 COL 48 VALUE "Visit our Library of the Worlds  
      -        "last books!" FOREGROUND-COLOR 2.
               05 LINE 30 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 30 COL 48 VALUE "Avoid that radioactive rain & ch'
      -        "eck the Weather." FOREGROUND-COLOR 2 . 
               05 LINE 31 COL 46 VALUE "* " FOREGROUND-COLOR IS 7.
               05 LINE 315 COL 48 VALUE "Most importantly. HAVE FUN! It's
      -        " only the apocolypse!" FOREGROUND-COLOR 2 .
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
               05 LINE 38 COL 46 VALUE "(a) About.      "
                  HIGHLIGHT FOREGROUND-COLOR IS 3.              
              05 LINE 40 COL 46 VALUE "Pick: "
                  HIGHLIGHT FOREGROUND-COLOR IS 2.
               05 MENU-CHOICE-FIELD LINE 40 COL 53 PIC X
                  USING MENU-CHOICE BLINK.

           01 TORCH-SCREEN
               BACKGROUND-COLOR IS 6 . 
               05 BLANK SCREEN.
               05 TORCH-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING TORCH-CHOICE BLINK.

           01 CHANGE-PASSWORD-SCREEN.
               05 BLANK SCREEN.  
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
               HIGHLIGHT FOREGROUND-COLOR 2.
               05 CHANGE-PWORD-FIELD LINE 40 COLUMN 73 PIC X
               USING CHANGE-PWORD-CHOICE.
               05 LINE 44 COL 78 VALUE "Powered by the MOJAVE EXPRESS DE
      -        "LIVERY SERVICE" FOREGROUND-COLOR 2.

           01 CHANGE-ACCOUNT-NUM-SCREEN.
               05 BLANK SCREEN.  
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
               HIGHLIGHT FOREGROUND-COLOR 2.
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
           PERFORM 0110-DISPLAY-LOGIN.
          
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
           ELSE IF ERROR-CHOICE = 'x' or 'X'
               STOP RUN
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
           ELSE IF MENU-CHOICE = "f" or "F" THEN
                CALL "games-server" USING USER-INFO-NAME, 
                USER-INFO-CRED-DISPLAY
                PERFORM 0120-DISPLAY-MENU
           ELSE IF MENU-CHOICE = "b" or "B" THEN
               CALL "library-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MENU-CHOICE = 'c' or 'C' THEN 
               CALL "buy-credits-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY, WS-USERS, USER-NAME, WS-PASSWORD,
               ACCOUNT-NUM
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MENU-CHOICE = 'a' or 'A' THEN 
               CALL "about-page-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY
               PERFORM 0120-DISPLAY-MENU
           ELSE IF (MENU-CHOICE = 'w' or 'W')
               MOVE '2' TO COST
               CALL "weather-server" USING USER-INFO-NAME, 
               USER-INFO-CRED-DISPLAY COST
               PERFORM 0120-DISPLAY-MENU 
           ELSE IF MENU-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF MENU-CHOICE = 'u' or 'U' THEN 
               PERFORM 0650-CHANGE-ACCOUNT-NUM
           ELSE IF MENU-CHOICE = 'p' or 'P' THEN 
               PERFORM 0600-CHANGE-PASSWORD
           END-IF.
      
           PERFORM 0120-DISPLAY-MENU.

       0350-TORCH.
           INITIALIZE TORCH-CHOICE.
           DISPLAY TORCH-SCREEN. 
           ACCEPT TORCH-CHOICE-FIELD.
           IF TORCH-CHOICE = 'x' OR 'X' OR 't' OR 'T' THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE 
               PERFORM 0350-TORCH
           END-IF. 
           
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
               MOVE UPDATED-PASSWORD TO WS-PASSWORD
           ELSE IF CHANGE-PWORD-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF CHANGE-PWORD-CHOICE = 'x' or 'X' THEN 
               STOP RUN
           END-IF. 
           
           INITIALIZE CHANGE-PWORD-CHOICE.
           DISPLAY CHANGE-PASSWORD-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-PWORD-FIELD.
           IF CHANGE-PWORD-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE IF CHANGE-PWORD-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF CHANGE-PWORD-CHOICE = 'x' or 'X' THEN 
               STOP RUN
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
           ELSE IF CHANGE-ACNT-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF CHANGE-ACNT-CHOICE = 'x' or 'X' THEN 
               STOP RUN
           END-IF. 
           
           INITIALIZE CHANGE-ACNT-CHOICE.
           DISPLAY CHANGE-ACCOUNT-NUM-SCREEN. 
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT CHANGE-ACNT-FIELD.
           IF CHANGE-ACNT-CHOICE = "g" OR "G" THEN 
               PERFORM 0120-DISPLAY-MENU
           ELSE IF CHANGE-ACNT-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF CHANGE-ACNT-CHOICE = 't' or 'T' THEN 
               PERFORM 0350-TORCH
           ELSE IF CHANGE-ACNT-CHOICE = 'x' or 'X' THEN 
               STOP RUN
           ELSE 
               PERFORM 0120-DISPLAY-MENU
           END-IF. 
