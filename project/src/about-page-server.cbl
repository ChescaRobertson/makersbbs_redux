       IDENTIFICATION DIVISION.
       PROGRAM-ID. about-page-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.

           FUNCTION DISPLAY-TITLE
           FUNCTION DISPLAY-BODY
           FUNCTION CHOICE-TO-NUM.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

               SELECT F-ABOUT-FILE ASSIGN TO 'about-page.dat'
                 ORGANIZATION IS LINE SEQUENTIAL. 
       DATA DIVISION.
           FILE SECTION.
         
           FD F-ABOUT-FILE.
           01 ABOUT-INFO.
               05 ABOUT-AUTHOR PIC X(12).
               05 ABOUT-TITLE PIC X(31).
               05 ABOUT-BODY PIC X(500).
                
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
           
            *>-------- About Page Variables ---------
           01 ABOUT-PAGE-CHOICE PIC X(2).
           01 WS-ABOUT. 
               05 WS-ABOUTS OCCURS 100 TIMES 
               ASCENDING KEY IS WS-ABOUT-AUTHOR
               INDEXED BY ABOUT-IDX.
                   10 WS-ABOUT-AUTHOR PIC X(12).
                   10 WS-ABOUT-TITLE PIC X(31).
                   10 WS-ABOUT-BODY PIC X(500).

           01 ABOUT-OFFSET UNSIGNED-INT.
           01 ABOUT-PAGE-NUM PIC 99.
           01 ABOUT-NUM UNSIGNED-INT.

           01 ABOUT-PAGE-READ-CHOICE PIC X.
           01 ABOUT-TITLE-READ PIC X(31).
           01 WS-ABOUT-BODY-SEGMENTS.
               10 WS-ABOUT-BODY-SEGMENT-1 PIC X(60). 
               10 WS-ABOUT-BODY-SEGMENT-2 PIC X(60). 
               10 WS-ABOUT-BODY-SEGMENT-3 PIC X(60). 
               10 WS-ABOUT-BODY-SEGMENT-4 PIC X(60). 
               10 WS-ABOUT-BODY-SEGMENT-5 PIC X(60).

           01 ABOUT-INVALID-CHOICE-MESSAGE PIC X(15).
      
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
      -    "____|------------|______________________________________/".
                 05 LINE 47 COL 10 VALUE
           "   \_________/     |===|                                    
      -    "    |------------|                                    /".
                 05 LINE 48 COL 10 VALUE
           "             \_____|___/____________________________________
      -    "____||||||||||||||____________________________________/".
                 05 LINE 50 COL 10 VALUE
           "============================================================
      -    "==========================================================="
           .  

           
   

           01 ABOUT-PAGE-SCREEN.
               05 BLANK SCREEN.
               05 LINE 12 COL 45 VALUE
           "           _                 _     _____                 "
                   FOREGROUND-COLOR IS 2.
               05 LINE 13 COL 45 VALUE
           "     /\   | |               | |   |  __ \                "
                   FOREGROUND-COLOR IS 2.
               05 LINE 14 COL 45 VALUE
           "    /  \  | |__   ___  _   _| |_  | |__) |_ _  __ _  ___ "
                   FOREGROUND-COLOR IS 2.
               05 LINE 15 COL 45 VALUE
           "   / /\ \ | '_ \ / _ \| | | | __| |  ___/ _` |/ _` |/ _ \"
                   FOREGROUND-COLOR IS 2.
               05 LINE 16 COL 45 VALUE
           "  / ____ \| |_) | (_) | |_| | |_  | |  | (_| | (_| |  __/"
                   FOREGROUND-COLOR IS 2.
               05 LINE 17 COL 45 VALUE
           " /_/    \_\_.__/ \___/ \__,_|\__| |_|   \__,_|\__, |\___|"
                   FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 45 VALUE
           "                                               __/ |     "
                   FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 45 VALUE
           "                                              |___/      "
                   FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 45 VALUE 
           "Welcome to the BBS System, after extensive user feedback "
                   FOREGROUND-COLOR IS 2.
               05 line 22 col 45 value
           "and the mass influx of users we have extended our "
                   FOREGROUND-COLOR IS 2.
               05 LINE 23 COL 45 VALUE
           "functionality of the system, this has meant however we've "
                   FOREGROUND-COLOR IS 2.
               05 LINE 24 COL 45 VALUE
           "had to implement a monetary payment system for upkeep "
                   FOREGROUND-COLOR IS 2.
               05 LINE 25 COL 45 VALUE
           "below is a few bits of advice for using our credits "
                   FOREGROUND-COLOR IS 2.
               05 LINE 26 COL 45 VALUE 
           "system and in general, the program itself."
                   FOREGROUND-COLOR IS 2.
               05 LINE 28 COL 45 VALUE '1.'
                   FOREGROUND-COLOR IS 2.
               05 LINE 28 COL 47 PIC X(31) USING 
               WS-ABOUT-TITLE(ABOUT-OFFSET)
                   FOREGROUND-COLOR IS 2.
               05 LINE 30 COL 45 VALUE '2.'
                   FOREGROUND-COLOR IS 2.
               05 LINE 30 COL 47 PIC X(31) USING 
               WS-ABOUT-TITLE(ABOUT-OFFSET - 1)
                   FOREGROUND-COLOR IS 2.
               05 LINE 32 COL 45 VALUE '3.'
                   FOREGROUND-COLOR IS 2.
               05 LINE 32 COL 47 PIC X(31) USING 
               WS-ABOUT-TITLE(ABOUT-OFFSET - 2)
                   FOREGROUND-COLOR IS 2.
               05 LINE 34 COL 45 VALUE '4.'
                   FOREGROUND-COLOR IS 2.
               05 LINE 34 COL 47 PIC X(31) USING 
               WS-ABOUT-TITLE(ABOUT-OFFSET - 3)
                   FOREGROUND-COLOR IS 2.
               05 LINE 36 COL 45 VALUE '5.'
               FOREGROUND-COLOR IS 2.
               05 LINE 36 COL 47 PIC X(31) USING 
               WS-ABOUT-TITLE(ABOUT-OFFSET - 4)
                   FOREGROUND-COLOR IS 2.
               05 Line 38 COL 45 PIC X(15) USING
               ABOUT-INVALID-CHOICE-MESSAGE
                   HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 LINE 39 COL 45 VALUE "( ) What number to read"
                   FOREGROUND-COLOR IS 3.
               05 LINE 40 COL 45 VALUE "(n) Next Page"
                   FOREGROUND-COLOR IS 3.
               05 LINE 41 COL 45 VALUE "(p) Previous Page"
                   FOREGROUND-COLOR IS 3.
               05 LINE 42 COL 45 VALUE "(q) Go back"
                   FOREGROUND-COLOR IS 3.          
               05 LINE 44 COL 45 VALUE "Pick:"
                   FOREGROUND-COLOR IS 2.
               05 ABOUT-PAGE-FIELD LINE 44 COL 50 PIC X USING 
               ABOUT-PAGE-CHOICE
                   BLINK, FOREGROUND-COLOR IS 2.
      
           01 ABOUT-PAGE-READ-SCREEN.
               05 BLANK SCREEN.
               05 LINE 16 COL 45 PIC X(31) USING ABOUT-TITLE-READ
                   FOREGROUND-COLOR IS 2.
               05 LINE 18 COL 45 PIC X(60) USING WS-ABOUT-BODY-SEGMENT-1
                   FOREGROUND-COLOR IS 2.
               05 LINE 19 COL 45 PIC X(60) USING WS-ABOUT-BODY-SEGMENT-2
                   FOREGROUND-COLOR IS 2.
               05 LINE 20 COL 45 PIC X(60) USING WS-ABOUT-BODY-SEGMENT-3
                   FOREGROUND-COLOR IS 2.
               05 LINE 21 COL 45 PIC X(60) USING WS-ABOUT-BODY-SEGMENT-4
                   FOREGROUND-COLOR IS 2.
               05 LINE 22 COL 45 PIC X(60) USING WS-ABOUT-BODY-SEGMENT-5
                   FOREGROUND-COLOR IS 2.
               05 LINE 39 COL 45 VALUE "(q) Go Back"
                   FOREGROUND-COLOR IS 2.
               05 Line 37 COL 45 PIC X(15) USING 
               ABOUT-INVALID-CHOICE-MESSAGE
                   HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 ABOUT-PAGE-READ-FIELD LINE 39 COL 57 PIC X USING
               ABOUT-PAGE-READ-CHOICE
                   FOREGROUND-COLOR IS 2.
               
       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0470-ABOUT-PAGE-TABLE.
           SET COUNTER TO 0. 
           OPEN INPUT F-ABOUT-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-ABOUT-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE ABOUT-TITLE TO WS-ABOUT-TITLE(COUNTER)
                       MOVE ABOUT-BODY TO WS-ABOUT-BODY(COUNTER)
                   AT END
                       MOVE 1 TO WS-FILE-IS-ENDED
                       MOVE COUNTER TO ABOUT-OFFSET
                       MOVE 1 TO ABOUT-PAGE-NUM
                       MOVE 1 TO ABOUT-NUM
               END-READ
           END-PERFORM.
           CLOSE F-ABOUT-FILE.
           PERFORM 0480-ABOUT-PAGE.

       0480-ABOUT-PAGE.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ABOUT-PAGE-CHOICE.
           DISPLAY ABOUT-PAGE-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT ABOUT-PAGE-FIELD.
           IF ABOUT-PAGE-CHOICE = 'q' OR 'Q' 
               GOBACK 
           ELSE IF ABOUT-PAGE-CHOICE = 'n' OR 'N' 
               IF ABOUT-OFFSET > 20
                   COMPUTE ABOUT-OFFSET = ABOUT-OFFSET - 10
                   COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM + 1
               END-IF
               PERFORM 0480-ABOUT-PAGE
           ELSE IF ABOUT-PAGE-CHOICE = 'p' 
               IF ABOUT-PAGE-NUM = '01'
                   PERFORM 0480-ABOUT-PAGE
               ELSE IF ABOUT-PAGE-NUM = '02'
                   COMPUTE ABOUT-OFFSET = ABOUT-OFFSET + 10
                   COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM - 1
                   PERFORM 0480-ABOUT-PAGE
               ELSE 
                   COMPUTE ABOUT-OFFSET = ABOUT-OFFSET + 10
                   COMPUTE ABOUT-PAGE-NUM = ABOUT-PAGE-NUM - 1
                   PERFORM 0480-ABOUT-PAGE
               END-IF
           ELSE IF ABOUT-PAGE-CHOICE = "1" OR "2" OR "3" OR "4" OR "5"
             SET ABOUT-NUM TO CHOICE-TO-NUM(ABOUT-PAGE-CHOICE)
             MOVE SPACES TO ABOUT-INVALID-CHOICE-MESSAGE
             PERFORM 0490-ABOUT-PAGE-READ
           ELSE
             MOVE "Invalid Choice!" TO ABOUT-INVALID-CHOICE-MESSAGE
             PERFORM 0480-ABOUT-PAGE 
           END-IF.
           

       0490-ABOUT-PAGE-READ.
           PERFORM 0500-TIME-AND-DATE.
           INITIALIZE ABOUT-PAGE-READ-CHOICE.
           IF ABOUT-NUM = 1 OR 2 OR 3 OR 4 OR 5
               MOVE DISPLAY-TITLE(ABOUT-OFFSET ABOUT-NUM WS-ABOUT) 
               TO ABOUT-TITLE-READ
               MOVE DISPLAY-BODY(ABOUT-OFFSET ABOUT-NUM WS-ABOUT)
               TO WS-ABOUT-BODY-SEGMENTS
           END-IF.
           DISPLAY ABOUT-PAGE-READ-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           ACCEPT ABOUT-PAGE-READ-FIELD.
           IF ABOUT-PAGE-READ-CHOICE = "q" or "Q"
               MOVE SPACES TO ABOUT-INVALID-CHOICE-MESSAGE
               PERFORM 0480-ABOUT-PAGE
           ELSE 
               MOVE "Invalid Choice!" TO ABOUT-INVALID-CHOICE-MESSAGE
               PERFORM 0490-ABOUT-PAGE-READ              
           END-IF.
           
           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           