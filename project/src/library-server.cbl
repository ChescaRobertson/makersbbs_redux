       IDENTIFICATION DIVISION.
       PROGRAM-ID. library-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.

           FUNCTION DISPLAY-TITLE
           FUNCTION CHOICE-TO-NUM
           FUNCTION DISPLAY-BODY
           FUNCTION DISPLAY-AUTHOR

           FUNCTION CHECK-BALANCE.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           *>------Library Control-----------------------
             SELECT F-LIBRARY-FILE ASSIGN TO "library.dat"
                       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
            *>------Library Section------
           FD F-LIBRARY-FILE.
           01 LIBRARY.
               05 FD-BOOK-AUTHOR PIC X(12).
               05 BOOK-TITLE PIC X(31).
               05 BOOK-BODY PIC X(500).
                
           WORKING-STORAGE SECTION.

           01 COST PIC 999.
           01 UPDATED-BALANCE PIC 999.
           01 INSUFFICIENT-FUNDS PIC X(20).
           01 USER-INFO-LOGGED-IN PIC X(15) VALUE "Logged in as:".

           01 WS-DATETIME.
              05 WS-FORMATTED-YEAR  PIC  X(4).           
              05 WS-FORMATTED-MONTH PIC  X(2).          
              05 WS-FORMATTED-DY    PIC  X(2).
              05 WS-HOURS-MINS.
                  10 WS-FORMATTED-HOUR  PIC  X(2).
                  10 WS-FORMATTED-MINS  PIC  X(2).                   
           
            *>--------Library Section---------
           01 LIBRARY-CHOICE PIC X(2).
           01 PAGE-NUM PIC 99.
           01 LIBRARY-DISPLAY-MESSAGE PIC X(40).
           01 LIBRARY-NUM UNSIGNED-INT.
           01 TITLE PIC X(31).
           01 BODY PIC X(500).
           01 BOOK-AUTHOR PIC X(12).
           01 WS-BOOKS.
               05 WS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS WS-BOOK-AUTHOR-NAME
               INDEXED BY BOOK-IDX.
                   10 WS-BOOK-AUTHOR-NAME PIC X(12).
                   10 WS-BOOK-TITLE PIC X(31).
                   10 WS-BODY PIC X(500).
           
           01 OFFSET UNSIGNED-INT.
           01 READ-CHOICE PIC X.     
           01 AUDIOBOOK-MSG PIC X(50).

           01 WS-RANDOM-NUM-MSG PIC X(40).

           01 WS-READ-BODY-SEGMENTS.
               10 WS-READ-BODY-SEGMENT-1 PIC X(60). 
               10 WS-READ-BODY-SEGMENT-2 PIC X(60). 
               10 WS-READ-BODY-SEGMENT-3 PIC X(60). 
               10 WS-READ-BODY-SEGMENT-4 PIC X(60). 
               10 WS-READ-BODY-SEGMENT-5 PIC X(60).
      
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

           
   

           01 LIBRARY-SCREEN.
           05 BLANK SCREEN.
              05 LINE 09 COL 49 VALUE "---------------------------------
      -      "----------------------" FOREGROUND-COLOR IS 3.
               05 LINE 10 COL 49 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
               05 LINE 11 COL 49 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               
               
               05 LINE 12 COL 49 VALUE 
           "-------------------------------------------------------".
               05 LINE 13 COL 65 VALUE
               "WELCOME TO THE LIBRARY".

               05 LINE 14 COL 49 VALUE
           "Please Choose Below which book you would like to have in"
             .
               05 LINE 15 COL 49 VALUE
           "AudioBook Format, the charge will be 5 credits"
             .
                   
               05 LINE 18 COL 45 VALUE "||   AUTHOR   ||".
               05 LINE 18 COL 66 VALUE 



               "||                  TITLE                ||".
               05 LINE 19 COL 43 VALUE '1.'.
               05 LINE 19 COL 49 PIC X(12) 
               USING WS-BOOK-AUTHOR-NAME(OFFSET).
               05 LINE 19 COL 69 PIC X(31) USING WS-BOOK-TITLE(OFFSET).
               05 LINE 20 COL 49 VALUE
           "---------------------------------------------------------".
               05 LINE 21 COL 43 VALUE '2.'.
               05 LINE 21 COL 49 PIC X(12) 
               USING WS-BOOK-AUTHOR-NAME(OFFSET - 1)
               .
               05 LINE 21 COL 69 PIC X(31) 
               USING WS-BOOK-TITLE(OFFSET - 1)
               .
               05 LINE 22 COL 49 VALUE
           "---------------------------------------------------------".
               05 LINE 23 COL 43 VALUE '3.'.
               05 LINE 23 COL 49 PIC X(12) 
               USING WS-BOOK-AUTHOR-NAME(OFFSET - 2)
               .
               05 LINE 23 COL 69 PIC X(31) 
               USING WS-BOOK-TITLE(OFFSET - 2)
               .
               05 LINE 24 COL 49 VALUE
           "---------------------------------------------------------".
               05 LINE 25 COL 43 VALUE '4.'.
               05 LINE 25 COL 49 PIC X(12) 
               USING WS-BOOK-AUTHOR-NAME(OFFSET - 3)
               .
               05 LINE 25 COL 69 PIC X(31) 
               USING WS-BOOK-TITLE(OFFSET - 3)
               .
               05 LINE 26 COL 49 VALUE
           "---------------------------------------------------------".
               05 LINE 27 COL 43 VALUE '5.'.
               05 LINE 27 COL 49 PIC X(12) 
               USING WS-BOOK-AUTHOR-NAME(OFFSET - 4)
               .
               05 LINE 27 COL 69 PIC X(31) 
               USING WS-BOOK-TITLE(OFFSET - 4)
               .
               05 LINE 28 COL 49 VALUE
           "---------------------------------------------------------".

                
           
               05 LINE 31 COL 43 PIC X(40) USING LIBRARY-DISPLAY-MESSAGE
               .
               05 LINE 31 COL 77 VALUE 'Page No.'.
               05 LINE 31 COL 86 PIC 99 USING PAGE-NUM.
               05 LINE 35 COL 43 VALUE "( )Read the book by number".
               05 LINE 35 COL 77 VALUE "(n) Next Page".
               05 LINE 36 COL 43 VALUE "(p) Previous Page".
               05 LINE 36 COL 77 VALUE "(q) Go back".
               05 LINE 38 COL 78 VALUE "Pick: ".
               05 LIBRARY-FIELD LINE 38 COLUMN 86 PIC X 
               USING LIBRARY-CHOICE.
               05 LINE 40 COL 78 PIC X(20) USING INSUFFICIENT-FUNDS.
               
               
           01 READ-BOOK-SCREEN
               BACKGROUND-COLOR IS 8.
                05 BLANK SCREEN.
           05 LINE 09 COL 49 VALUE "---------------------------------
      -      "----------------------" FOREGROUND-COLOR IS 3.
              05 LINE 10 COL 49 VALUE "*********************************
      -      "***********************" FOREGROUND-COLOR IS 5.
              05 LINE 11 COL 49 VALUE "---------------------------------
      -      "-----------------------" FOREGROUND-COLOR IS 2.
               
               
               05 LINE 12 COL 49 VALUE 
           "-------------------------------------------------------".
               05 LINE 13 COL 65 VALUE
               "WELCOME TO THE LIBRARY".
               05 LINE 14 COL 49 VALUE
           "Please Choose Below which book you would like to have in"
             .

               05 LINE 15 COL 49 VALUE
           "AudioBook Format, the charge will be 5 credits"
             .
               05 LINE 16 COL 49 PIC X(50) USING AUDIOBOOK-MSG
                    HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 LINE 18 COL 60 VALUE 'Title:'.
               05 LINE 18 COL 69 PIC X(50) USING TITLE.
               05 LINE 22 COLUMN 40 PIC X(60) USING 
               WS-READ-BODY-SEGMENT-1.
               05 LINE 23 COLUMN 40 PIC X(60) USING 
               WS-READ-BODY-SEGMENT-2.
               05 LINE 24 COLUMN 40 PIC X(60) USING 
               WS-READ-BODY-SEGMENT-3.
               
               05 LINE 25 COLUMN 40 PIC X(60) USING 
               WS-READ-BODY-SEGMENT-4.
               05 LINE 26 COLUMN 40 PIC X(60) USING 
               WS-READ-BODY-SEGMENT-5.
               05 LINE 31 COLUMN 69 VALUE 'Author: '.               
               05 LINE 31 COLUMN 60 PIC X(12) USING BOOK-AUTHOR.
               05 LINE 37 COL 60 VALUE "Pick: ".
               05 READ-CHOICE-FIELD LINE 37 COLUMN 67 PIC X
               USING READ-CHOICE.
               05 LINE 33 COL 60 VALUE 'Press q to leave'.
               
       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0220-GENERATE-LIBRARY-TABLE.
           call 'generate-library-table' USING WS-BOOKS 
           LIBRARY-DISPLAY-MESSAGE OFFSET PAGE-NUM.
           PERFORM 0230-LIBRARY-MENU.

       0230-LIBRARY-MENU.
           INITIALIZE LIBRARY-CHOICE.

           MOVE "10" TO COST.
    
           DISPLAY LIBRARY-SCREEN.

           DISPLAY PIP-BOY-SCREEN.

           PERFORM 0113-DISPLAY-TIME-USER-INFO.

           ACCEPT LIBRARY-FIELD.
           IF LIBRARY-CHOICE = 'q' THEN 
               GOBACK
           ELSE IF LIBRARY-CHOICE = 'n' THEN
               IF OFFSET > 10
                   COMPUTE OFFSET = OFFSET - 5
                   COMPUTE PAGE-NUM = PAGE-NUM + 1
                   MOVE 'Here are the next 5 books' TO
                       LIBRARY-DISPLAY-MESSAGE
               END-IF
               PERFORM 0230-LIBRARY-MENU
           ELSE IF LIBRARY-CHOICE = 'p' THEN
               IF PAGE-NUM = '01'
                 PERFORM 0230-LIBRARY-MENU
               ELSE IF PAGE-NUM = '02'
                 COMPUTE OFFSET = OFFSET + 5
                 COMPUTE PAGE-NUM = PAGE-NUM - 1
                 MOVE 'Here are the previous 5 books' TO
                   LIBRARY-DISPLAY-MESSAGE
                 PERFORM 0230-LIBRARY-MENU
               ELSE
                 COMPUTE OFFSET = OFFSET + 5
                 COMPUTE PAGE-NUM = PAGE-NUM - 1
                   PERFORM 0230-LIBRARY-MENU
               END-IF
           ELSE IF (LIBRARY-CHOICE = '1' OR '2' OR '3' OR '4' OR '5')
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               SET LIBRARY-NUM TO CHOICE-TO-NUM(LIBRARY-CHOICE)
               PERFORM 0240-READ-BOOK
           ELSE IF (LIBRARY-CHOICE = '1' OR '2' OR '3' OR '4' OR '5')
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "FALSE") THEN
               MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
               PERFORM 0230-LIBRARY-MENU
           ELSE
               PERFORM 0230-LIBRARY-MENU
           END-IF. 

       0240-READ-BOOK.
           INITIALIZE READ-CHOICE.
           MOVE "5" TO COST.
           IF LIBRARY-NUM = 1 OR 2 OR 3 OR 4 OR 5
               MOVE DISPLAY-TITLE(OFFSET LIBRARY-NUM WS-BOOKS)
               TO TITLE
               MOVE DISPLAY-BODY(OFFSET LIBRARY-NUM WS-BOOKS)
               TO BODY
               MOVE DISPLAY-AUTHOR(OFFSET LIBRARY-NUM WS-BOOKS)
               TO BOOK-AUTHOR
           END-IF.
           MOVE BODY TO WS-READ-BODY-SEGMENTS.
           DISPLAY READ-BOOK-SCREEN.
           DISPLAY PIP-BOY-SCREEN.
           PERFORM 0113-DISPLAY-TIME-USER-INFO.
           
           ACCEPT READ-CHOICE.


           IF READ-CHOICE = 'q' THEN
               PERFORM 0230-LIBRARY-MENU
           ELSE IF (READ-CHOICE = 'a' )
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
               CALL 'deduct-credits' USING USER-INFO-NAME, COST, 
               UPDATED-BALANCE
               MOVE UPDATED-BALANCE TO USER-INFO-CREDITS
               MOVE "To enable the audiobook feature, please read aloud"
               TO AUDIOBOOK-MSG
               PERFORM 0240-READ-BOOK
           ELSE IF (READ-CHOICE = 'a' )
           AND (CHECK-BALANCE(COST, USER-INFO-CREDITS) = "TRUE") THEN
               MOVE "INSUFFICIENT CREDITS" TO INSUFFICIENT-FUNDS
               PERFORM 0230-LIBRARY-MENU
           END-IF.

           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           