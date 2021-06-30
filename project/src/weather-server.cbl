       IDENTIFICATION DIVISION.
       PROGRAM-ID. weather-server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.

           FUNCTION CHECK-BALANCE.

       DATA DIVISION.
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
      
           *>----- Weather Variables -----
           01 W1-CHOICE PIC X.
           01 W2-CHOICE PIC X.
           01 W3-CHOICE PIC X.
           01 W4-CHOICE PIC X.

           01 SEED PIC 9(8).
           01 ANSWER PIC 99.
      
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
   
           01 WEATHER-SCREEN-1.
               05 BLANK SCREEN.
               05 LINE 8 COL 39 VALUE "WEATHER REPORT: " UNDERLINE, 
               HIGHLIGHT.
               05 LINE 12 COL 15 VALUE "MORNING: " HIGHLIGHT.
               05 LINE 14 COL 29 VALUE "OVERCAST" HIGHLIGHT.
               05 LINE 15 COL 29 VALUE "10 'C".
               05 LINE 16 COL 29 VALUE "<- 3-4 km/h ".
               05 LINE 17 COL 29 VALUE "0.0 mm | 0%".
               05 LINE 15 COL 18 VALUE     ".--.".
               05 LINE 16 COL 15 VALUE  ".-(    ).". 
               05 LINE 17 COL 14 VALUE "(___.__)__)".
               05 LINE 12 COL 50 VALUE "NOON: " HIGHLIGHT.
               05 LINE 14 COL 64 VALUE "HEAVY RAIN" HIGHLIGHT.
               05 LINE 15 COL 64 VALUE "16 'C".
               05 LINE 16 COL 64 VALUE "<- 6-8 km/h ".
               05 LINE 17 COL 64 VALUE "2.4 mm | 87%".
               05 LINE 14 COL 53 VALUE     ".--.".
               05 LINE 15 COL 50 VALUE  ".-(    ).". 
               05 LINE 16 COL 49 VALUE "(___.__)__)".
               05 LINE 17 COL 49 VALUE " , , , , ," FOREGROUND-COLOR IS 
               3.
               05 LINE 18 COL 49 VALUE ", , , , ," FOREGROUND-COLOR IS 
               3.
               05 LINE 21 COL 15 VALUE "EVENING: " HIGHLIGHT.
               05 LINE 23 COL 29 VALUE "PATCHY RAIN" HIGHLIGHT.
               05 LINE 24 COL 29 VALUE "18 'C".
               05 LINE 25 COL 29 VALUE "<- 4-6 km/h ".
               05 LINE 26 COL 29 VALUE "1.7 mm | 68%".
               05 LINE 23 COL 14 VALUE  "_`/''" FOREGROUND-COLOR IS 6 . 
               05 LINE 23 COL 19 VALUE    ".-.".
               05 LINE 24 COL 15 VALUE    ",\_" FOREGROUND-COLOR IS 6 .
               05 LINE 24 COL 18 VALUE      "(   ).". 
               05 LINE 25 COL 16 VALUE    "/" FOREGROUND-COLOR IS 6 .
               05 LINE 25 COL 17 VALUE   "(___(__)".
               05 LINE 26 COL 17 VALUE " , , , " FOREGROUND-COLOR IS 3.
               05 LINE 27 COL 17 VALUE "  , , " FOREGROUND-COLOR IS 3.
               05 LINE 21 COL 50 VALUE "NIGHT: " HIGHLIGHT.
               05 LINE 23 COL 64 VALUE "PATCHY RAIN" HIGHLIGHT.
               05 LINE 24 COL 64 VALUE "12 'C".
               05 LINE 25 COL 64 VALUE "<- 2-4 km/h ".
               05 LINE 26 COL 64 VALUE "1.4 mm | 64%".
               05 LINE 23 COL 49 VALUE  "_`/''" FOREGROUND-COLOR IS 6 . 
               05 LINE 23 COL 54 VALUE    ".-.".
               05 LINE 24 COL 50 VALUE    ",\_" FOREGROUND-COLOR IS 6 .
               05 LINE 24 COL 53 VALUE      "(   ).". 
               05 LINE 25 COL 51 VALUE    "/" FOREGROUND-COLOR IS 6 .
               05 LINE 25 COL 52 VALUE   "(___(__)".
               05 LINE 26 COL 52 VALUE " , , , " FOREGROUND-COLOR IS 3.
               05 LINE 27 COL 52 VALUE "  , , " FOREGROUND-COLOR IS 3.
               05 LINE 29 COL 39 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
               05 LINE 31 COL 39 VALUE "Pick: ".
               05 W1-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING W1-CHOICE.


           01 WEATHER-SCREEN-2.
               05 BLANK SCREEN.
               05 LINE 8 COL 39 VALUE "WEATHER REPORT: " UNDERLINE, 
               HIGHLIGHT.
               05 LINE 12 COL 15 VALUE "MORNING: " HIGHLIGHT.
               05 LINE 14 COL 29 VALUE "OVERCAST" HIGHLIGHT.
               05 LINE 15 COL 29 VALUE "9 'C".
               05 LINE 16 COL 29 VALUE "-> 3-4 km/h ".
               05 LINE 17 COL 29 VALUE "0.0 mm | 0%".
               05 LINE 15 COL 18 VALUE     ".--.".
               05 LINE 16 COL 15 VALUE  ".-(    ).". 
               05 LINE 17 COL 14 VALUE "(___.__)__)".
               05 LINE 12 COL 50 VALUE "NOON: " HIGHLIGHT.
               05 LINE 14 COL 64 VALUE "PARTLY CLOUDY" HIGHLIGHT.
               05 LINE 15 COL 64 VALUE "14 'C".
               05 LINE 16 COL 64 VALUE "-> 2-4 km/h ".
               05 LINE 17 COL 64 VALUE "0.0 mm | 0%".
               05 LINE 14 COL 51 VALUE    "\  / " FOREGROUND-COLOR IS 6
               . 
               05 LINE 15 COL 49 VALUE  "_ /''" FOREGROUND-COLOR IS 6 . 
               05 LINE 15 COL 54 VALUE    ".-.".
               05 LINE 16 COL 51 VALUE    "\_" FOREGROUND-COLOR IS 6 .
               05 LINE 16 COL 53 VALUE      "(   ).". 
               05 LINE 17 COL 51 VALUE    "/" FOREGROUND-COLOR IS 6 .
               05 LINE 17 COL 52 VALUE   "(___(__)".
               05 LINE 21 COL 15 VALUE "EVENING: " HIGHLIGHT.
               05 LINE 23 COL 29 VALUE "PARTLY CLOUDY" HIGHLIGHT.
               05 LINE 24 COL 29 VALUE "12 'C".
               05 LINE 25 COL 29 VALUE "-> 4-8 km/h ".
               05 LINE 26 COL 29 VALUE "0.0 mm | 0%".
               05 LINE 23 COL 16 VALUE    "\  / " FOREGROUND-COLOR IS 6
               . 
               05 LINE 24 COL 14 VALUE  "_ /''" FOREGROUND-COLOR IS 6 . 
               05 LINE 24 COL 19 VALUE    ".-.".
               05 LINE 25 COL 16 VALUE    "\_" FOREGROUND-COLOR IS 6 .
               05 LINE 25 COL 18 VALUE      "(   ).". 
               05 LINE 26 COL 16 VALUE    "/" FOREGROUND-COLOR IS 6 .
               05 LINE 26 COL 17 VALUE   "(___(__)".
               05 LINE 21 COL 50 VALUE "NIGHT: " HIGHLIGHT.
               05 LINE 23 COL 64 VALUE "OVERCAST" HIGHLIGHT.
               05 LINE 24 COL 64 VALUE "10 'C".
               05 LINE 25 COL 64 VALUE "-> 6-8 km/h ".
               05 LINE 26 COL 64 VALUE "0.0 mm | 0%".
               05 LINE 24 COL 53 VALUE     ".--.".
               05 LINE 25 COL 50 VALUE  ".-(    ).". 
               05 LINE 26 COL 49 VALUE "(___.__)__)".
               05 LINE 29 COL 39 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
               05 LINE 31 COL 39 VALUE "Pick: ".
               05 W2-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING W2-CHOICE.


           01 WEATHER-SCREEN-3.
               05 BLANK SCREEN.
               05 LINE 8 COL 39 VALUE "WEATHER REPORT: " UNDERLINE, 
               HIGHLIGHT.
               05 LINE 12 COL 15 VALUE "MORNING: " HIGHLIGHT.
               05 LINE 14 COL 29 VALUE "OVERCAST" HIGHLIGHT.
               05 LINE 15 COL 29 VALUE "14 'C".
               05 LINE 16 COL 29 VALUE "-> 1-4 km/h ".
               05 LINE 17 COL 29 VALUE "0.0 mm | 0%".
               05 LINE 15 COL 18 VALUE     ".--.".
               05 LINE 16 COL 15 VALUE  ".-(    ).". 
               05 LINE 17 COL 14 VALUE "(___.__)__)".
               05 LINE 12 COL 50 VALUE "NOON: " HIGHLIGHT.
               05 LINE 13 COL 50 VALUE "SEVERE WEATHER: " BLINK, 
               HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 LINE 14 COL 64 VALUE "RADSTORM" HIGHLIGHT.
               05 LINE 15 COL 64 VALUE "26 'C".
               05 LINE 16 COL 64 VALUE "<- 14-20 km/h ".
               05 LINE 17 COL 64 VALUE "3.8 mm | 87%".
               05 LINE 14 COL 53 VALUE     ".--.".
               05 LINE 15 COL 50 VALUE  ".-(    ).". 
               05 LINE 16 COL 49 VALUE "(___.__)__)".
               05 LINE 17 COL 49 VALUE " , * , * ," FOREGROUND-COLOR IS 
               2.
               05 LINE 18 COL 49 VALUE "* , * , *" FOREGROUND-COLOR IS 
               2.
               05 LINE 21 COL 15 VALUE "EVENING: " HIGHLIGHT.
               05 LINE 22 COL 15 VALUE "SEVERE WEATHER: " BLINK, 
               HIGHLIGHT, FOREGROUND-COLOR IS 4.
               05 LINE 23 COL 29 VALUE "RADSTORM" HIGHLIGHT.
               05 LINE 24 COL 29 VALUE "32 'C".
               05 LINE 25 COL 29 VALUE "<- 12-18 km/h ".
               05 LINE 26 COL 29 VALUE "4.1 mm | 81%".
               05 LINE 23 COL 18 VALUE     ".--.".
               05 LINE 24 COL 15 VALUE  ".-(    ).". 
               05 LINE 25 COL 14 VALUE "(___.__)__)".
               05 LINE 26 COL 14 VALUE " , * , * ," FOREGROUND-COLOR IS 
               2.
               05 LINE 27 COL 14 VALUE "* , * , *" FOREGROUND-COLOR IS 
               2.
               05 LINE 21 COL 50 VALUE "NIGHT: " HIGHLIGHT.
               05 LINE 23 COL 64 VALUE "LIGHT RAIN" HIGHLIGHT.
               05 LINE 24 COL 64 VALUE "18 'C".
               05 LINE 25 COL 64 VALUE "-> 4-8 km/h ".
               05 LINE 26 COL 64 VALUE "1.4 mm | 62%".
               05 LINE 23 COL 53 VALUE     ".--.".
               05 LINE 24 COL 50 VALUE  ".-(    ).". 
               05 LINE 25 COL 49 VALUE "(___.__)__)".
               05 LINE 26 COL 49 VALUE " ` ` ` ` `" FOREGROUND-COLOR IS 
               3.
               05 LINE 27 COL 49 VALUE "` ` ` ` `" FOREGROUND-COLOR IS 
               3.
               05 LINE 29 COL 39 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
               05 LINE 31 COL 39 VALUE "Pick: ".
               05 W3-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING W3-CHOICE.


           01 WEATHER-SCREEN-4.
               05 BLANK SCREEN.
               05 LINE 8 COL 39 VALUE "WEATHER REPORT: " UNDERLINE, 
               HIGHLIGHT.
               05 LINE 12 COL 15 VALUE "MORNING: " HIGHLIGHT.
               05 LINE 14 COL 29 VALUE "OVERCAST" HIGHLIGHT.
               05 LINE 15 COL 29 VALUE "2 'C".
               05 LINE 16 COL 29 VALUE "-> 4-8 km/h ".
               05 LINE 17 COL 29 VALUE "0.0 mm | 0%".
               05 LINE 15 COL 18 VALUE     ".--.".
               05 LINE 16 COL 15 VALUE  ".-(    ).". 
               05 LINE 17 COL 14 VALUE "(___.__)__)".
               05 LINE 12 COL 50 VALUE "NOON: " HIGHLIGHT.
               05 LINE 14 COL 64 VALUE "SLEET SHOWERS" HIGHLIGHT.
               05 LINE 15 COL 64 VALUE "1 'C".
               05 LINE 16 COL 64 VALUE "<- 11-14 km/h ".
               05 LINE 17 COL 64 VALUE "2.8 mm | 82%".
               05 LINE 14 COL 53 VALUE     ".--.".
               05 LINE 15 COL 50 VALUE  ".-(    ).". 
               05 LINE 16 COL 49 VALUE "(___.__)__)".
               05 LINE 17 COL 49 VALUE " ,   ,   ," FOREGROUND-COLOR IS 
               3.
               05 LINE 18 COL 49 VALUE "   *   *  " .
               05 LINE 21 COL 15 VALUE "EVENING: " HIGHLIGHT.
               05 LINE 22 COL 15 VALUE "CAUTION: " BLINK, 
               HIGHLIGHT, FOREGROUND-COLOR IS 6.
               05 LINE 23 COL 29 VALUE "HEAVY SNOW" HIGHLIGHT.
               05 LINE 24 COL 29 VALUE "2 'C".
               05 LINE 25 COL 29 VALUE "<- 12-18 km/h ".
               05 LINE 26 COL 29 VALUE "4.1 mm | 81%".
               05 LINE 23 COL 18 VALUE     ".--.".
               05 LINE 24 COL 15 VALUE  ".-(    ).". 
               05 LINE 25 COL 14 VALUE "(___.__)__)".
               05 LINE 26 COL 14 VALUE " * * * * *".
               05 LINE 27 COL 14 VALUE "* * * * *".
               05 LINE 21 COL 50 VALUE "NIGHT: " HIGHLIGHT.
               05 LINE 23 COL 64 VALUE "LIGHT SNOW" HIGHLIGHT.
               05 LINE 24 COL 64 VALUE "-1 'C".
               05 LINE 25 COL 64 VALUE "<- 4-8 km/h ".
               05 LINE 26 COL 64 VALUE "1.4 mm | 62%".
               05 LINE 23 COL 53 VALUE     ".--.".
               05 LINE 24 COL 50 VALUE  ".-(    ).". 
               05 LINE 25 COL 49 VALUE "(___.__)__)".
               05 LINE 26 COL 49 VALUE " *   *   *".
               05 LINE 27 COL 49 VALUE "  *   *  ".
               05 LINE 29 COL 39 VALUE "(g) Go back"
                REVERSE-VIDEO , HIGHLIGHT.            
               05 LINE 31 COL 39 VALUE "Pick: ".
               05 W4-CHOICE-FIELD LINE 31 COL 45 PIC X 
               USING W4-CHOICE.

       PROCEDURE DIVISION USING USER-INFO-NAME, USER-INFO-CRED-DISPLAY.

       0113-DISPLAY-TIME-USER-INFO.
           DISPLAY TIME-SCREEN.
           DISPLAY USER-INFO-SCREEN.
           DISPLAY CONNECTED-SCREEN.

       0300-CHECK-WEATHER SECTION.
           ACCEPT SEED FROM TIME.
           COMPUTE ANSWER =
               FUNCTION REM(FUNCTION RANDOM(SEED) * 1000, 10) + 1.
           IF ANSWER > 0 AND ANSWER <= 3 
               PERFORM WEATHER-ENVIRONMENT-1
           ELSE IF ANSWER > 3 AND ANSWER <= 6
               PERFORM WEATHER-ENVIRONMENT-2
           ELSE IF ANSWER = 7 OR ANSWER = 8 
               PERFORM WEATHER-ENVIRONMENT-3
           ELSE 
               PERFORM WEATHER-ENVIRONMENT-4
           END-IF. 
           
           WEATHER-ENVIRONMENT-1.
           INITIALIZE W1-CHOICE.
           DISPLAY WEATHER-SCREEN-1.
           ACCEPT W1-CHOICE-FIELD.
           IF W1-CHOICE = 'g' OR 'G' THEN 
               GOBACK
           ELSE 
               PERFORM WEATHER-ENVIRONMENT-1 
           END-IF. 

           WEATHER-ENVIRONMENT-2.
           INITIALIZE W2-CHOICE.
           DISPLAY WEATHER-SCREEN-2.
           ACCEPT W2-CHOICE-FIELD.
           IF W2-CHOICE = 'g' OR 'G' THEN 
               GOBACK
           ELSE 
               PERFORM WEATHER-ENVIRONMENT-2 
           END-IF. 

           WEATHER-ENVIRONMENT-3.
           INITIALIZE W3-CHOICE.
           DISPLAY WEATHER-SCREEN-3.
           ACCEPT W3-CHOICE-FIELD.
           IF W3-CHOICE = 'g' OR 'G' THEN 
               GOBACK
           ELSE 
               PERFORM WEATHER-ENVIRONMENT-3
           END-IF. 

           WEATHER-ENVIRONMENT-4.
           INITIALIZE W4-CHOICE.
           DISPLAY WEATHER-SCREEN-4.
           ACCEPT W4-CHOICE-FIELD.
           IF W4-CHOICE = 'g' OR 'G' THEN 
               GOBACK
           ELSE 
               PERFORM WEATHER-ENVIRONMENT-4
           END-IF. 

           GOBACK.

       0500-TIME-AND-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           