       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.

           SCREEN SECTION.
           01 LOGIN-SCREEN.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COLUMN 10 PIC X(10)
                USING USER-NAME.

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 8 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 8 COLUMN 30 VALUE "(l) Logout".
             05 LINE 8 COLUMN 60 VALUE "(q) Quit".
             05 LINE 20 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING MENU-CHOICE.

           01-MESSAGES-MENU-SCREEN.

           01-VIEW-MESSAGE-SCREEN.

           01-WRITE-MESSAGE-SCREEN.     


       PROCEDURE DIVISION.

       0110-DISPLAY-LOGIN.
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0120-DISPLAY-MENU.

       0120-DISPLAY-MENU.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "n" THEN
             PERFORM 0120-DISPLAY-MENU
           END-IF.

       0130-MESSAGES-MENU.
           CALL MESSAGES-MENU.

       0140-VIEW-MESSAGE.
           CALL VIEW-MESSAGE

       0150-WRITE-MESSAGE.
           CALL WRITE-MESSAGE
               
