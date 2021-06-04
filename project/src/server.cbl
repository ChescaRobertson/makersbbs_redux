       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT F-MESSAGES-FILE ASSIGN TO "messages.dat"
                 ORGANISATION IS LINE SEQUENTIAL.
 
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
               05 RC-MESSAGE-ID-NUM PIC 99.
               05 RC-MESSAGE-TITLE PIC X(60).
               05 RC-MESSAGE-BODY PIC X(300).

           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.
           01 WS-COUNTER PIC 9(2).
           01 MSG-MENU-CHOICE PIC X.
           01 WS-MESSAGES.
               05 WS-MESSAGE OCCURS 10 TIMES
               ASCENDING KEY IS WS-TITLE
               INDEXED BY MSG-IDX.
                   10 WS-TITLE PIC X(60).
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
             05 LINE 8 COLUMN 10 VALUE "(m) Messages".
             05 LINE 8 COLUMN 30 VALUE "(l) Logout".
             05 LINE 8 COLUMN 48 VALUE "(q) Quit".
             05 LINE 20 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING MENU-CHOICE.

           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COLUMN 10 VALUE "1. ".
             05 LINE 6 COLUMN 10 PIC X(60) USING WS-TITLE(10).
             05 LINE 7 COLUMN 10 VALUE "2. ".
             05 LINE 7 COLUMN 10 PIC X(60) USING WS-TITLE(9).
             05 LINE 8 COLUMN 10 VALUE "3. ".
             05 LINE 8 COLUMN 10 PIC X(60) USING WS-TITLE(8).
             05 LINE 9 COLUMN 10 VALUE "4. ".
             05 LINE 9 COLUMN 10 PIC X(60) USING WS-TITLE(7).
             05 LINE 10 COLUMN 10 VALUE "5. ".
             05 LINE 10 COLUMN 10 PIC X(60) USING WS-TITLE(6).
             05 LINE 11 COLUMN 10 VALUE "6. ".
             05 LINE 11 COLUMN 10 PIC X(60) USING WS-TITLE(5).
             05 LINE 12 COLUMN 10 VALUE "7. ".
             05 LINE 12 COLUMN 10 PIC X(60) USING WS-TITLE(4).
             05 LINE 13 COLUMN 10 VALUE "8. ".
             05 LINE 13 COLUMN 10 PIC X(60) USING WS-TITLE(3).
             05 LINE 14 COLUMN 10 VALUE "9. ".
             05 LINE 14 COLUMN 10 PIC X(60) USING WS-TITLE(2).
             05 LINE 15 COLUMN 10 VALUE "10. ".
             05 LINE 15 COLUMN 10 PIC X(60) USING WS-TITLE(1).
             05 LINE 26 COLUMN 10 VALUE "( ) Read Message".
             05 LINE 28 COLUMN 10 VALUE "(w) Write your own message".
             05 LINE 30 COLUMN 10 VALUE "(n) Next Page".
             05 LINE 30 COLUMN 30 VALUE "(p) Previous Page".
             05 LINE 30 COLUMN 50 VALUE "(g) Go back".
             05 LINE 32 COLUMN 10 VALUE "Pick: ".
             05 MSG-MENU-CHOICE-FIELD LINE 32 COLUMN 16 PIC X
                USING MSG-MENU-CHOICE.


      *    01-VIEW-MESSAGE-SCREEN.

      *    01-WRITE-MESSAGE-SCREEN.     


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
           ELSE IF MENU-CHOICE = "m" THEN
             PERFORM 0130-MSG-MENU
           END-IF.

       0130-MSG-MENU.
      *    CALL MESSAGES-MENU.
           SET MSG-IDX TO 0.
           OPEN INPUT F-MESSAGES-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ F-MESSAGES-FILE
               NOT AT END
                 ADD 1 TO MSG-IDX
                 MOVE RC-MESSAGE-TITLE TO WS-MESSAGE(MSG-IDX)
               AT END  
                 MOVE 1 TO WS-FILE-IS-ENDED
             END-READ      
           END-PERFORM.

           CLOSE F-MESSAGES-FILE.
           
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           IF MSG-MENU-CHOICE = "g" THEN
               PERFORM 0120-DISPLAY-MENU
           END-IF.
               
