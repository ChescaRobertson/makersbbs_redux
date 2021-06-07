       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION MESSAGE-NUM.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT F-MESSAGES-FILE ASSIGN TO "messages.dat"
                 ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
               05 RC-MESSAGE-TITLE PIC X(50).
               05 RC-MESSAGE-BODY PIC X(300).

           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.
           01 COUNTER UNSIGNED-INT.
           01 OFFSET UNSIGNED-INT.
           01 MSG-MENU-CHOICE PIC X.
           01 MSG-MENU-CHOICE2 PIC X.
           01 WRITE-MSG-MENU-CHOICE PIC X.
           01 MSG-TITLE PIC X(50).
           01 MSG-BODY PIC X(300).
           01 WS-MESSAGES.
               05 WS-MESSAGE OCCURS 100 TIMES
               ASCENDING KEY IS WS-TITLE
               INDEXED BY MSG-IDX.
                   10 WS-TITLE PIC X(60).

           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).  

           SCREEN SECTION.
           01 LOGIN-SCREEN.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK
             HIGHLIGHT, FOREGROUND-COLOR IS 3.
             05 LINE 4 COL 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COL 10 PIC X(10)
                USING USER-NAME. 

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK.
             05 LINE 4 COL 10 VALUE "Welcome, ".
             05 LINE 4 COL 19 PIC X(10) USING USER-NAME.
             05 LINE 10 COL 10 VALUE "(m) Messages"
                REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.
             05 LINE 10 COL 30 VALUE "(g) Games"
                REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 4.
             05 LINE 12 COL 10 VALUE "(l)   Logout"
                REVERSE-VIDEO , HIGHLIGHT.            
             05 LINE 12 COL 30 VALUE "(q)  Quit"
                REVERSE-VIDEO, HIGHLIGHT.  
             05 LINE 20 COL 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COL 16 PIC X
                USING MENU-CHOICE.

           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK.
             05 LINE 4 COL 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COL 10 VALUE "1.  ".
             05 LINE 6 COL 14 PIC X(50) USING WS-TITLE(OFFSET).
             05 LINE 7 COL 10 VALUE "2.  ".
             05 LINE 7 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 1).
             05 LINE 8 COL 10 VALUE "3.  ".
             05 LINE 8 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 2).
             05 LINE 9 COL 10 VALUE "4.  ".
             05 LINE 9 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 3).
             05 LINE 10 COL 10 VALUE "5.  ".
             05 LINE 10 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 4).
             05 LINE 11 COL 10 VALUE "6.  ".
             05 LINE 11 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 5).
             05 LINE 12 COL 10 VALUE "7.  ".
             05 LINE 12 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 6).
             05 LINE 13 COL 10 VALUE "8.  ".
             05 LINE 13 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 7).
             05 LINE 14 COL 10 VALUE "9.  ".
             05 LINE 14 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 8).
             05 LINE 15 COL 10 VALUE "10.  ".
             05 LINE 15 COL 14 PIC X(50) USING WS-TITLE(OFFSET - 9).
             05 LINE 26 COL 10 VALUE "( ) Read Message by Number"
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.  
             05 LINE 28 COL 10 VALUE "(w) Write your own message"
             REVERSE-VIDEO HIGHLIGHT FOREGROUND-COLOR IS 2.               
             05 LINE 30 COL 10 VALUE "(n) Next Page"
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6.  
             05 LINE 30 COL 30 VALUE "(p) Previous Page"
             REVERSE-VIDEO, HIGHLIGHT FOREGROUND-COLOR IS 6. 
             05 LINE 32 COL 10 VALUE "(g)   Go back"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 32 COL 30 VALUE "(q) Quit"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 34 COL 10 VALUE "Pick: ".
             05 MSG-MENU-CHOICE-FIELD LINE 34 COL 16 PIC X
                USING MSG-MENU-CHOICE.

           01 WRITE-MSG-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "MAKERS BBS" UNDERLINE, BLINK.
             05 LINE 4 COL 10 VALUE "Title: ".
             05 MSG-TITLE-FIELD LINE 4 COL 17 PIC X(50)
                USING MSG-TITLE.
             05 LINE 6 COL 10 VALUE "Body: ".
             05 MSG-BODY-FIELD LINE 6 COL 16 PIC X(300)
                USING MSG-BODY.
             05 LINE 30 COL 10 VALUE "(g) Go back"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 32 COL 10 VALUE "(q)    Quit"
             REVERSE-VIDEO, HIGHLIGHT.
             05 LINE 34 COL 10 VALUE "Pick: ".             
             05 WRITE-MSG-CHOICE-FIELD LINE 34 COL 16 PIC X
                USING WRITE-MSG-MENU-CHOICE.        
                           
       PROCEDURE DIVISION.

       0100-CREATE-TABLE.
           SET COUNTER TO 0.
           OPEN INPUT F-MESSAGES-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ F-MESSAGES-FILE
               NOT AT END
                 ADD 1 TO COUNTER
                 MOVE RC-MESSAGE-TITLE TO WS-MESSAGE(COUNTER)
               AT END  
                 MOVE 1 TO WS-FILE-IS-ENDED
                 MOVE COUNTER TO OFFSET
             END-READ      
           END-PERFORM.
           CLOSE F-MESSAGES-FILE.

       
       0110-DISPLAY-LOGIN.
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0120-DISPLAY-MENU.

       0120-DISPLAY-MENU.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" or "Q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" or "L" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "m" or "M" THEN
             PERFORM 0130-MSG-MENU
           END-IF.

       0130-MSG-MENU.
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           IF MSG-MENU-CHOICE = "n" or "N" THEN
               IF OFFSET > 20
                    COMPUTE OFFSET = OFFSET - 10
               ELSE MOVE 10 TO OFFSET
           ELSE IF MSG-MENU-CHOICE = "p" OR "P" THEN
                     COMPUTE OFFSET = OFFSET + 10        
               END-IF
           IF MSG-MENU-CHOICE = "q" or "Q" THEN
               STOP RUN
           ELSE IF MSG-MENU-CHOICE = "w" or "W" then
               PERFORM 0140-WRITE-MSG   
           ELSE IF MENU-CHOICE = "g" or "G" THEN
               PERFORM 0120-DISPLAY-MENU     
           END-IF.

           PERFORM 0130-MSG-MENU.

       0140-WRITE-MSG.
           INITIALIZE MSG-TITLE.
           INITIALIZE MSG-BODY.
           INITIALIZE WRITE-MSG-MENU-CHOICE.
           DISPLAY WRITE-MSG-SCREEN.
           ACCEPT MSG-TITLE-FIELD.
           ACCEPT MSG-BODY-FIELD.
           ACCEPT WRITE-MSG-CHOICE-FIELD.
            OPEN EXTEND F-MESSAGES-FILE.
               MOVE MSG-TITLE TO RC-MESSAGE-TITLE.
               MOVE MSG-BODY TO RC-MESSAGE-BODY.
               WRITE RC-MESSAGE
               END-WRITE.
           CLOSE F-MESSAGES-FILE.
           IF WRITE-MSG-CHOICE-FIELD = "q" or "Q" THEN
               STOP RUN
           ELSE IF WRITE-MSG-CHOICE-FIELD = "g" or "G" then
               PERFORM 0120-DISPLAY-MENU
           END-IF.
