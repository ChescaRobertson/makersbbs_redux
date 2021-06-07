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
               05 RC-MESSAGE-TITLE PIC X(60).
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
           01 MSG-TITLE PIC X(60).
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
             05 LINE 2 COL 10 VALUE "Makers BBS".
             05 LINE 4 COL 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COL 10 PIC X(10)
                USING USER-NAME.

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "Makers BBS".
             05 LINE 4 COL 10 VALUE "Welcome, ".
             05 LINE 4 COL 19 PIC X(10) USING USER-NAME.
             05 LINE 8 COL 10 VALUE "(m) Messages".
             05 LINE 8 COL 30 VALUE "(l) Logout".
             05 LINE 8 COL 48 VALUE "(q) Quit".
             05 LINE 20 COL 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COL 16 PIC X
                USING MENU-CHOICE.

           01 MSG-MENU-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "Makers BBS".
             05 LINE 4 COL 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COL 10 VALUE "1. ".
             05 LINE 6 COL 13 PIC X(60) USING WS-TITLE(OFFSET).
             05 LINE 7 COL 10 VALUE "2. ".
             05 LINE 7 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 1).
             05 LINE 8 COL 10 VALUE "3. ".
             05 LINE 8 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 2).
             05 LINE 9 COL 10 VALUE "4. ".
             05 LINE 9 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 3).
             05 LINE 10 COL 10 VALUE "5. ".
             05 LINE 10 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 4).
             05 LINE 11 COL 10 VALUE "6. ".
             05 LINE 11 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 5).
             05 LINE 12 COL 10 VALUE "7. ".
             05 LINE 12 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 6).
             05 LINE 13 COL 10 VALUE "8. ".
             05 LINE 13 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 7).
             05 LINE 14 COL 10 VALUE "9. ".
             05 LINE 14 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 8).
             05 LINE 15 COL 10 VALUE "10. ".
             05 LINE 15 COL 13 PIC X(60) USING WS-TITLE(OFFSET - 9).
             05 LINE 26 COL 10 VALUE "( ) Read Message".
             05 LINE 28 COL 10 VALUE "(w) Write your own message".
             05 LINE 30 COL 10 VALUE "(n) Next Page".
             05 LINE 30 COL 30 VALUE "(p) Previous Page".
             05 LINE 30 COL 50 VALUE "(g) Go back".
             05 LINE 32 COL 10 VALUE "Pick: ".
             05 MSG-MENU-CHOICE-FIELD LINE 32 COL 16 PIC X
                USING MSG-MENU-CHOICE.

           01 MSG-MENU-SCREEN2
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "Makers BBS".
             05 LINE 4 COL 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COL 10 VALUE "11. ".
             05 LINE 6 COL 13 PIC X(60) USING WS-TITLE(20).
             05 LINE 7 COL 10 VALUE "12. ".
             05 LINE 7 COL 13 PIC X(60) USING WS-TITLE(19).
             05 LINE 8 COL 10 VALUE "13. ".
             05 LINE 8 COL 13 PIC X(60) USING WS-TITLE(18).
             05 LINE 9 COL 10 VALUE "14. ".
             05 LINE 9 COL 13 PIC X(60) USING WS-TITLE(17).
             05 LINE 10 COL 10 VALUE "15. ".
             05 LINE 10 COL 13 PIC X(60) USING WS-TITLE(16).
             05 LINE 11 COL 10 VALUE "16. ".
             05 LINE 11 COL 13 PIC X(60) USING WS-TITLE(15).
             05 LINE 12 COL 10 VALUE "17. ".
             05 LINE 12 COL 13 PIC X(60) USING WS-TITLE(14).
             05 LINE 13 COL 10 VALUE "18. ".
             05 LINE 13 COL 13 PIC X(60) USING WS-TITLE(13).
             05 LINE 14 COL 10 VALUE "19. ".
             05 LINE 14 COL 13 PIC X(60) USING WS-TITLE(12).
             05 LINE 15 COL 10 VALUE "20. ".
             05 LINE 15 COL 13 PIC X(60) USING WS-TITLE(11).
             05 LINE 26 COL 10 VALUE "( ) Read Message".
             05 LINE 28 COL 10 VALUE "(w) Write your own message".
             05 LINE 30 COL 10 VALUE "(n) Next Page".
             05 LINE 30 COL 30 VALUE "(p) Previous Page".
             05 LINE 30 COL 50 VALUE "(g) Go back".
             05 LINE 32 COL 10 VALUE "Pick: ".
             05 MSG-MENU-CHOICE2-FIELD LINE 32 COL 16 PIC X
                USING MSG-MENU-CHOICE2.

           01 WRITE-MSG-SCREEN
             BACKGROUND-COLOR IS 1.
             05 BLANK SCREEN.
             05 LINE 2 COL 10 VALUE "Makers BBS".
             05 LINE 4 COL 10 VALUE "Title: ".
             05 MSG-TITLE-FIELD LINE 4 COL 17 PIC X(60)
                USING MSG-TITLE.
             05 LINE 6 COL 10 VALUE "Body: ".
             05 MSG-BODY-FIELD LINE 6 COL 16 PIC X(300)
                USING MSG-BODY.
             05 LINE 30 COL 50 VALUE "(g) Go back".
             05 LINE 31 COL 50 VALUE "(q) Quit".
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
           IF MENU-CHOICE = "q" THEN
             STOP RUN
           ELSE IF MENU-CHOICE = "l" THEN
             PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "m" THEN
             PERFORM 0130-MSG-MENU
           END-IF.

       0130-MSG-MENU.
           INITIALIZE MSG-MENU-CHOICE.
           DISPLAY MSG-MENU-SCREEN.
           ACCEPT MSG-MENU-CHOICE-FIELD.
           IF MSG-MENU-CHOICE = "g" THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF MSG-MENU-CHOICE = "n" then
               IF OFFSET > 20
                    COMPUTE OFFSET = OFFSET - 10
               ELSE MOVE 10 TO OFFSET
               END-IF
               PERFORM 0130-MSG-MENU       
           ELSE IF MSG-MENU-CHOICE = "w" then
               PERFORM 0150-WRITE-MSG          
           END-IF.


       0150-WRITE-MSG.
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
           IF WRITE-MSG-CHOICE-FIELD = "g" THEN
               PERFORM 0120-DISPLAY-MENU
           ELSE IF WRITE-MSG-CHOICE-FIELD = "q" then
               STOP RUN   
           END-IF.
