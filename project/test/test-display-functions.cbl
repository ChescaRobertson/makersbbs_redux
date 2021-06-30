       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-display-functions.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION DISPLAY-BODY
               FUNCTION DISPLAY-AUTHOR
               FUNCTION DISPLAY-TITLE
               FUNCTION CHOICE-TO-NUM.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 OFFSET UNSIGNED-INT.
           01 WS-TABLE-NUM UNSIGNED-INT.
           01 WS-TABLES.
               05 WS-TABLE OCCURS 100 TIMES
               ASCENDING KEY IS WS-AUTHOR-NAME
               INDEXED BY BOOK-IDX.
                   10 WS-AUTHOR-NAME PIC X(12).
                   10 WS-TITLE PIC X(31).
                   10 WS-BODY PIC X(500).

           01 BODY PIC X(500).
           01 TITLE PIC X(31).
           01 AUTHOR-NAME PIC X(12).
           
           01 WS-CHOICE PIC X(2).

       PROCEDURE DIVISION.

       TEST-DISPLAY-BODY.
       MOVE 5 TO OFFSET
       MOVE 1 TO WS-TABLE-NUM
       MOVE "C Dickens" TO WS-AUTHOR-NAME(5)
       MOVE "Great Expectations" TO WS-TITLE(5)
       MOVE "My father's family name being Pirrip" TO WS-BODY(5)
       CALL "assert-equals" USING DISPLAY-BODY(OFFSET, WS-TABLE-NUM, 
       WS-TABLES) "My father's family name being Pirrip". 

       TEST-DISPLAY-TITLE.
       MOVE 5 TO OFFSET
       MOVE 1 TO WS-TABLE-NUM
       MOVE "C Dickens" TO WS-AUTHOR-NAME(5)
       MOVE "Great Expectations" TO WS-TITLE(5)
       MOVE "My father's family name being Pirrip" TO WS-BODY(5)
       CALL "assert-equals" USING DISPLAY-TITLE(OFFSET, WS-TABLE-NUM, 
       WS-TABLES) "Great Expectations". 

       TEST-DISPLAY-AUTHOR.
       MOVE 5 TO OFFSET
       MOVE 1 TO WS-TABLE-NUM
       MOVE "C Dickens" TO WS-AUTHOR-NAME(5)
       MOVE "Great Expectations" TO WS-TITLE(5)
       MOVE "My father's family name being Pirrip" TO WS-BODY(5)
       CALL "assert-equals" USING DISPLAY-AUTHOR(OFFSET, WS-TABLE-NUM, 
       WS-TABLES) "C Dickens". 

       TEST-CHOICE-TO-NUM.
       MOVE "3" TO WS-CHOICE
       CALL "assert-equals" USING CHOICE-TO-NUM(WS-CHOICE) 3.


         
           
       
   