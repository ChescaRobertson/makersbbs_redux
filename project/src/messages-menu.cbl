       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESSAGES-MENU.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT F-MESSAGES-FILE ASSIGN TO "messages.dat"
                 ORGANISATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
               05 RC-MESSAGE-ID-NUM PIC 9(3).
               05 RC-MESSAGE-TITLE PIC X(60).
               05 RC-MESSAGE-BODY PIC X(300).
           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE ZERO.
       PROCEDURE DIVISION.
           OPEN INPUT F-MESSAGES-FILE.

           PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ F-MESSAGES-FILE
               NOT AT END
                 DISPLAY
                   FUNCTION TRIM(RC-MESSAGE-TITLE)
                 END-DISPLAY
               AT END  
                 MOVE 1 WS-FILE-IS-ENDED
             END-READ      
           END-PERFORM.

           CLOSE F-MESSAGES-FILE.
       
       
       