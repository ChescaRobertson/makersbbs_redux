       IDENTIFICATION DIVISION.
       PROGRAM-ID. list-message.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGES-FILE ASSIGN TO "messages.dat"
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
               05 RC-ID PIC XXX.
               05 RC-MESSAGE-TITLE PIC X(50).
               05 RC-MESSAGE-CONTENT PIC X(300).
           WORKING-STORAGE SECTION.
           01 SUPPRESS-ZEROS PIC ZZZ.
           01 NEW-ID PIC XXX.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.
           LINKAGE SECTION.
           01 LS-ID PIC XXX.
           01 LS-RETURN-ID PIC XXX.
           01 LS-RETURN-TITLE PIC X(50).
           01 LS-RETURN-CONTENT PIC X(300).
 
           PROCEDURE DIVISION USING LS-ID LS-RETURN-ID LS-RETURN-TITLE 
             LS-RETURN-CONTENT.
           OPEN INPUT F-MESSAGES-FILE.
          *> *>  ACCOUNTING FOR PIC 9s:
          *>  MOVE LS-ID TO SUPPRESS-ZEROS.
          *>  MOVE SUPPRESS-ZEROS TO NEW-ID.
          *>  MOVE FUNCTION TRIM(NEW-ID) TO NEW-ID.
          *> *>  ACCOUNT FOR PIC 9s end.
          *>  DISPLAY "First Argument is: " LS-ID.
          *>  DISPLAY "Trimming Input...".
           MOVE FUNCTION TRIM(LS-ID) TO LS-ID.
          *>  DISPLAY "Trimming output: " LS-ID.
           
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ F-MESSAGES-FILE
             NOT AT END
              *>  DISPLAY 'IN THE PERFORM BLOCK.'
               MOVE RC-ID TO SUPPRESS-ZEROS
               MOVE SUPPRESS-ZEROS TO RC-ID
               MOVE FUNCTION TRIM(RC-ID) TO RC-ID
            *>  DISPLAY 'Condition to meet is: 'LS-ID ' equal to: ' RC-ID
               IF LS-ID = RC-ID THEN
                *>  DISPLAY 'Condition true. Formatting and moving values:'
                   MOVE RC-ID TO LS-RETURN-ID
                   MOVE RC-MESSAGE-TITLE TO LS-RETURN-TITLE
                   MOVE RC-MESSAGE-CONTENT TO LS-RETURN-CONTENT
                 MOVE FUNCTION TRIM(LS-RETURN-ID) TO LS-RETURN-ID  
                 MOVE FUNCTION TRIM(LS-RETURN-TITLE) TO LS-RETURN-TITLE
                 MOVE FUNCTION TRIM(LS-RETURN-CONTENT) TO 
                   LS-RETURN-CONTENT
               END-IF
               
             AT END MOVE 1 TO WS-FILE-IS-ENDED
           
           END-PERFORM.
           MOVE 0 TO WS-FILE-IS-ENDED.
           CLOSE F-MESSAGES-FILE.
          *>  DISPLAY 'FINAL CHECK OF VALUES:'.
          *>  DISPLAY LS-RETURN-ID LS-RETURN-TITLE.
          
      
      *> I would love to know why passing in a non variable as the first
      *> argument completely breaks everything. Cobol is strange at times.
      