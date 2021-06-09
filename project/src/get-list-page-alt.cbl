       IDENTIFICATION DIVISION.
       PROGRAM-ID. get-list-page-alt.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 NUM-OF-LINES PIC 999.
           01 WS-TABLE.
               05 WS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
                   NUM-OF-LINES.
                   10 WS-ID PIC XXX.
                   10 WS-TITLE PIC X(50).
                   10 WS-CONTENT PIC X(300).
                   10 WS-USERNAME PIC X(16).
           01 TEMP-ID PIC XXX.
           01 TEMP-TITLE PIC X(50).
           01 TEMP-CONTENT PIC X(300).
           01 TEMP-USERNAME PIC X(16).
           01 SUPPRESS-ZEROS PIC ZZZ.
           01 SEARCH-ID PIC XXX.
           01 LOOP-COUNTER PIC 999.
           01 SWAP-COUNTER-1 PIC 999.
           01 SWAP-COUNTER-2 PIC 999 VALUE 1.
           LINKAGE SECTION.
           01 LS-RETURN-TABLE.
               05 LS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
                 NUM-OF-LINES.
                   10 LS-ID PIC XXX.
                   10 LS-TITLE PIC X(50).
                   10 LS-CONTENT PIC X(300).
                   10 LS-USERNAME PIC X(16).
           01 NUM-LINES PIC 999.
       PROCEDURE DIVISION USING NUM-LINES LS-RETURN-TABLE.
           
           MOVE NUM-LINES TO NUM-OF-LINES.
           
          *>  ------------------------DEBUG-----------------------------
          *>  DISPLAY 'LOOP COUNTER IS: ' LOOP-COUNTER.
          *>  DISPLAY 'NUMBER OF LINES: ' NUM-OF-LINES.
          *> -----------------------DEBUG END---------------------------
           
          
           PERFORM UNTIL LOOP-COUNTER = NUM-OF-LINES
            
            ADD 1 TO LOOP-COUNTER
             MOVE LOOP-COUNTER TO SUPPRESS-ZEROS
             MOVE SUPPRESS-ZEROS TO SEARCH-ID
             MOVE FUNCTION TRIM(SEARCH-ID) TO SEARCH-ID 
            CALL 'list-message' USING SEARCH-ID TEMP-ID TEMP-TITLE 
              TEMP-CONTENT TEMP-USERNAME
            MOVE TEMP-ID TO WS-ID(LOOP-COUNTER)
            MOVE TEMP-TITLE TO WS-TITLE(LOOP-COUNTER)
            MOVE TEMP-CONTENT TO WS-CONTENT(LOOP-COUNTER)
            MOVE TEMP-USERNAME TO WS-USERNAME(LOOP-COUNTER)
               
           END-PERFORM.
          
          *>  vv keep this as initial commit of results
           MOVE WS-TABLE TO LS-RETURN-TABLE.
           
           
          *>  call id-sort here then move the result to the return table
          *> again:
           CALL 'id-sort' USING LS-RETURN-TABLE.
           MOVE 1 TO LOOP-COUNTER.
           
           
          
           
          
