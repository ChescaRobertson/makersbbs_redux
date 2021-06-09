       IDENTIFICATION DIVISION.
       PROGRAM-ID. id-sort.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 NUM-OF-LINES PIC 999.
           01 LOOP-COUNT PIC 999.
           01 REVERSE-ID PIC 999.
           01 SUPPRESS-ZEROS PIC ZZZ.
           01 FINAL-ID PIC XXX.
           01 HOLDER-TABLE.
               05 HOLD-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON 
                NUM-OF-LINES.
                   10 HOLD-ID PIC XXX.
                   10 HOLD-TITLE PIC X(50).
                   10 HOLD-CONTENT PIC X(300).
                   10 HOLD-USERNAME PIC X(16).
           LINKAGE SECTION.
           01 SORTED-TABLE.
               05 S-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON 
                NUM-OF-LINES.
                   10 S-ID PIC XXX.
                   10 S-TITLE PIC X(50).
                   10 S-CONTENT PIC X(300).
                   10 S-USERNAME PIC X(16).
           PROCEDURE DIVISION USING SORTED-TABLE.
           CALL 'number-of-file-lines' USING NUM-OF-LINES.
           MOVE NUM-OF-LINES TO REVERSE-ID.
      
           PERFORM UNTIL LOOP-COUNT = NUM-OF-LINES
               ADD 1 TO LOOP-COUNT
               MOVE REVERSE-ID TO SUPPRESS-ZEROS
               MOVE SUPPRESS-ZEROS TO FINAL-ID
               
               MOVE FUNCTION TRIM(FINAL-ID) TO S-ID(LOOP-COUNT)
               SUBTRACT 1 FROM REVERSE-ID
           END-PERFORM.
          *>  REMEMBER to put these moves after the loops!!!!!!!!!!!
           MOVE 0 TO LOOP-COUNT.
           MOVE NUM-OF-LINES TO REVERSE-ID.
          
          *> SOLUTION BELOW VVVVVVV ------------------------------------
           PERFORM UNTIL LOOP-COUNT = NUM-OF-LINES
             ADD 1 TO LOOP-COUNT
             MOVE S-ENTRY(REVERSE-ID) TO HOLD-ENTRY(LOOP-COUNT)
             SUBTRACT 1 FROM REVERSE-ID
           END-PERFORM.
          *>  ------------------DEBUG CHECKING VALUES-------------------
              *>  DISPLAY 'first value in index 1 for S-ENTRY:'.
              *>  DISPLAY S-ID(1) S-TITLE(1).
              *>  DISPLAY 'last value in HOLD-ENTRY TABLE: '.
              *>  DISPLAY HOLD-ID(NUM-OF-LINES) HOLD-TITLE(NUM-OF-LINES).
              *>  DISPLAY '----------------------------------------------'.
              *>  DISPLAY 'Now the first value in HOLD-ENTRY TABLE:'.
              *>  DISPLAY HOLD-ID(1) HOLD-TITLE(1).
               
          *>  ------------------------END-DEBUG-------------------------
           MOVE 0 TO LOOP-COUNT.
           MOVE NUM-OF-LINES TO REVERSE-ID.
           MOVE HOLDER-TABLE TO SORTED-TABLE.
           