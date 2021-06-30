       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-TITLE.
       DATA DIVISION.
      
           LINKAGE SECTION.
           01 OFFSET UNSIGNED-INT.
           01 LS-TABLE-NUM UNSIGNED-INT.
           01 LS-TABLES.
               05 LS-TABLE OCCURS 100 TIMES
               ASCENDING KEY IS LS-AUTHOR-NAME
               INDEXED BY TABLE-IDX.
                   10 LS-AUTHOR-NAME PIC X(12).
                   10 LS-TITLE PIC X(31).
                   10 LS-BODY PIC X(500).
           01 TITLE PIC X(31).
           

       PROCEDURE DIVISION USING OFFSET LS-TABLE-NUM LS-TABLES
       RETURNING TITLE.
           
           IF LS-TABLE-NUM = 1
                       MOVE LS-TITLE(OFFSET) TO 
                      TITLE        
           ELSE IF LS-TABLE-NUM = 2
                       MOVE LS-TITLE(OFFSET - 1) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 3
                       MOVE LS-TITLE(OFFSET - 2) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 4
                       MOVE LS-TITLE(OFFSET - 3) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 5
                       MOVE LS-TITLE(OFFSET - 4) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 6
                       MOVE LS-TITLE(OFFSET - 5) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 7
                       MOVE LS-TITLE(OFFSET - 6) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 8
                       MOVE LS-TITLE(OFFSET - 7) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 9
                       MOVE LS-TITLE(OFFSET - 8) TO 
                      TITLE
           ELSE IF LS-TABLE-NUM = 10
                       MOVE LS-TITLE(OFFSET - 9) TO 
                      TITLE                      
           END-IF.

           END FUNCTION DISPLAY-TITLE.
           
           