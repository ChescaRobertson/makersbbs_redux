       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-BODY.
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
           01 BODY PIC X(500).
           

       PROCEDURE DIVISION USING OFFSET LS-TABLE-NUM LS-TABLES
       RETURNING BODY.
       
           IF LS-TABLE-NUM = 1
                       MOVE LS-BODY(OFFSET) TO BODY    
           ELSE IF LS-TABLE-NUM = 2
                       MOVE LS-BODY(OFFSET - 1) TO BODY
           ELSE IF LS-TABLE-NUM = 3
                       MOVE LS-BODY(OFFSET - 2) TO BODY 
           ELSE IF LS-TABLE-NUM = 4
                       MOVE LS-BODY(OFFSET - 3) TO BODY 
           ELSE IF LS-TABLE-NUM = 5
                       MOVE LS-BODY(OFFSET - 4) TO BODY
           ELSE IF LS-TABLE-NUM = 6
                       MOVE LS-BODY(OFFSET - 5) TO BODY 
           ELSE IF LS-TABLE-NUM = 7
                       MOVE LS-BODY(OFFSET - 6) TO BODY
           ELSE IF LS-TABLE-NUM = 8
                       MOVE LS-BODY(OFFSET - 7) TO BODY
           ELSE IF LS-TABLE-NUM = 9
                       MOVE LS-BODY(OFFSET - 8) TO BODY
           ELSE IF LS-TABLE-NUM = 10
                       MOVE LS-BODY(OFFSET - 9) TO BODY                       
           END-IF.

           END FUNCTION DISPLAY-BODY.
           