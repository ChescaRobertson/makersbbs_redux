       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-AUTHOR.
       DATA DIVISION.
   
           LINKAGE SECTION.
           01 OFFSET UNSIGNED-INT.
           01 LS-TABLE-NUM UNSIGNED-INT.
           01 LS-TABLES.
               05 LS-TABLE OCCURS 100 TIMES
               ASCENDING KEY IS LS-AUTHOR-NAME
               INDEXED BY BOOK-IDX.
                   10 LS-AUTHOR-NAME PIC X(12).
                   10 LS-TITLE PIC X(31).
                   10 LS-BODY PIC X(500).
           01 AUTHOR-NAME PIC X(12).
           

       PROCEDURE DIVISION USING OFFSET LS-TABLE-NUM LS-TABLES 
       RETURNING AUTHOR-NAME.
           
           IF LS-TABLE-NUM = 1
                       MOVE LS-AUTHOR-NAME (OFFSET) 
                       TO AUTHOR-NAME   
           ELSE IF LS-TABLE-NUM = 2
                       MOVE LS-AUTHOR-NAME (OFFSET - 1) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 3
                       MOVE LS-AUTHOR-NAME (OFFSET - 2) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 4
                       MOVE LS-AUTHOR-NAME (OFFSET - 3) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 5
                       MOVE LS-AUTHOR-NAME (OFFSET - 4) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 6
                       MOVE LS-AUTHOR-NAME (OFFSET - 5) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 7
                       MOVE LS-AUTHOR-NAME (OFFSET - 6) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 8
                       MOVE LS-AUTHOR-NAME (OFFSET - 7) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 9
                       MOVE LS-AUTHOR-NAME (OFFSET - 8) 
                       TO AUTHOR-NAME
           ELSE IF LS-TABLE-NUM = 10
                       MOVE LS-AUTHOR-NAME (OFFSET - 9) 
                       TO AUTHOR-NAME                      
           END-IF.

           END FUNCTION DISPLAY-AUTHOR.
           