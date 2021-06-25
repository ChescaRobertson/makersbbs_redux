       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-ABOUT-TITLE.
       DATA DIVISION.
      
           LINKAGE SECTION.
           01 LS-ABOUT-NUM UNSIGNED-INT.
           01 LS-ABOUT.
               05 LS-ABOUTS OCCURS 100 TIMES
               ASCENDING KEY IS LS-ABOUT-TITLE
               INDEXED BY ABOUT-IDX.
                   10 LS-ABOUT-TITLE PIC X(60).
                   10 LS-ABOUT-BODY PIC X(500).
           01 LS-READ-TITLE PIC X(60).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-ABOUT-NUM LS-ABOUT 
       RETURNING LS-READ-TITLE.
           
           IF LS-ABOUT-NUM = 1
                       MOVE LS-ABOUT-TITLE(OFFSET) TO LS-READ-TITLE        
           ELSE IF LS-ABOUT-NUM = 2
                       MOVE LS-ABOUT-TITLE(OFFSET - 1) TO LS-READ-TITLE
           ELSE IF LS-ABOUT-NUM = 3
                       MOVE LS-ABOUT-TITLE(OFFSET - 2) TO LS-READ-TITLE 
           ELSE IF LS-ABOUT-NUM = 4
                       MOVE LS-ABOUT-TITLE(OFFSET - 3) TO LS-READ-TITLE 
           ELSE IF LS-ABOUT-NUM = 5
                       MOVE LS-ABOUT-TITLE(OFFSET - 4) TO LS-READ-TITLE
           ELSE IF LS-ABOUT-NUM = 6
                       MOVE LS-ABOUT-TITLE(OFFSET - 5) TO LS-READ-TITLE 
           ELSE IF LS-ABOUT-NUM = 7
                       MOVE LS-ABOUT-TITLE(OFFSET - 6) TO LS-READ-TITLE
           ELSE IF LS-ABOUT-NUM = 8
                       MOVE LS-ABOUT-TITLE(OFFSET - 7) TO LS-READ-TITLE
           ELSE IF LS-ABOUT-NUM = 9
                       MOVE LS-ABOUT-TITLE(OFFSET - 8) TO LS-READ-TITLE
           ELSE IF LS-ABOUT-NUM = 10
                       MOVE LS-ABOUT-TITLE(OFFSET - 9) TO LS-READ-TITLE                       
           END-IF.

           END FUNCTION DISPLAY-ABOUT-TITLE.
           
           