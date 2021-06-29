       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-ABOUT-TITLE.
       DATA DIVISION.
      
           LINKAGE SECTION.
           01 ABOUT-OFFSET PIC 99.
           01 LS-ABOUT-NUM PIC 9.
           01 LS-ABOUT.
               05 LS-ABOUTS OCCURS 100 TIMES
               ASCENDING KEY IS LS-ABOUT-TITLE
               INDEXED BY ABOUT-IDX.
                   10 LS-ABOUT-TITLE PIC X(31).
                   10 LS-ABOUT-BODY PIC X(500).
                   
           01 ABOUT-TITLE-READ PIC X(31).
           

       PROCEDURE DIVISION USING ABOUT-OFFSET LS-ABOUT-NUM LS-ABOUT
       RETURNING ABOUT-TITLE-READ.
           
           IF LS-ABOUT-NUM = 1
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET) TO 
                       ABOUT-TITLE-READ        
           ELSE IF LS-ABOUT-NUM = 2
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 1) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 3
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 2) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 4
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 3) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 5
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 4) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 6
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 5) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 7
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 6) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 8
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 7) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 9
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 8) TO 
                       ABOUT-TITLE-READ
           ELSE IF LS-ABOUT-NUM = 10
                       MOVE LS-ABOUT-TITLE(ABOUT-OFFSET - 9) TO 
                       ABOUT-TITLE-READ                      
           END-IF.

           END FUNCTION DISPLAY-ABOUT-TITLE.
           
           