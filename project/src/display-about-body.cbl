              IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-ABOUT-BODY.
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
                   
           01 ABOUT-BODY-READ  PIC X(500).
           

       PROCEDURE DIVISION USING ABOUT-OFFSET LS-ABOUT-NUM LS-ABOUT
       RETURNING ABOUT-BODY-READ.
           
           IF LS-ABOUT-NUM = 1
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET) TO 
                       ABOUT-BODY-READ        
           ELSE IF LS-ABOUT-NUM = 2
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 1) TO 
                       ABOUT-BODY-READ
           ELSE IF LS-ABOUT-NUM = 3
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 2) TO 
                       ABOUT-BODY-READ
           ELSE IF LS-ABOUT-NUM = 4
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 3) TO 
                       ABOUT-BODY-READ 
           ELSE IF LS-ABOUT-NUM = 5
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 4) TO 
                       ABOUT-BODY-READ 
           ELSE IF LS-ABOUT-NUM = 6
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 5) TO 
                       ABOUT-BODY-READ 
           ELSE IF LS-ABOUT-NUM = 7
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 6) TO 
                       ABOUT-BODY-READ 
           ELSE IF LS-ABOUT-NUM = 8
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 7) TO 
                       ABOUT-BODY-READ 
           ELSE IF LS-ABOUT-NUM = 9
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 8) TO 
                       ABOUT-BODY-READ
           ELSE IF LS-ABOUT-NUM = 10
                       MOVE LS-ABOUT-BODY(ABOUT-OFFSET - 9) TO 
                       ABOUT-BODY-READ                      
           END-IF.

           END FUNCTION DISPLAY-ABOUT-BODY.
           
           