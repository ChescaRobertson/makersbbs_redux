       IDENTIFICATION DIVISION.
       FUNCTION-ID. ABOUT-CHOICE-TO-NUM.
       DATA DIVISION.
           LINKAGE SECTION.
           01 LS-ABOUT-CHOICE PIC X.
           01 LS-ABOUT-NUM PIC 9.
           01 LS-RESULT UNSIGNED-INT.

       PROCEDURE DIVISION USING LS-ABOUT-CHOICE. 
       RETURNING LS-ABOUT-NUM.

           IF LS-ABOUT-CHOICE = "1" 
               SET LS-ABOUT-NUM TO 1 
           ELSE IF LS-ABOUT-CHOICE = "2" 
               SET LS-ABOUT-NUM TO 2 
           ELSE IF LS-ABOUT-CHOICE = "3" 
               SET LS-ABOUT-NUM TO 3 
           ELSE IF LS-ABOUT-CHOICE = "4" 
               SET LS-ABOUT-NUM TO 4 
           ELSE IF LS-ABOUT-CHOICE = "5" 
               SET LS-ABOUT-NUM TO 5
           end-if. 
           

           END FUNCTION ABOUT-CHOICE-TO-NUM.
           