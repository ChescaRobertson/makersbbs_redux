       IDENTIFICATION DIVISION.
       FUNCTION-ID. CHOICE-TO-NUM.
       DATA DIVISION.
           LINKAGE SECTION.
           01 LS-CHOICE PIC X(2).
           01 LS-NUM UNSIGNED-INT.
           01 LS-RESULT UNSIGNED-INT.

       PROCEDURE DIVISION USING LS-CHOICE RETURNING LS-NUM.

           IF LS-CHOICE = "1" 
               SET LS-NUM TO 1 
           ELSE IF LS-CHOICE = "2" 
               SET LS-NUM TO 2 
           ELSE IF LS-CHOICE = "3" 
               SET LS-NUM TO 3 
           ELSE IF LS-CHOICE = "4" 
               SET LS-NUM TO 4 
           ELSE IF LS-CHOICE = "5" 
               SET LS-NUM TO 5
           END-IF.

           END FUNCTION CHOICE-TO-NUM.
           