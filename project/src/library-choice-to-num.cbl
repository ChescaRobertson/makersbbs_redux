       IDENTIFICATION DIVISION.
       FUNCTION-ID. LIBRARY-CHOICE-TO-NUM.
       DATA DIVISION.
           LINKAGE SECTION.
           01 LS-LIBRARY-CHOICE PIC X(2).
           01 LS-LIBRARY-NUM UNSIGNED-INT.
           01 LS-RESULT UNSIGNED-INT.

       PROCEDURE DIVISION USING LS-LIBRARY-CHOICE 
       RETURNING LS-LIBRARY-NUM.

           IF LS-LIBRARY-CHOICE = "1" 
               SET LS-LIBRARY-NUM TO 1 
           ELSE IF LS-LIBRARY-CHOICE = "2" 
               SET LS-LIBRARY-NUM TO 2 
           ELSE IF LS-LIBRARY-CHOICE = "3" 
               SET LS-LIBRARY-NUM TO 3 
           ELSE IF LS-LIBRARY-CHOICE = "4" 
               SET LS-LIBRARY-NUM TO 4 
           ELSE IF LS-LIBRARY-CHOICE = "5" 
               SET LS-LIBRARY-NUM TO 5 
           ELSE IF LS-LIBRARY-CHOICE = "6" 
               SET LS-LIBRARY-NUM TO 6 
           ELSE IF LS-LIBRARY-CHOICE = "7" 
               SET LS-LIBRARY-NUM TO 7 
           ELSE IF LS-LIBRARY-CHOICE = "8" 
               SET LS-LIBRARY-NUM TO 8 
           ELSE IF LS-LIBRARY-CHOICE = "9" 
               SET LS-LIBRARY-NUM TO 9
           ELSE IF LS-LIBRARY-CHOICE = "10" 
               SET LS-LIBRARY-NUM TO 10 
           END-IF.

           END FUNCTION LIBRARY-CHOICE-TO-NUM.