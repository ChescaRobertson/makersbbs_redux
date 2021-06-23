       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-BOOK-AUTHOR.
       DATA DIVISION.
   
           LINKAGE SECTION.
           01 LS-LIBRARY-NUM UNSIGNED-INT.
           01 LS-BOOKS.
               05 LS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS LS-TITLE
               INDEXED BY BOOK-IDX.
                   10 LS-TITLE PIC X(60).
                   10 LS-BODY PIC X(500).
                   10 LS-DATE PIC X(10).
                   10 LS-AUTHOR PIC X(10).
           01 POST-AUTHOR PIC X(10).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-LIBRARY-NUM LS-BOOKS 
       RETURNING POST-AUTHOR.
           
           IF LS-LIBRARY-NUM = 1
                       MOVE LS-AUTHOR (OFFSET) TO POST-AUTHOR    
           ELSE IF LS-LIBRARY-NUM = 2
                       MOVE LS-AUTHOR (OFFSET - 1) TO POST-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 3
                       MOVE LS-AUTHOR (OFFSET - 2) TO POST-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 4
                       MOVE LS-AUTHOR (OFFSET - 3) TO POST-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 5
                       MOVE LS-AUTHOR (OFFSET - 4) TO POST-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 6
                       MOVE LS-AUTHOR (OFFSET - 5) TO POST-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 7
                       MOVE LS-AUTHOR (OFFSET - 6) TO POST-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 8
                       MOVE LS-AUTHOR (OFFSET - 7) TO POST-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 9
                       MOVE LS-AUTHOR (OFFSET - 8) TO POST-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 10
                       MOVE LS-AUTHOR (OFFSET - 9) TO POST-AUTHOR                       
           END-IF.

           END FUNCTION DISPLAY-BOOK-AUTHOR.
           