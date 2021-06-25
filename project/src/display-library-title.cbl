       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-LIBRARY-TITLE.
       DATA DIVISION.
      
           LINKAGE SECTION.
           01 OFFSET UNSIGNED-INT.
           01 LS-LIBRARY-NUM UNSIGNED-INT.
           01 LS-BOOKS.
               05 LS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS LS-BOOK-AUTHOR-NAME
               INDEXED BY BOOK-IDX.
                   10 LS-BOOK-AUTHOR-NAME PIC X(12).
                   10 LS-BOOK-TITLE PIC X(30).
                   10 LS-BODY PIC X(500).
           01 TITLE PIC X(60).
           

       PROCEDURE DIVISION USING OFFSET LS-LIBRARY-NUM LS-BOOKS 
       RETURNING TITLE.
           
           IF LS-LIBRARY-NUM = 1
                       MOVE LS-BOOK-TITLE(OFFSET) TO TITLE        
           ELSE IF LS-LIBRARY-NUM = 2
                       MOVE LS-BOOK-TITLE(OFFSET - 1) TO TITLE
           ELSE IF LS-LIBRARY-NUM = 3
                       MOVE LS-BOOK-TITLE(OFFSET - 2) TO TITLE 
           ELSE IF LS-LIBRARY-NUM = 4
                       MOVE LS-BOOK-TITLE(OFFSET - 3) TO TITLE 
           ELSE IF LS-LIBRARY-NUM = 5
                       MOVE LS-BOOK-TITLE(OFFSET - 4) TO TITLE
           ELSE IF LS-LIBRARY-NUM = 6
                       MOVE LS-BOOK-TITLE(OFFSET - 5) TO TITLE 
           ELSE IF LS-LIBRARY-NUM = 7
                       MOVE LS-BOOK-TITLE(OFFSET - 6) TO TITLE
           ELSE IF LS-LIBRARY-NUM = 8
                       MOVE LS-BOOK-TITLE(OFFSET - 7) TO TITLE
           ELSE IF LS-LIBRARY-NUM = 9
                       MOVE LS-BOOK-TITLE(OFFSET - 8) TO TITLE
           ELSE IF LS-LIBRARY-NUM = 10
                       MOVE LS-BOOK-TITLE(OFFSET - 9) TO TITLE                       
           END-IF.

           END FUNCTION DISPLAY-LIBRARY-TITLE.
           
           