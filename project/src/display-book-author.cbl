       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-BOOK-AUTHOR.
       DATA DIVISION.
   
           LINKAGE SECTION.
           01 LS-LIBRARY-NUM UNSIGNED-INT.
           01 LS-BOOKS.
               05 LS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS LS-BOOK-AUTHOR-NAME
               INDEXED BY BOOK-IDX.
                   10 LS-BOOK-AUTHOR-NAME PIC X(12).
                   10 LS-BOOK-TITLE PIC X(30).
                   10 LS-BODY PIC X(500).
           01 BOOK-AUTHOR PIC X(12).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-LIBRARY-NUM LS-BOOKS 
       RETURNING BOOK-AUTHOR.
           
           IF LS-LIBRARY-NUM = 1
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET) 
                       TO BOOK-AUTHOR    
           ELSE IF LS-LIBRARY-NUM = 2
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 1) 
                       TO BOOK-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 3
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 2) 
                       TO BOOK-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 4
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 3) 
                       TO BOOK-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 5
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 4) 
                       TO BOOK-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 6
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 5) 
                       TO BOOK-AUTHOR 
           ELSE IF LS-LIBRARY-NUM = 7
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 6) 
                       TO BOOK-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 8
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 7) 
                       TO BOOK-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 9
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 8) 
                       TO BOOK-AUTHOR
           ELSE IF LS-LIBRARY-NUM = 10
                       MOVE LS-BOOK-AUTHOR-NAME (OFFSET - 9) 
                       TO BOOK-AUTHOR                       
           END-IF.

           END FUNCTION DISPLAY-BOOK-AUTHOR.
           