       IDENTIFICATION DIVISION.
       PROGRAM-ID. generate-library-table.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION. 
           FILE-CONTROL.
               SELECT F-LIBRARY-FILE ASSIGN TO 'library.dat'
                 ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           
           FILE SECTION.

           FD F-LIBRARY-FILE.
           01 LIBRARY.
               05 BOOK-AUTHOR PIC X(12).
               05 BOOK-TITLE PIC X(30).

      
           WORKING-STORAGE SECTION.
           01 COUNTER PIC 99.
           01 WS-FILE-IS-ENDED PIC X.
           01 PAGE-NUM UNSIGNED-INT.
           01 LIBRARY-NUM UNSIGNED-INT.
           01 OFFSET PIC 99.

           LINKAGE SECTION.
           01 LS-BOOKS.
               05 LS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS LS-BOOK-AUTHOR-NAME
               INDEXED BY LS-BOOK-IDX.
                   10 LS-BOOK-AUTHOR-NAME PIC X(12).
                   10 LS-BOOK-TITLE PIC X(30).
           01 LS-LIBRARY-DISPLAY-MESSAGE PIC X(40). 
           
           
           
           PROCEDURE DIVISION USING LS-BOOKS LS-LIBRARY-DISPLAY-MESSAGE.

           SET COUNTER TO 0.
           OPEN INPUT F-LIBRARY-FILE.
           MOVE 0 TO WS-FILE-IS-ENDED.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-LIBRARY-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE BOOK-AUTHOR 
                       TO LS-BOOK-AUTHOR-NAME(COUNTER)
                       MOVE BOOK-TITLE
                       TO LS-BOOK-TITLE(COUNTER)
                   AT END
                       MOVE 1 TO WS-FILE-IS-ENDED
                       MOVE COUNTER TO OFFSET
                       MOVE 1 TO PAGE-NUM
                       MOVE 1 TO LIBRARY-NUM
                       MOVE "Here are the last 5 books" TO 
                       LS-LIBRARY-DISPLAY-MESSAGE
               END-READ
           END-PERFORM.
           CLOSE F-LIBRARY-FILE.
                   

