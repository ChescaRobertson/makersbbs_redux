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
               05 BOOK-TITLE PIC X(31).
               05 BOOK-BODY PIC X(500).

      
           WORKING-STORAGE SECTION.
           01 COUNTER UNSIGNED-INT.
           01 WS-FILE-IS-ENDED UNSIGNED-INT.
           01 LIBRARY-NUM UNSIGNED-INT.
  

           LINKAGE SECTION.
           01 LS-BOOKS.
               05 LS-BOOK OCCURS 100 TIMES
               ASCENDING KEY IS LS-BOOK-AUTHOR-NAME
               INDEXED BY LS-BOOK-IDX.
                   10 LS-BOOK-AUTHOR-NAME PIC X(12).
                   10 LS-BOOK-TITLE PIC X(31).
                   10 LS-BODY PIC X(500).
           01 LS-LIBRARY-DISPLAY-MESSAGE PIC X(40).
           01 LS-OFFSET UNSIGNED-INT. 
           01 PAGE-NUM PIC 99.
           
           
           PROCEDURE DIVISION USING LS-BOOKS LS-LIBRARY-DISPLAY-MESSAGE
           LS-OFFSET PAGE-NUM.

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
                       MOVE BOOK-BODY TO
                       LS-BODY(COUNTER)
                   AT END
                       MOVE 1 TO WS-FILE-IS-ENDED
                       MOVE COUNTER TO LS-OFFSET
                       MOVE 1 TO PAGE-NUM
                       MOVE 1 TO LIBRARY-NUM
                       MOVE "Here are the last 5 books" TO 
                       LS-LIBRARY-DISPLAY-MESSAGE
               END-READ
           END-PERFORM.
           CLOSE F-LIBRARY-FILE.
                   

