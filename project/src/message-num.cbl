       IDENTIFICATION DIVISION.
       FUNCTION-ID. MESSAGE-NUM.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGE-FILE ASSIGN TO "messages.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGE-FILE.
           01 MESSAGES.
              05 MESSAGE-TITLE PIC X(60).
              05 MESSAGE-BODY PIC X(500).
           01 NUM UNSIGNED-INT.
           WORKING-STORAGE SECTION.
           01 COUNTER UNSIGNED-INT.
           01 WS-FILE-IS-ENDED PIC 9.
           01 NUM1 UNSIGNED-INT.
           01 NUM2 UNSIGNED-INT.
           01 NUM3 UNSIGNED-INT.
           01 NUM4 UNSIGNED-INT.
           01 NUM5 UNSIGNED-INT.
           01 NUM6 UNSIGNED-INT.
           01 NUM7 UNSIGNED-INT.
           01 NUM8 UNSIGNED-INT.
           01 NUM9 UNSIGNED-INT.
           01 NUM10 UNSIGNED-INT.
           
           01 WS-MESSAGES.
              05 WS-MESSAGE OCCURS 10 TIMES
              ASCENDING KEY IS WS-TITLE
              INDEXED BY MSG-IDX.
                  10 WS-TITLE PIC X(60).
           LINKAGE SECTION.
      *     01 LS-COUNTER UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).
       PROCEDURE DIVISION RETURNING LS-MESSAGE.
           SET MSG-IDX TO 0.
           SET COUNTER TO 0.
           MOVE 0 TO WS-FILE-IS-ENDED.
           OPEN INPUT F-MESSAGE-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-MESSAGE-FILE
                   NOT AT END
                       ADD 1 TO MSG-IDX
                       ADD 1 TO COUNTER
                    IF COUNTER = 1                     
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 2                       
                       MOVE MSG-IDX TO NUM
                     END-IF
                    IF COUNTER = 3                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 4                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 5                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 6                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 7                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 8                       
                       MOVE MSG-IDX TO NUM
                    END-IF
                    IF COUNTER = 9                       
                       MOVE MSG-IDX TO NUM
                    ELSE
                        MOVE MSG-IDX TO NUM
                    END-IF 
                 MOVE MESSAGE-TITLE TO LS-MESSAGE
                           
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED 
               END-READ 
           END-PERFORM.
           CLOSE F-MESSAGE-FILE.
           END FUNCTION MESSAGE-NUM.
