       IDENTIFICATION DIVISION.
           FUNCTION-ID. VERIFY-PASSWORD.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT section.
           FILE-CONTROL.
           SELECT F-USERS-FILE ASSIGN TO 'users.dat'
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(10).  
              05 USER-CREDIT PIC 99. 

           WORKING-STORAGE SECTION.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-PASSWORD-ENTRY PIC X(20).
           01 LS-RESULT PIC X(5).

       PROCEDURE DIVISION USING LS-USERNAME, LS-PASSWORD-ENTRY 
       RETURNING LS-RESULT.

           OPEN INPUT F-USERS-FILE 
             PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ F-USERS-FILE
             NOT AT END
               IF LS-PASSWORD-ENTRY = USER-PASSWORD
                    MOVE 'TRUE' TO LS-RESULT
               ELSE 
                   MOVE 'FALSE' TO LS-RESULT
               END-IF
           END-PERFORM.

           CLOSE F-USERS-FILE.
           

           END FUNCTION VERIFY-PASSWORD.
