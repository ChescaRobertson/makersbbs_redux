       IDENTIFICATION DIVISION.
       PROGRAM-ID. update-password.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
       
           SELECT F-USERS-FILE ASSIGN TO "users.dat"
             ORGANISATION IS SEQUENTIAL.
             
       DATA DIVISION.
           FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16). 
              05 USER-PASSWORD PIC X(20).  
              05 USER-ACNT-NUM PIC X(8). 
              05 FILLER PIC XX VALUE SPACES. 
              05 USER-CREDIT PIC 999. 
              05 FILLER PIC X VALUE X'0A'.

           WORKING-STORAGE SECTION.
           01 WS-USER-FILE-IS-ENDED PIC 9 VALUE 0.

           LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-UPDATED-PASSWORD PIC X(20).
           
           
     
       PROCEDURE DIVISION USING LS-USERNAME LS-UPDATED-PASSWORD.

           OPEN I-O F-USERS-FILE.
           MOVE 0 TO WS-USER-FILE-IS-ENDED.

           PERFORM UNTIL WS-USER-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       IF LS-USERNAME = USERNAME
                           MOVE LS-UPDATED-PASSWORD TO USER-PASSWORD
                           REWRITE USERS 
                       END-IF
                  AT END 
                       MOVE 1 TO WS-USER-FILE-IS-ENDED
               END-READ
           END-PERFORM.
           
           CLOSE F-USERS-FILE.

   