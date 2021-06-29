       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-add-credits.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-WINNINGS PIC 999.
           01 LS-CREDIT-BALANCE PIC 999.
        
       PROCEDURE DIVISION.

       TEST-ADD-CREDITS.
           MOVE "Chesca" TO LS-USERNAME
           MOVE "10" TO LS-WINNINGS
           SET ENVIRONMENT "users_dat" TO "users-win-credits.dat".
           CALL "add-credits" USING LS-USERNAME, LS-WINNINGS, 
           LS-CREDIT-BALANCE.
           
   