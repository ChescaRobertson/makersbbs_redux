       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-msg-menu.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 RETURN-COUNTER PIC 999.

           01 WS-ID PIC XXX.
           01 RETURN-ID PIC XXX.
           01 RETURN-TITLE PIC X(50).
           01 RETURN-CONTENT PIC X(300).
           01 USERNAME PIC X(16).

           01 LS-RETURN-TABLE.
               05 LS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
                 NUM-OF-LINES.
                   10 LS-ID PIC 999.
                   10 LS-TITLE PIC X(50).
                   10 LS-CONTENT PIC X(300).
                   10 LS-USERNAME PIC X(16).
           01 NUM-OF-LINES PIC 999.
       PROCEDURE DIVISION.
           
       TEST-NUMBER-OF-FILE-LINES.
           SET ENVIRONMENT "messages_dat" TO "messages.dat".
           CALL "number-of-file-lines" USING RETURN-COUNTER.
           CALL 'assert-equals' USING "005" RETURN-COUNTER.
       
       TEST-LIST-MESSAGES.
           SET ENVIRONMENT "messages_dat" TO "list_messages.dat".
           CALL "list-message" USING  WS-ID RETURN-ID, RETURN-TITLE, 
           RETURN-CONTENT, USERNAME.
           
           