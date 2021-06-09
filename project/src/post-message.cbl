       IDENTIFICATION DIVISION.
       PROGRAM-ID. post-message.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGES-FILE ASSIGN TO 'messages.dat'
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
             05 RC-ID PIC 999.
             05 RC-TITLE PIC X(50).
             05 RC-CONTENT PIC X(300).
             05 RC-USERNAME PIC X(16).
           WORKING-STORAGE SECTION.
           01 WS-NUM-OF-LINES PIC 999.
           01 WS-MESSAGE.
             05 WS-ID PIC 999.
             05 WS-TITLE PIC X(50).
             05 WS-CONTENT PIC X(300).  
             05 WS-USERNAME PIC X(16).  
           LINKAGE SECTION.
           01 LS-MESSAGE.
            *>  05 LS-ID PIC 999.
             05 LS-TITLE PIC X(50).
             05 LS-CONTENT PIC X(300).
             05 LS-USERNAME PIC X(16).
          *>  PUT THIS VARIABLE IN THE WORKING STORAGE IN SERVER ^^
       PROCEDURE DIVISION USING LS-MESSAGE.
           
           CALL 'number-of-file-lines' USING WS-NUM-OF-LINES.
           
           ADD 1 TO WS-NUM-OF-LINES.
           MOVE WS-NUM-OF-LINES TO WS-ID.
           MOVE LS-TITLE TO WS-TITLE.
           MOVE FUNCTION TRIM(LS-CONTENT) TO WS-CONTENT.
           MOVE LS-USERNAME TO WS-USERNAME.
           OPEN EXTEND F-MESSAGES-FILE.
           MOVE WS-MESSAGE TO RC-MESSAGE.
           WRITE RC-MESSAGE.
           CLOSE F-MESSAGES-FILE.
           