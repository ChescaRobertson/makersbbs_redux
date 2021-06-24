       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-hangman.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.              
               FUNCTION HIGH-SCORE-CALCULATOR.
           

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 TEST-WORD-LENGTH PIC 99.
           01 TEST-GUESSES-LEFT PIC 99.

       PROCEDURE DIVISION.

           MOVE '5' TO TEST-WORD-LENGTH.
           MOVE '6' TO TEST-GUESSES-LEFT.

       0100-test-high-score-calculator.
           CALL 'assert-equals' USING 
           HIGH-SCORE-CALCULATOR(TEST-WORD-LENGTH TEST-GUESSES-LEFT) 
           '30'.
           