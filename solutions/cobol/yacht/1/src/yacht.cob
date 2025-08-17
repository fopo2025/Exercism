       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       01 WS-ARR.
         10 WS-DIE PIC 9 OCCURS 6 TIMES INDEXED BY IDX.
       01 I PIC 9.
       01 J PIC 9.
       01 TEMP PIC 99.
      
       PROCEDURE DIVISION.
      
         YACHT.
         MOVE 0 TO WS-RESULT.
         MOVE 0 TO I.
         MOVE 0 TO J.
         MOVE 0 TO TEMP.
         INITIALIZE WS-ARR.
         
         PERFORM 5 TIMES
           DIVIDE WS-DICE BY 10 GIVING WS-DICE REMAINDER TEMP
             ADD 1 TO WS-DIE(TEMP)
         END-PERFORM.
      
         EVALUATE WS-CATEGORY
           WHEN "ones"
             MOVE WS-DIE(1) TO WS-RESULT
           
           WHEN "twos"
             MULTIPLY WS-DIE(2) BY 2 GIVING WS-RESULT
      
           WHEN "threes"
             MULTIPLY WS-DIE(3) BY 3 GIVING WS-RESULT
      
           WHEN "fours"
             MULTIPLY WS-DIE(4) BY 4 GIVING WS-RESULT
      
           WHEN "fives"
             MULTIPLY WS-DIE(5) BY 5 GIVING WS-RESULT
      
           WHEN "sixes"
             MULTIPLY WS-DIE(6) BY 6 GIVING WS-RESULT

           WHEN "full house"
             SET IDX TO 1
      
             PERFORM 6 TIMES
               IF WS-DIE(IDX) IS EQUAL TO 3 THEN
                 MOVE IDX TO I
               END-IF
               IF WS-DIE(IDX) IS EQUAL TO 2 THEN
                 MOVE IDX TO J
               END-IF
               SET IDX UP BY 1
             END-PERFORM

             IF (I IS NOT EQUAL TO 0) AND (J IS NOT EQUAL TO 0)
               MULTIPLY I BY 3 GIVING WS-RESULT
               MULTIPLY J BY 2 GIVING TEMP
               ADD TEMP TO WS-RESULT
             END-IF

           WHEN "four of a kind"
             SET IDX TO 1
      
             PERFORM 6 TIMES
               IF WS-DIE(IDX) IS GREATER THAN OR EQUAL TO 4 THEN
                 MOVE IDX TO I
               END-IF
               SET IDX UP BY 1
             END-PERFORM
      
             MULTIPLY I BY 4 GIVING WS-RESULT

           WHEN "little straight"
             SET IDX TO 1
      
             PERFORM 5 TIMES
               IF WS-DIE(IDX) IS NOT EQUAL TO 1 THEN
                 EXIT PERFORM
               END-IF
               SET IDX UP BY 1
             END-PERFORM

             IF IDX IS EQUAL TO 6 THEN
               MOVE 30 TO WS-RESULT
             END-IF
      
           WHEN "big straight"
             SET IDX TO 2
      
             PERFORM 5 TIMES
               IF WS-DIE(IDX) IS NOT EQUAL TO 1 THEN
                 EXIT PERFORM
               END-IF
               SET IDX UP BY 1
             END-PERFORM

             IF IDX IS EQUAL TO 7 THEN
               MOVE 30 TO WS-RESULT
             END-IF
      
           WHEN "choice"
             SET IDX TO 1
      
             PERFORM 6 TIMES
               MULTIPLY WS-DIE(IDX) BY IDX GIVING TEMP
               ADD TEMP TO WS-RESULT
               SET IDX UP BY 1
             END-PERFORM
               
           WHEN "yacht"
             SET IDX TO 1
      
             PERFORM 6 TIMES
               IF WS-DIE(IDX) IS EQUAL TO 5 THEN
                 MOVE 50 TO WS-RESULT
               END-IF
               SET IDX UP BY 1
             END-PERFORM
            
         END-EVALUATE.
      