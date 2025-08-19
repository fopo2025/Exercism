       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISOGRAM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 WS-PHRASE PIC X(60).
         01 WS-RESULT PIC 99.
         01 WS-CHAR-ARR.
           10 WS-ARR-CHAR PIC X OCCURS 26 TIMES INDEXED BY WS-ARR-CHAR-IDX.
         01 WS-I PIC 99 COMP-5.
         01 WS-CHAR PIC X.
         01 WS-ORD PIC 999 COMP-5.

       PROCEDURE DIVISION.
         ISOGRAM.
         MOVE 1 TO WS-RESULT.
         SET WS-ARR-CHAR-IDX TO 26.

         PERFORM 26 TIMES
           MOVE "N" TO WS-ARR-CHAR(WS-ARR-CHAR-IDX)
           SET WS-ARR-CHAR-IDX DOWN BY 1
         END-PERFORM.
         
         PERFORM VARYING WS-I FROM 60 BY -1
         UNTIL WS-I IS LESS THAN 1
         OR WS-PHRASE(WS-I:1) IS NOT EQUAL TO SPACE
         END-PERFORM.

         PERFORM VARYING WS-I FROM WS-I BY -1
         UNTIL WS-I IS LESS THAN 1
         OR WS-RESULT IS LESS THAN 1
           MOVE WS-PHRASE(WS-I:1) TO WS-CHAR
           MOVE 26 TO WS-ORD

           IF WS-CHAR IS GREATER THAN OR EQUAL TO "a"
           AND WS-CHAR IS LESS THAN OR EQUAL TO "z"
             MOVE FUNCTION ORD(WS-CHAR) TO WS-ORD
             SUBTRACT FUNCTION ORD("a") FROM WS-ORD
           
           ELSE
             IF WS-CHAR IS GREATER THAN OR EQUAL TO "A"
             AND WS-CHAR IS LESS THAN OR EQUAL TO "Z"
               MOVE FUNCTION ORD(WS-CHAR) TO WS-ORD
               SUBTRACT FUNCTION ORD("A") FROM WS-ORD
             END-IF
           END-IF

             IF WS-ORD IS LESS THAN 26
               ADD 1 TO WS-ORD
      
               IF WS-ARR-CHAR(WS-ORD) IS EQUAL TO "Y" THEN
                 MOVE 0 TO WS-RESULT
               ELSE
                 MOVE "Y" TO WS-ARR-CHAR(WS-ORD)
               END-IF
             END-IF

         END-PERFORM.
      
         EXIT.
      