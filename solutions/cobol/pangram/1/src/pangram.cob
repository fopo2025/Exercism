        IDENTIFICATION DIVISION.
        PROGRAM-ID. PANGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SENTENCE PIC X(60).
        01 WS-RESULT PIC 9.
        01 ARR.
          10 C PIC X OCCURS 26 TIMES INDEXED BY IDX.
        01 I PIC 99.
        01 K PIC 99.
        01 D PIC X.
        01 N PIC 999.
        PROCEDURE DIVISION.
        PANGRAM.
          SET IDX TO 1.
          PERFORM 60 TIMES
            MOVE SPACE TO C(IDX)
            SET IDX UP BY 1
          END-PERFORM.
      
          MOVE 0 TO K.
          MOVE 0 TO N.
          
          PERFORM VARYING I FROM 1 BY 1 UNTIL I IS GREATER THAN 60
            MOVE WS-SENTENCE(I:1) TO D
            IF D IS GREATER THAN OR EQUAL TO "A"
              AND D IS LESS THAN OR EQUAL TO "Z" THEN
                MOVE FUNCTION ORD(D) TO N
                SUBTRACT 64 FROM N
                IF C(N) IS NOT EQUAL TO "Y"
                  MOVE "Y" TO C(N)
                  ADD 1 TO K
                END-IF
            END-IF
            IF D IS GREATER THAN OR EQUAL TO "a"
              AND D IS LESS THAN OR EQUAL TO "z" THEN
              MOVE FUNCTION ORD(D) TO N
              SUBTRACT 96 FROM N
              IF C(N) IS NOT EQUAL TO "Y"
                MOVE "Y" TO C(N)
                ADD 1 TO K
              END-IF
            END-IF
          END-PERFORM.

          IF K IS GREATER THAN OR EQUAL TO 26 THEN
            MOVE 1 TO WS-RESULT
          ELSE
            MOVE 0 TO WS-RESULT
          END-IF.
            
        EXIT.