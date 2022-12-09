           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY02.

           DATA DIVISION.
           
           WORKING-STORAGE SECTION.

           01 IN-ROUND.
              03 IN-OPPONENT-CHOICE   PIC X(1).
                 88 OP-CHOSE-ROCK           VALUE 'A'.
                 88 OP-CHOSE-PAPER          VALUE 'B'.
                 88 OP-CHOSE-SCISSORS       VALUE 'C'.
              03 FILLER             PIC X(1).
              03 IN-PLAYER-CHOICE   PIC X(1).
                 88 PL-CHOSE-ROCK           VALUE 'X'.
                 88 PL-CHOSE-PAPER          VALUE 'Y'.
                 88 PL-CHOSE-SCISSORS       VALUE 'Z'.
           
           01 WS-OUTCOME REDEFINES IN-ROUND PIC X(3).
              88 WS-ROUND-WON               VALUE 'A Y'
                                                  'B Z'
                                                  'C X'.
              88 WS-ROUND-DRAW              VALUE 'A X'
                                                  'B Y'
                                                  'C Z'.
              88 WS-ROUND-LOST              VALUE 'A Z'
                                                  'B X'
                                                  'C Y'.

           01 PL-POINTS                     PIC 9(10) VALUE 0.

           PROCEDURE DIVISION.

           MOVE SPACE TO IN-ROUND
           ACCEPT IN-ROUND
           PERFORM UNTIL IN-ROUND = SPACE 
               EVALUATE TRUE
                  WHEN PL-CHOSE-ROCK
                     ADD 1 TO PL-POINTS
                  WHEN PL-CHOSE-PAPER
                     ADD 2 TO PL-POINTS
                  WHEN PL-CHOSE-SCISSORS
                     ADD 3 TO PL-POINTS
               END-EVALUATE

               EVALUATE TRUE
                   WHEN WS-ROUND-WON
                     ADD 6 TO PL-POINTS
                   WHEN WS-ROUND-DRAW
                     ADD 3 TO PL-POINTS
                   WHEN WS-ROUND-LOST
                     CONTINUE
               END-EVALUATE
               MOVE SPACE TO IN-ROUND
               ACCEPT IN-ROUND
           END-PERFORM

           DISPLAY PL-POINTS
           STOP RUN.
