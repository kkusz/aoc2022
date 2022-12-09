           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY02P2.

           DATA DIVISION.
           
           WORKING-STORAGE SECTION.

           01 IN-ROUND.
              03 IN-OPPONENT-CHOICE   PIC X(1).
                 88 OP-CHOSE-ROCK           VALUE 'A'.
                 88 OP-CHOSE-PAPER          VALUE 'B'.
                 88 OP-CHOSE-SCISSORS       VALUE 'C'.
              03 FILLER             PIC X(1).
              03 IN-PLAYER-OUTCOME  PIC X(1).
                 88 PL-SHOULD-LOSE          VALUE 'X'.
                 88 PL-SHOULD-DRAW          VALUE 'Y'.
                 88 PL-SHOULD-WIN           VALUE 'Z'.
           
           01 WS-OUTCOME REDEFINES IN-ROUND PIC X(3).
              88 WS-USE-ROCK                VALUE 'A Y'
                                                  'B X'
                                                  'C Z'.
              88 WS-USE-PAPER               VALUE 'A Z'
                                                  'B Y'
                                                  'C X'.
              88 WS-USE-SCISSORS            VALUE 'A X'
                                                  'B Z'
                                                  'C Y'.

           01 PL-POINTS                     PIC 9(10) VALUE 0.

           PROCEDURE DIVISION.

           MOVE SPACE TO IN-ROUND
           ACCEPT IN-ROUND
           PERFORM UNTIL IN-ROUND = SPACE 
               EVALUATE TRUE
                  WHEN WS-USE-ROCK
                     ADD 1 TO PL-POINTS
                  WHEN WS-USE-PAPER
                     ADD 2 TO PL-POINTS
                  WHEN WS-USE-SCISSORS
                     ADD 3 TO PL-POINTS
               END-EVALUATE

               EVALUATE TRUE
                   WHEN PL-SHOULD-WIN
                     ADD 6 TO PL-POINTS
                   WHEN PL-SHOULD-DRAW
                     ADD 3 TO PL-POINTS
                   WHEN PL-SHOULD-LOSE
                     CONTINUE
               END-EVALUATE
               MOVE SPACE TO IN-ROUND
               ACCEPT IN-ROUND
           END-PERFORM

           DISPLAY PL-POINTS
           STOP RUN.
