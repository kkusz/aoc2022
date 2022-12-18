           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY03P2.

           DATA DIVISION.

           WORKING-STORAGE SECTION.

           01 WS-INPUT             PIC X(80).
           01 WS-LEN               PIC 9(5) COMP.
           01 WS-HALF-LEN          PIC 9(5) COMP.

           01 WS-CHAR              PIC X.

           01 WS-PRIORITY          PIC 9(2).

           01 WS-RUCKSACK-TABLE.
              03 FILLER OCCURS 52.
                 05 FILLER OCCURS 3.
                   07 WS-RUCKSACK-ITEM  PIC 9(2).
           
           01 IX                   PIC 9(3) COMP.

           01 WS-SUM               PIC 9(5) COMP VALUE 0.

           01 WS-LINE              PIC 9 VALUE 1.

           PROCEDURE DIVISION.
               MOVE SPACES TO WS-INPUT
               ACCEPT WS-INPUT
               PERFORM UNTIL WS-INPUT = SPACES
                  MOVE ZERO TO WS-LEN
                  INSPECT FUNCTION REVERSE(WS-INPUT)
                     TALLYING WS-LEN FOR LEADING SPACES
                  COMPUTE WS-LEN = LENGTH OF WS-INPUT - WS-LEN
                  PERFORM FILL-RUCKSACK
                  MOVE SPACES TO WS-INPUT
                  ACCEPT WS-INPUT
               END-PERFORM
               DISPLAY WS-SUM
               STOP RUN
               .

           FILL-RUCKSACK SECTION.
               IF WS-LINE = 1
                  INITIALIZE WS-RUCKSACK-TABLE
               END-IF

               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-LEN
                   MOVE WS-INPUT(IX:1) TO WS-CHAR
                   PERFORM GET-PRIORITY
                   ADD 1 TO WS-RUCKSACK-ITEM(WS-PRIORITY,WS-LINE)
               END-PERFORM
               IF WS-LINE = 3
                   PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 52
                       IF  WS-RUCKSACK-ITEM(IX,1) > 0
                       AND WS-RUCKSACK-ITEM(IX,2) > 0
                       AND WS-RUCKSACK-ITEM(IX,3) > 0
                          ADD IX  TO WS-SUM
                          MOVE 99 TO IX
                       END-IF
                   END-PERFORM
                   MOVE 1 TO WS-LINE
               ELSE
                   ADD 1  TO WS-LINE
               END-IF
               .

           GET-PRIORITY SECTION.
               EVALUATE WS-CHAR
                   WHEN 'a'  MOVE 1  TO WS-PRIORITY
                   WHEN 'b'  MOVE 2  TO WS-PRIORITY
                   WHEN 'c'  MOVE 3  TO WS-PRIORITY
                   WHEN 'd'  MOVE 4  TO WS-PRIORITY
                   WHEN 'e'  MOVE 5  TO WS-PRIORITY
                   WHEN 'f'  MOVE 6  TO WS-PRIORITY
                   WHEN 'g'  MOVE 7  TO WS-PRIORITY
                   WHEN 'h'  MOVE 8  TO WS-PRIORITY
                   WHEN 'i'  MOVE 9  TO WS-PRIORITY
                   WHEN 'j'  MOVE 10 TO WS-PRIORITY
                   WHEN 'k'  MOVE 11 TO WS-PRIORITY
                   WHEN 'l'  MOVE 12 TO WS-PRIORITY
                   WHEN 'm'  MOVE 13 TO WS-PRIORITY
                   WHEN 'n'  MOVE 14 TO WS-PRIORITY
                   WHEN 'o'  MOVE 15 TO WS-PRIORITY
                   WHEN 'p'  MOVE 16 TO WS-PRIORITY
                   WHEN 'q'  MOVE 17 TO WS-PRIORITY
                   WHEN 'r'  MOVE 18 TO WS-PRIORITY
                   WHEN 's'  MOVE 19 TO WS-PRIORITY
                   WHEN 't'  MOVE 20 TO WS-PRIORITY
                   WHEN 'u'  MOVE 21 TO WS-PRIORITY
                   WHEN 'v'  MOVE 22 TO WS-PRIORITY
                   WHEN 'w'  MOVE 23 TO WS-PRIORITY
                   WHEN 'x'  MOVE 24 TO WS-PRIORITY
                   WHEN 'y'  MOVE 25 TO WS-PRIORITY
                   WHEN 'z'  MOVE 26 TO WS-PRIORITY
                   WHEN 'A'  MOVE 27 TO WS-PRIORITY
                   WHEN 'B'  MOVE 28 TO WS-PRIORITY
                   WHEN 'C'  MOVE 29 TO WS-PRIORITY
                   WHEN 'D'  MOVE 30 TO WS-PRIORITY
                   WHEN 'E'  MOVE 31 TO WS-PRIORITY
                   WHEN 'F'  MOVE 32 TO WS-PRIORITY
                   WHEN 'G'  MOVE 33 TO WS-PRIORITY
                   WHEN 'H'  MOVE 34 TO WS-PRIORITY
                   WHEN 'I'  MOVE 35 TO WS-PRIORITY
                   WHEN 'J'  MOVE 36 TO WS-PRIORITY
                   WHEN 'K'  MOVE 37 TO WS-PRIORITY
                   WHEN 'L'  MOVE 38 TO WS-PRIORITY
                   WHEN 'M'  MOVE 39 TO WS-PRIORITY
                   WHEN 'N'  MOVE 40 TO WS-PRIORITY
                   WHEN 'O'  MOVE 41 TO WS-PRIORITY
                   WHEN 'P'  MOVE 42 TO WS-PRIORITY
                   WHEN 'Q'  MOVE 43 TO WS-PRIORITY
                   WHEN 'R'  MOVE 44 TO WS-PRIORITY
                   WHEN 'S'  MOVE 45 TO WS-PRIORITY
                   WHEN 'T'  MOVE 46 TO WS-PRIORITY
                   WHEN 'U'  MOVE 47 TO WS-PRIORITY
                   WHEN 'V'  MOVE 48 TO WS-PRIORITY
                   WHEN 'W'  MOVE 49 TO WS-PRIORITY
                   WHEN 'X'  MOVE 50 TO WS-PRIORITY
                   WHEN 'Y'  MOVE 51 TO WS-PRIORITY
                   WHEN 'Z'  MOVE 52 TO WS-PRIORITY
               END-EVALUATE
               .
