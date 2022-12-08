           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY01.

           ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
              FILE-CONTROL.
              SELECT CALORIES ASSIGN TO 'input_long.txt'
              ORGANIZATION IS LINE SEQUENTIAL.       

           DATA DIVISION.
              FILE SECTION.
              FD CALORIES.
              01 IN-CALORIE    PIC X(11).

              WORKING-STORAGE SECTION.
              01 WS-CALORIE    PIC 9(11).
              01 WS-EOF-YN     PIC X(1).
                 88 WS-EOF                  VALUE 'Y'.
                 88 WS-NOT-EOF              VALUE 'N'.

              01 IX                  PIC 9(3)  VALUE 1.
              01 WS-ELF-COUNT        PIC 9(3).
              01 WS-ELVES-TABLE.
                 03 WS-ELF-CALORIE   PIC 9(11) VALUE ZERO OCCURS 300.
              01 WS-MAX-CALORIE      PIC 9(11) VALUE ZERO.


           PROCEDURE DIVISION.

              OPEN INPUT CALORIES.
              PERFORM UNTIL WS-EOF
                 READ CALORIES
                 AT END
                    SET WS-EOF TO TRUE
                    IF WS-ELF-CALORIE(IX) > WS-MAX-CALORIE
                       MOVE WS-ELF-CALORIE(IX) TO WS-MAX-CALORIE
                    END-IF
                    DISPLAY WS-MAX-CALORIE
                 NOT AT END
                    IF IN-CALORIE NOT = SPACES
                       MOVE IN-CALORIE TO WS-CALORIE
                       ADD WS-CALORIE TO WS-ELF-CALORIE(IX)
                    ELSE
                       IF WS-ELF-CALORIE(IX) > WS-MAX-CALORIE
                          MOVE WS-ELF-CALORIE(IX) TO WS-MAX-CALORIE
                       END-IF
                       ADD 1 TO IX
                    END-IF
                 END-READ
              END-PERFORM.
              CLOSE CALORIES.
              STOP RUN.
