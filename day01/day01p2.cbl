           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY01P2.

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

              01 WS-TOP-CALORIES.
                 03 WS-TOP-CALORIE   PIC 9(11) VALUE ZERO OCCURS 3.

              01 WS-SUM-CALORIES     PIC 9(11) VALUE ZERO.


           PROCEDURE DIVISION.
              OPEN INPUT CALORIES.
              PERFORM UNTIL WS-EOF
                 READ CALORIES
                 AT END
                    SET WS-EOF TO TRUE
                    PERFORM PLACE-IN-TOP3
                    COMPUTE WS-SUM-CALORIES = WS-TOP-CALORIE(1)
                                            + WS-TOP-CALORIE(2)
                                            + WS-TOP-CALORIE(3)
                    DISPLAY WS-SUM-CALORIES
                 NOT AT END
                    IF IN-CALORIE NOT = SPACES
                       MOVE IN-CALORIE TO WS-CALORIE
                       ADD WS-CALORIE TO WS-ELF-CALORIE(IX)
                    ELSE
                       PERFORM PLACE-IN-TOP3
                       ADD 1 TO IX
                    END-IF
                 END-READ
              END-PERFORM.
              CLOSE CALORIES.
              STOP RUN.

           PLACE-IN-TOP3 SECTION.

               IF WS-ELF-CALORIE(IX) > WS-TOP-CALORIE(1)
                  MOVE WS-TOP-CALORIE(2)  TO WS-TOP-CALORIE(3)
                  MOVE WS-TOP-CALORIE(1)  TO WS-TOP-CALORIE(2)
                  MOVE WS-ELF-CALORIE(IX) TO WS-TOP-CALORIE(1)
               ELSE
                  IF WS-ELF-CALORIE(IX) > WS-TOP-CALORIE(2)
                     MOVE WS-TOP-CALORIE(2)  TO WS-TOP-CALORIE(3)
                     MOVE WS-ELF-CALORIE(IX) TO WS-TOP-CALORIE(2)
                  ELSE 
                     IF WS-ELF-CALORIE(IX) > WS-TOP-CALORIE(3)
                        MOVE WS-ELF-CALORIE(IX) TO WS-TOP-CALORIE(3)
                     END-IF
                  END-IF
               END-IF
               .
