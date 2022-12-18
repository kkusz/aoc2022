           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY04.

           DATA DIVISION.

           WORKING-STORAGE SECTION.
               01 WS-INPUT          PIC X(80).

               01 WS-PAIR1A         PIC 99.
               01 WS-PAIR1B         PIC 99.
               01 WS-PAIR2A         PIC 99.
               01 WS-PAIR2B         PIC 99.

               01 WS-COUNT          PIC 9(5) COMP VALUE 0.
               01 WS-COUNT-OVERLAP  PIC 9(5) COMP VALUE 0.


           PROCEDURE DIVISION.
               MOVE SPACES TO WS-INPUT
               ACCEPT WS-INPUT
               PERFORM UNTIL WS-INPUT = SPACES
                   UNSTRING WS-INPUT DELIMITED BY ',' OR '-'
                      INTO WS-PAIR1A,
                           WS-PAIR1B,
                           WS-PAIR2A,
                           WS-PAIR2B
                   END-UNSTRING

                   IF  WS-PAIR1A <= WS-PAIR2A
                   AND WS-PAIR1B >= WS-PAIR2B
                       ADD 1 TO WS-COUNT
                   ELSE
                       IF  WS-PAIR2A <= WS-PAIR1A
                       AND WS-PAIR2B >= WS-PAIR1B
                           ADD 1 TO WS-COUNT
                       END-IF
                   END-IF

                   IF  WS-PAIR1B >= WS-PAIR2A
                   AND WS-PAIR1A <= WS-PAIR2B
                   OR  WS-PAIR2B >= WS-PAIR1A
                   AND WS-PAIR2A <= WS-PAIR1B
                       ADD 1 TO WS-COUNT-OVERLAP
                   END-IF

                   MOVE SPACES TO WS-INPUT
                   ACCEPT WS-INPUT
               END-PERFORM

               DISPLAY WS-COUNT
               DISPLAY WS-COUNT-OVERLAP
       
               STOP RUN.
