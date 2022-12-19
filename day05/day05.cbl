           IDENTIFICATION DIVISION.
           PROGRAM-ID. DAY05.

           ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
              FILE-CONTROL.
              SELECT FILE-INPUT ASSIGN TO 'input_long.txt'
              ORGANIZATION IS LINE SEQUENTIAL.       

           DATA DIVISION.
           FILE SECTION.
              FD FILE-INPUT.
              01 IN-INPUT                     PIC X(80).

           WORKING-STORAGE SECTION.

              01 WS-EOF-YN                    PIC X(1).
                 88 WS-EOF                             VALUE 'Y'.
                 88 WS-NOT-EOF                         VALUE 'N'.

              01 WS-BUFFER.
                 03 WS-BUFFER-LINE OCCURS 10.
                    05 WS-BUFFER-STACK OCCURS 9.
                       07 WS-BUFFER-ELEMENT.
                          09 FILLER           PIC X.
                          09 WS-BUFFER-LETTER PIC X.
                          09 FILLER           PIC XX.

               01 WS-BUFFER-SIZE              PIC 99   VALUE 0.
               01 WS-NUM-STACKS               PIC 99   VALUE 0.

               01 WS-BUFFERED-SW              PIC X    VALUE 'N'.
                  88 WS-BUFFERED                       VALUE 'Y'.
                  88 WS-NOT-BUFFERED                   VALUE 'N'.

               01 WS-STACKS-TABLE.
                  03 WS-STACK OCCURS 9.
                     05 WS-STACK-SIZE         PIC 99   VALUE 0.
                     05 WS-STACK-ITEM         PIC X VALUE ' ' OCCURS 99.

               01 IX                          PIC 99 COMP.
               01 IX2                         PIC 99 COMP.
               01 IX3                         PIC 99 COMP.

               01 WS-TIMES                    PIC 99 COMP.
               01 WS-FROM                     PIC 9  COMP.
               01 WS-TO                       PIC 9  COMP.

               01 WS-OUTPUT                   PIC X(10) VALUE SPACE.

           PROCEDURE DIVISION.
               OPEN INPUT FILE-INPUT
                   PERFORM UNTIL WS-EOF
                       READ FILE-INPUT
                       AT END
                          SET WS-EOF TO TRUE
                          PERFORM PRINT-MESSAGE
                       NOT AT END
                          IF WS-NOT-BUFFERED
                             IF IN-INPUT = SPACE
                                SET WS-BUFFERED TO TRUE
                             ELSE
                                ADD 1 TO WS-BUFFER-SIZE
                                MOVE IN-INPUT
                                  TO WS-BUFFER-LINE(WS-BUFFER-SIZE)
                             END-IF
                          ELSE
                             IF WS-NUM-STACKS = 0
                                PERFORM BUILD-STACK-FROM-BUFFER
                             END-IF
                             PERFORM INTERPRET-COMMANDS
                          END-IF
                   END-PERFORM
               CLOSE FILE-INPUT
               STOP RUN
               .

           BUILD-STACK-FROM-BUFFER SECTION.
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
                   IF WS-BUFFER-LETTER(WS-BUFFER-SIZE,IX) IS NUMERIC
                       ADD 1 TO WS-NUM-STACKS
                   ELSE
                       MOVE 10 TO IX
                   END-IF
               END-PERFORM

               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-NUM-STACKS
                   PERFORM VARYING IX2 FROM WS-BUFFER-SIZE
                                       BY -1 UNTIL IX2 < 1
                       IF WS-BUFFER-LETTER(IX2,IX) = SPACE
                          CONTINUE
                       ELSE
                          IF WS-BUFFER-LETTER(IX2,IX) IS NUMERIC
                             CONTINUE
                          ELSE
                             ADD 1 TO WS-STACK-SIZE(IX)
                             MOVE WS-STACK-SIZE(IX) TO IX3
                             MOVE WS-BUFFER-LETTER(IX2,IX)
                                  TO WS-STACK-ITEM(IX,IX3)
                          END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               .

           INTERPRET-COMMANDS SECTION.
               UNSTRING IN-INPUT(5:) DELIMITED BY 'from' OR 'to'
                   INTO WS-TIMES, WS-FROM, WS-TO
               END-UNSTRING
               PERFORM WS-TIMES TIMES
                  MOVE WS-STACK-SIZE(WS-FROM) TO IX
                  MOVE WS-STACK-SIZE(WS-TO)   TO IX2

                  ADD 1 TO IX2
                  ADD 1 TO WS-STACK-SIZE(WS-TO)

                  MOVE WS-STACK-ITEM(WS-FROM,IX)
                    TO WS-STACK-ITEM(WS-TO  ,IX2)

                  MOVE SPACE TO WS-STACK-ITEM(WS-FROM,IX)

                  SUBTRACT 1 FROM WS-STACK-SIZE(WS-FROM)
               END-PERFORM
               .

           PRINT-MESSAGE SECTION.
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-NUM-STACKS
                  MOVE WS-STACK-SIZE(IX) TO IX2
                  MOVE WS-STACK-ITEM(IX,IX2) TO WS-OUTPUT(IX:1)
               END-PERFORM
               DISPLAY WS-OUTPUT
               .
