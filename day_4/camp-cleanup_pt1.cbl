       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMP-CLEANUP.

       ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
              FILE-CONTROL.
                  SELECT input-file ASSIGN TO "test.txt"
                      ORGANIZATION LINE SEQUENTIAL
                      FILE STATUS input-file-status.

              DATA DIVISION.
              FILE SECTION.
              FD  input-file.
              01  input-record         PIC X(256).

             WORKING-STORAGE SECTION.
               01  input-file-status   PIC 99.
                 88  file-is-ok                  VALUE 0.
                 88  end-of-file                 VALUE 10.

               01  line-count          PIC 9(06).
               01  strlen              PIC 9(02).
               01  split-row           PIC 9(02).

               01 ASSIGN_SEP           PIC 9(01).
               01 ROW_SEP.
                 03 ROWS_SEP-A PIC 9(01) OCCURS 4 TIMES.

               01 FIRST-RANGE.
                 03 FIRST-RANGE-A PIC 9(02) OCCURS 2 TIMES.

               01 SEC-RANGE.
                 03 SEC-RANGE-A PIC 9(02) OCCURS 2 TIMES.

               01 COUNT-RANGE          PIC 9(02).
               01 total-val            PIC 9(05).
               01 result-char          PIC A(01).
               01 char-conversion      PIC 9(05).
               01 OVERLAPS             PIC 9(04) VALUE 0.

               01  TEST-STRING         PIC X(30).
               01  SEARCH-STRING       PIC X(30).
               01  INDE                PIC 9(02) VALUE 0.
               01  OUT-DEX             PIC 9(02).
               01  INDEX-LOOKUP        PIC 9(01).
               01  MATCH-STATUS        PIC X(1)  VALUE 'N'.
                   88 NO-MATCH                   VALUE 'N'.
                   88 MATCH                      VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN.
       OPEN INPUT input-file
       IF NOT file-is-ok
           DISPLAY "The file could not be opened."
           GOBACK
       END-IF

       PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
           READ input-file
           COMPUTE INDE = 0
           COMPUTE ASSIGN_SEP = 1
           PERFORM UNTIL INDE >= LENGTH OF input-record
           OR input-record(INDE:1) = SPACES
               PERFORM FIND-SCOPE
               MOVE INDE TO ROWS_SEP-A(ASSIGN_SEP)
               COMPUTE ASSIGN_SEP = ASSIGN_SEP + 1
           END-PERFORM
           
           COMPUTE COUNT-RANGE = ROWS_SEP-A(1) - 1
           COMPUTE FIRST-RANGE-A(1)=
           FUNCTION NUMVAL(input-record(1:COUNT-RANGE))

           COMPUTE COUNT-RANGE = ROWS_SEP-A(2) - ROWS_SEP-A(1)
           COMPUTE FIRST-RANGE-A(2)=
           FUNCTION NUMVAL(input-record(ROWS_SEP-A(1):COUNT-RANGE))
           
           COMPUTE ROWS_SEP-A(2) = ROWS_SEP-A(2) + 1
           COMPUTE COUNT-RANGE = ROWS_SEP-A(3) - ROWS_SEP-A(2)
           COMPUTE SEC-RANGE-A(1)=
           FUNCTION NUMVAL(input-record(ROWS_SEP-A(2):COUNT-RANGE))

           COMPUTE COUNT-RANGE = ROWS_SEP-A(4) - ROWS_SEP-A(3)
           COMPUTE SEC-RANGE-A(2)=
           FUNCTION NUMVAL(input-record(ROWS_SEP-A(3):COUNT-RANGE))
           IF FIRST-RANGE-A(1) NOT = ZERO
               IF FIRST-RANGE-A(1) >= SEC-RANGE-A(1)
               AND FIRST-RANGE-A(2) <= SEC-RANGE-A(2)
                   DISPLAY line-count
                   DISPLAY FIRST-RANGE-A(1) " >= " SEC-RANGE-A(1)
                   DISPLAY FIRST-RANGE-A(2) " <= " SEC-RANGE-A(2)
                   COMPUTE OVERLAPS = OVERLAPS + 1
               ELSE

                   IF FIRST-RANGE-A(1) <= SEC-RANGE-A(1)
                   AND FIRST-RANGE-A(2) >= SEC-RANGE-A(2)
                       DISPLAY line-count
                       DISPLAY FIRST-RANGE-A(1) " <= " SEC-RANGE-A(1)
                       DISPLAY FIRST-RANGE-A(2) " >= " SEC-RANGE-A(2)
                       COMPUTE OVERLAPS = OVERLAPS + 1
                   END-IF
               END-IF
           END-IF
       END-PERFORM
       CLOSE input-file
       DISPLAY "AMOUNT OF OVERLAPS: "OVERLAPS
       STOP RUN.

       FIND-SCOPE.
       MOVE "N" TO MATCH-STATUS

       PERFORM UNTIL INDE >= LENGTH OF input-record OR MATCH
           COMPUTE INDE = INDE + 1
           IF input-record(INDE:1) = '-'
           OR input-record(INDE:1) = ','
           OR input-record(INDE:1) = SPACES
               SET MATCH TO TRUE
           END-IF
           
       END-PERFORM
       .
