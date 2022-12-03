       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUCKSACK-REORGANIZATION.

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

               01 total-val            PIC 9(05).
               01 result-char          PIC A(01).
               01 char-conversion      PIC 9(05).

               01  TEST-STRING         PIC X(30).
               01  SEARCH-STRING       PIC X(30).
               01  IN-DEX-RESULT       PIC 9(02).
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

             MOVE FUNCTION LENGTH(FUNCTION TRIM(input-record)) TO strlen
             
             COMPUTE split-row = strlen / 2
             MOVE input-record(1:split-row) TO SEARCH-STRING
             COMPUTE split-row = split-row + 1
             MOVE input-record(split-row:split-row) TO TEST-STRING
             
             PERFORM FIND-MATCHES
             
             MOVE TEST-STRING(IN-DEX-RESULT:1) TO result-char
             COMPUTE char-conversion = FUNCTION ORD(result-char)
             
             PERFORM CONVERT-NUMBERS
             COMPUTE total-val = total-val + char-conversion
             
           END-PERFORM
           DISPLAY total-val

           CLOSE input-file

           STOP RUN.

       FIND-MATCHES.
       MOVE ZERO TO IN-DEX-RESULT
       MOVE "N" TO MATCH-STATUS
       PERFORM UNTIL IN-DEX-RESULT = LENGTH OF TEST-STRING OR MATCH
         COMPUTE IN-DEX-RESULT = IN-DEX-RESULT + 1
         COMPUTE OUT-DEX = 0
         PERFORM UNTIL OUT-DEX = LENGTH OF SEARCH-STRING OR MATCH
           COMPUTE OUT-DEX = OUT-DEX + 1
           IF TEST-STRING(IN-DEX-RESULT:1) = SEARCH-STRING(OUT-DEX:1)
               SET MATCH  TO TRUE
           END-IF
         END-PERFORM
       END-PERFORM
       .

       CONVERT-NUMBERS.
       IF char-conversion > 65
         IF char-conversion < 98
           COMPUTE char-conversion = char-conversion - 39
         END-IF
         IF char-conversion > 97
           COMPUTE char-conversion = char-conversion - 97
         END-IF
       ELSE
         COMPUTE char-conversion = 0
       END-IF
       .
