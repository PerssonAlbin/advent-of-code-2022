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

               01 stored-rows          PIC 9(03) VALUE 1.
               01 ROWS.
                 03 ROWS-A PIC x(99) OCCURS 3 TIMES.
               01 result               PIC X(256).
               01 result-1             PIC X(256).
               01 result-2             PIC X(256).
                 

               01  TEST-STRING         PIC X(99).
               01  SEARCH-STRING       PIC X(99).
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
             
             MOVE FUNCTION TRIM(input-record) TO ROWS-A(stored-rows)

             IF stored-rows = 3
               
               MOVE ROWS-A(1) TO TEST-STRING
               MOVE ROWS-A(2) TO SEARCH-STRING
               PERFORM FIND-MATCHES
               MOVE result TO result-1
               MOVE SPACES TO result

               MOVE ROWS-A(2) TO TEST-STRING
               MOVE ROWS-A(3) TO SEARCH-STRING
               PERFORM FIND-MATCHES
               MOVE result TO result-2
               MOVE SPACES TO result

               MOVE result-1 TO TEST-STRING
               MOVE result-2 TO SEARCH-STRING
               PERFORM FIND-MATCHES
               
               COMPUTE char-conversion = FUNCTION ORD(result(1:1))
               MOVE SPACES TO result
             
               PERFORM CONVERT-NUMBERS
               COMPUTE total-val = total-val + char-conversion
               
               COMPUTE stored-rows = 0
             END-IF
             COMPUTE stored-rows = stored-rows + 1
           END-PERFORM
           DISPLAY total-val

           CLOSE input-file

           STOP RUN.

       FIND-MATCHES.
       COMPUTE IN-DEX-RESULT = 1
       PERFORM UNTIL IN-DEX-RESULT = LENGTH OF TEST-STRING
         COMPUTE OUT-DEX = 1
         PERFORM UNTIL OUT-DEX = LENGTH OF SEARCH-STRING
           IF TEST-STRING(IN-DEX-RESULT:1) = SEARCH-STRING(OUT-DEX:1)
             IF TEST-STRING(IN-DEX-RESULT:1) NOT = SPACES
               STRING FUNCTION CONCATENATE(
                FUNCTION TRIM(result);
                FUNCTION TRIM(TEST-STRING(IN-DEX-RESULT:1)))
               INTO result
             END-IF
           END-IF
           COMPUTE OUT-DEX = OUT-DEX + 1
         END-PERFORM
         COMPUTE IN-DEX-RESULT = IN-DEX-RESULT + 1
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
