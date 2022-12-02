       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALORIE-COUNTER.

       ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
              FILE-CONTROL.
                  SELECT input-file ASSIGN TO "test.txt"
                      ORGANIZATION LINE SEQUENTIAL
                      FILE STATUS input-file-status.

              DATA DIVISION.
              FILE SECTION.
              FD  input-file.
              01  input-record PIC X(256).

              WORKING-STORAGE SECTION.
               01  input-file-status   PIC 99.
                   88  file-is-ok      VALUE 0.
                   88  end-of-file     VALUE 10.

               01  line-count          PIC 9(6).
               01  first-char          PIC A(1).
               01  second-char         PIC A(1).
               01  second-decrypt      PIC A(1).
               01  total-sum           PIC 9(6).
               01  rotation            PIC A(3) VALUE 'XYZ'.

               01  TEST-STRING         PIC X(30) VALUE 'ABC'.
               01  SEARCH-STRING       PIC X(05).
               01  INDEX-RESULT        PIC 9(01).
               01  INDEX-LOOKUP        PIC 9(01).
               01  MATCH-STATUS        PIC X(1) VALUE 'N'.
                   88 NO-MATCH                  VALUE 'N'.
                   88 MATCH                     VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT input-file
           IF NOT file-is-ok
             DISPLAY "The file could not be opened."
             GOBACK
           END-IF

    
           PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
             READ input-file
             MOVE SPACES TO second-decrypt

             MOVE FUNCTION TRIM(input-record(1:1)) TO first-char
             MOVE FUNCTION TRIM(input-record(3:1)) TO second-char

               IF second-char = "Z"
                   MOVE first-char TO SEARCH-STRING
                   PERFORM FIND-MATCHES
                   COMPUTE INDEX-LOOKUP = INDEX-RESULT + 1
                   IF INDEX-LOOKUP > 3
                       COMPUTE INDEX-LOOKUP = 1
                   END-IF
                   MOVE rotation(INDEX-LOOKUP:1) TO second-decrypt
               END-IF

               IF second-char = "Y"
                   MOVE first-char TO SEARCH-STRING
                   PERFORM FIND-MATCHES
                   COMPUTE INDEX-LOOKUP = INDEX-RESULT
                   MOVE rotation(INDEX-LOOKUP:1) TO second-decrypt
               END-IF

               IF second-char = "X"
                   MOVE first-char TO SEARCH-STRING
                   PERFORM FIND-MATCHES
                   COMPUTE INDEX-LOOKUP = INDEX-RESULT - 1
                   IF INDEX-LOOKUP < 1
                       COMPUTE INDEX-LOOKUP = 3
                   END-IF
                   MOVE rotation(INDEX-LOOKUP:1) TO second-decrypt
               END-IF

               IF first-char = "A"
                   IF second-decrypt = "X"
                   COMPUTE total-sum = total-sum + 3
                   END-IF

                   IF second-decrypt = "Y"
                       COMPUTE total-sum = total-sum + 6
                   END-IF
               END-IF

               IF first-char = "B"
                   IF second-decrypt = "Z"
                   COMPUTE total-sum = total-sum + 6
                   END-IF

                   IF second-decrypt = "Y"
                       COMPUTE total-sum = total-sum + 3
                   END-IF
               END-IF
               IF first-char = "C"
                   IF second-decrypt = "X"
                   COMPUTE total-sum = total-sum + 6
                   END-IF

                   IF second-decrypt = "Z"
                       COMPUTE total-sum = total-sum + 3
                   END-IF
               END-IF

           

               IF second-decrypt = "X"
                   COMPUTE total-sum = total-sum + 1
               END-IF

               IF second-decrypt = "Y"
                   COMPUTE total-sum = total-sum + 2
               END-IF
               IF second-decrypt = "Z"
                   COMPUTE total-sum = total-sum + 3
               END-IF

           END-PERFORM
           DISPLAY "Total points: "total-sum
           CLOSE input-file

           STOP RUN.


       FIND-MATCHES.
           MOVE ZERO TO INDEX-RESULT
           MOVE "N" TO MATCH-STATUS
           PERFORM UNTIL INDEX-RESULT = LENGTH OF TEST-STRING OR MATCH
               COMPUTE INDEX-RESULT = INDEX-RESULT + 1
               IF TEST-STRING(INDEX-RESULT:1) = SEARCH-STRING
                   SET MATCH  TO TRUE
               END-IF
           END-PERFORM
           .
