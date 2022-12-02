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
              01  input-file-status PIC 99.
                  88  file-is-ok    VALUE 0.
                  88  end-of-file   VALUE 10.

              01  line-count        PIC 9(6).
              01  first-char        PIC A(1).
              01  second-char       PIC A(1).
              01  total-sum         PIC 9(6).
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT input-file
           IF NOT file-is-ok
             DISPLAY "The file could not be opened."
             GOBACK
           END-IF

    
           PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
             READ input-file

             MOVE FUNCTION TRIM(input-record(1:1)) TO first-char
             MOVE FUNCTION TRIM(input-record(3:1)) TO second-char

               IF first-char = "A"
                   IF second-char = "X"
                   COMPUTE total-sum = total-sum + 3
                   END-IF

                   IF second-char = "Y"
                       COMPUTE total-sum = total-sum + 6
                   END-IF
               END-IF

               IF first-char = "B"
                   IF second-char = "Z"
                   COMPUTE total-sum = total-sum + 6
                   END-IF

                   IF second-char = "Y"
                       COMPUTE total-sum = total-sum + 3
                   END-IF
               END-IF
               IF first-char = "C"
                   IF second-char = "X"
                   COMPUTE total-sum = total-sum + 6
                   END-IF

                   IF second-char = "Z"
                       COMPUTE total-sum = total-sum + 3
                   END-IF
               END-IF

           

               IF second-char = "X"
                   COMPUTE total-sum = total-sum + 1
               END-IF

               IF second-char = "Y"
                   COMPUTE total-sum = total-sum + 2
               END-IF
               IF second-char = "Z"
                   COMPUTE total-sum = total-sum + 3
               END-IF
           END-PERFORM
           DISPLAY "Total points: "total-sum
           CLOSE input-file

           STOP RUN.
