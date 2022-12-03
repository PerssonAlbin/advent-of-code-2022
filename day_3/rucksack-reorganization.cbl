       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUCKSACK-REORGANIZATION.

       ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
              FILE-CONTROL.
                  SELECT input-file ASSIGN TO "sample.txt"
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
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT input-file
           IF NOT file-is-ok
             DISPLAY "The file could not be opened."
             GOBACK
           END-IF

           PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
             READ input-file

             DISPLAY FUNCTION TRIM(input-record)
           END-PERFORM

           STOP RUN.
