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
              01  converter         PIC 9(5).
              01  rekner            PIC 9(5).
              01  highest_num       PIC 9(5).
       PROCEDURE DIVISION.
           OPEN INPUT input-file
           IF NOT file-is-ok
             DISPLAY "The file could not be opened."
             GOBACK
           END-IF

    
           PERFORM VARYING line-count FROM 1 BY 1 UNTIL end-of-file
             READ input-file

             COMPUTE rekner = rekner + FUNCTION NUMVAL(input-record)
             IF input-record = SPACES
               COMPUTE rekner = 0
             END-IF
      
             IF rekner > highest_num 
               COMPUTE highest_num = rekner
             END-IF
      
           END-PERFORM
           DISPLAY "highest_num="highest_num
           CLOSE input-file

           STOP RUN.
