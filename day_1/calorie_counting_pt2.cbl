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
              01  temp              PIC 9(5).
              01  top_1             PIC 9(5).
              01  top_2             PIC 9(5).
              01  top_3             PIC 9(5).
              01  summa               PIC 9(6).
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
               IF rekner > top_1
                 COMPUTE temp = top_1
                 COMPUTE top_1 = rekner
                 COMPUTE rekner = temp
               END-IF

               IF rekner > top_2
                 COMPUTE temp = top_2
                 COMPUTE top_2 = rekner
                 COMPUTE rekner = temp
               END-IF
        
               IF rekner > top_3
                 COMPUTE temp = top_3
                 COMPUTE top_3 = rekner
                 COMPUTE rekner = temp
               END-IF
               COMPUTE temp = 0
               COMPUTE rekner = 0
             END-IF
      
           END-PERFORM
           DISPLAY "top 1="top_1
           DISPLAY "top 2="top_2
           DISPLAY "top 3="top_3
           COMPUTE summa = top_1 + top_2
           COMPUTE summa = summa + top_3
           DISPLAY "Total sum: "summa
           CLOSE input-file

           STOP RUN.
