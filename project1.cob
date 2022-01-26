      ******************************************************************
      *Author: David Nguyen
      *Due Date: January 30, 2022
      *Purpose: project1
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. project1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'EMPL.PROG1'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'EMPL.PROG1.OUTPUT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-RECORD PIC X(132).
       FD  OUTPUT-FILE
           LABEL RECORDS ARE OMITTED.
       01  OUTPUT-RECORD PIC X(132).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-EMP-ID PIC X(7).
           03 I-LNAME PIC X(15).
           03 I-FNAME PIC X(15).
           03 I-EMP-TYPE PIC X(2).
           03 I-TITLE PIC X(17).
           03 I-SSN PIC X(9).
           03 FILLER PIC X(24).
           03 I-DATE PIC X(8).
           03 FILLER PIC X(2).
           03 I-EMP-RATE.
               05 I-EMP-RATE-WHOLE PIC 9(4).
               05 I-EMP-RATE-DECIMAL PIC P9(2).
           03 I-EMP-STATUS PIC X(1).
           03 I-DEDUCT OCCURS 5 TIMES.
               05 I-DEDUCT-WHOLE PIC 9(3).
               05 I-DEDUCT-DECIMAL PIC P9(2).
       01  OUTPUT-HEADING1.
           03 FILLER PIC X(1) VALUE SPACES.
           03 H1-CURR-DATE PIC 99/99/99.
           03 FILLER PIC X(40) VALUE SPACES.
           03 FILLER PIC X(21) VALUE 'MASTERMIND COBOL, INC'.
           03 FILLER PIC X(36) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'PAGE'.
           03 H1-PAGE-NUM PIC ZZ9 VALUE 1.
       01  OUTPUT-HEADING2.
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'EMP ID'.
           03 FILLER PIC X(6) VALUE SPACES.
           03 FILLER PIC X(3) VALUE 'SSN'.
           03 FILLER PIC X(12) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'NAME'.
           03 FILLER PIC X(34) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'TYPE'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'TITLE'.
           03 FILLER PIC X(18) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'DATE'.
           03 FILLER PIC X(31) VALUE SPACES.
       01  OUTPUT-HEADING3.
           03 FILLER PIC X(34) VALUE SPACES.
           03 FILLER PIC X(4) VALUE 'LAST'.
           03 FILLER PIC X(13) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'FIRST'.
           03 FILLER PIC X(76) VALUE SPACES.
       01  OUTPUT-DATA1.
           03 FILLER PIC X(3) VALUE SPACES.
           03 D1-EMP-ID PIC X(8).
           03 FILLER PIC X(4) VALUE SPACES.
           03 D1-SSN PIC XXXBXXBXXXX.
           03 FILLER PIC X(4) VALUE SPACES.
           03 D1-LNAME PIC X(14).
           03 FILLER PIC X(5) VALUE SPACES.
           03 D1-FNAME PIC X(13).
           03 FILLER PIC X(7) VALUE SPACES.
           03 D1-EMP-TYPE PIC X(2).
           03 FILLER PIC X(4) VALUE SPACES.
           03 D1-TITLE PIC X(17) VALUE SPACES.
           03 FILLER PIC X(5) VALUE SPACES.
           03 D1-DATE PIC 99/99/9999.
           03 FILLER PIC X(25) VALUE SPACES.
       01  OUTPUT-DATA2.
           03 FILLER PIC X(69) VALUE SPACES.
           03 FILLER PIC X(7) VALUE 'DEDUCT:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 D2-DEDUCT1 PIC ZZ9.99.
           03 FILLER PIC X(5) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'RATE:'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 D2-EMP-RATE PIC Z,ZZ9.99.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'STATUS'.
           03 FILLER PIC X(2) VALUE SPACES.
           03 D2-EMP-STATUS PIC X(1).
           03 FILLER PIC X(12) VALUE SPACES.
       01  OUTPUT-DATA3.
           03 FILLER PIC X(81) VALUE SPACES.
           03 D3-DEDUCTOTHERS PIC ZZ9.99.
           03 FILLER PIC X(45) VALUE SPACES.
       01  OUTPUT-DATA4.
           03 FILLER PIC X(69) VALUE SPACES.
           03 FILLER PIC X(6) VALUE 'TOTAL:'.
           03 FILLER PIC X(3) VALUE SPACES.
           03 D4-DEDUCT-TOTAL PIC $$,$$9.99.
           03 FILLER PIC X(45) VALUE SPACES.
       01  OUTPUT-FOOTER1.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(32) VALUE 'NUMBER OF EMPLOYEE RECORDS READ:'.
           03 FILLER PIC X(13) VALUE SPACES.
           03 F1-EMP-COUNTER PIC ZZZ9.
           03 FILLER PIC X(82) VALUE SPACES.
       01  OUTPUT-FOOTER2.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(27) VALUE 'NUMBER OF HOURLY EMPLOYEES:'.
           03 FILLER PIC X(18) VALUE SPACES.
           03 F2-EMP-H-COUNT PIC ZZZ9.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(20) VALUE 'AVERAGE HOURLY RATE:'.
           03 FILLER PIC X(9) VALUE SPACES.
           03 F2-AVG-H-RATE PIC $$$9.99.
           03 FILLER PIC X(10) VALUE SPACES.
           03 FILLER PIC X(17) VALUE 'TOTAL AVG DEDUCT:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 F2-TOTAL-AVG-DEDUCT PIC $$,$$9.99.
           03 FILLER PIC X(1) VALUE SPACES.
       01  OUTPUT-FOOTER3.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(29) VALUE 'NUMBER OF SALARIED EMPLOYEES:'.
           03 FILLER PIC X(16) VALUE SPACES.
           03 F3-EMP-S-COUNT PIC ZZZ9.
           03 FILLER PIC X(4) VALUE SPACES.
           03 FILLER PIC X(22) VALUE 'AVERAGE SALARIED RATE:'.
           03 FILLER PIC X(5) VALUE SPACES.
           03 F3-AVG-S-RATE PIC $$,$$$.99.
           03 FILLER PIC X(10) VALUES SPACES.
           03 FILLER PIC X(13) VALUE 'TOTAL DEDUCT:'.
           03 FILLER PIC X(8) VALUE SPACES.
           03 F3-TOTAL-DEDUCT PIC $$$,$$9.99.
           03 FILLER PIC X(1) VALUE SPACES.
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
           03 PAGE-NUM PIC 999 VALUE 1.
           03 RECORD-PAGE-COUNTER PIC 99 VALUE 0.
           03 EMP-COUNTER PIC 9(4).
           03 EMP-H-COUNT PIC 9(4).
           03 EMP-S-COUNT PIC 9(4).
           03 TOTAL-H-RATE PIC 9(8)V9(2).
           03 TOTAL-S-RATE PIC 9(10)V9(2).
           03 EMP-RATE-FORMATER PIC 9(4)V9(2).
           03 DEDUCT-ARRAY-FORMATOR OCCURS 5 TIMES.
               05 DEDUCT-FORMAT PIC 9(3)V9(2).
           03 SUB PIC 99.
           03 TOTAL-EMP-DEDUCT PIC 9(6)V9(2).
           03 DEDUCT-COUNT PIC 9(4).
           03 TOTAL-AVG-DEDUCT PIC 9(6)V9(2).
           03 TOTAL-DEDUCT PIC 9(7)V9(2).
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT OUTPUT-FILE.
           PERFORM 900-READ-INPUT.
           PERFORM 100-PRINT-HEADER.
           PERFORM 200-LOOP-FILE
               UNTIL EOF-I = 1;
           PERFORM 800-PRINT-FOOTER.
           CLOSE INPUT-FILE
               OUTPUT-FILE.
           STOP RUN.
      ******************************************************************
      *    PRINT THE HEADER
      ******************************************************************
       100-PRINT-HEADER.
           ACCEPT H1-CURR-DATE FROM DATE.
           IF PAGE-NUM = 1
               WRITE OUTPUT-RECORD FROM OUTPUT-HEADING1
           ELSE
               MOVE SPACES TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
                   AFTER ADVANCING PAGE
               WRITE OUTPUT-RECORD FROM OUTPUT-HEADING1
                   AFTER ADVANCING 1 LINE
           END-IF.
           WRITE OUTPUT-RECORD FROM OUTPUT-HEADING2
               AFTER ADVANCING 1 LINE.
           WRITE OUTPUT-RECORD FROM OUTPUT-HEADING3
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD
               AFTER ADVANCING 1 LINE.
           MOVE 0 TO RECORD-PAGE-COUNTER.
           ADD 1 TO PAGE-NUM.
           MOVE PAGE-NUM TO H1-PAGE-NUM.
      ******************************************************************
      *    LOOPING THROUGH THE RECORDS IN THE EMPL.PROG1 FILE
      ******************************************************************
       200-LOOP-FILE.
           PERFORM 300-OUTPUT-RECORDS.
           PERFORM 900-READ-INPUT.
      ******************************************************************
      *    KEEPS TRACK OF NUMBER OF LINES PRINTED
      ******************************************************************
       310-PAGE-COUNTER.
           ADD 1 TO RECORD-PAGE-COUNTER.
           IF RECORD-PAGE-COUNTER = 25
               PERFORM 100-PRINT-HEADER
           END-IF.
      ******************************************************************
      *    PRINT THE EMPLOYEE RECORDS
      ******************************************************************
       300-OUTPUT-RECORDS.
           MOVE I-EMP-ID TO D1-EMP-ID.
           MOVE I-SSN TO D1-SSN.
           INSPECT D1-SSN REPLACING ALL ' ' BY '-'.
           MOVE I-LNAME TO D1-LNAME.
           MOVE I-FNAME TO D1-FNAME.
           MOVE I-EMP-TYPE TO D1-EMP-TYPE.
           MOVE I-TITLE TO D1-TITLE.
           MOVE I-DATE TO D1-DATE.
           WRITE OUTPUT-RECORD FROM OUTPUT-DATA1
               AFTER ADVANCING 1 LINE.
           PERFORM 310-PAGE-COUNTER.
           PERFORM 320-PRINT-DEDUCT.
           ADD 1 TO EMP-COUNTER.
      ******************************************************************
      *    PRINT THE DUDUCT VALUES
      ******************************************************************
       320-PRINT-DEDUCT.
           MOVE I-EMP-RATE TO EMP-RATE-FORMATER.
           MOVE I-DEDUCT(1) TO DEDUCT-FORMAT(1).
      *    CALCULATED COMPUTATION FOR FOOTER OUTPUT
           IF I-EMP-STATUS = 'H'
               COMPUTE EMP-H-COUNT = EMP-H-COUNT + 1
               COMPUTE TOTAL-H-RATE = TOTAL-H-RATE + EMP-RATE-FORMATER
              ELSE
               COMPUTE EMP-S-COUNT = EMP-S-COUNT + 1
               COMPUTE TOTAL-S-RATE = TOTAL-S-RATE + EMP-RATE-FORMATER
           END-IF.
           PERFORM VARYING SUB FROM 1 BY 1
               UNTIL SUB > 5
               COMPUTE TOTAL-EMP-DEDUCT = 
                   TOTAL-EMP-DEDUCT + DEDUCT-FORMAT(SUB)
           END-PERFORM.
           MOVE DEDUCT-FORMAT(1) TO D2-DEDUCT1.
           MOVE EMP-RATE-FORMATER TO D2-EMP-RATE.
           MOVE I-EMP-STATUS TO D2-EMP-STATUS.
           WRITE OUTPUT-RECORD FROM OUTPUT-DATA2
               AFTER ADVANCING 1 LINE.
           PERFORM 310-PAGE-COUNTER.
      *    PRINT THE DEDUCT OF EMP HAVE
           PERFORM VARYING SUB FROM 2 BY 1
               UNTIL SUB > 5
               MOVE I-DEDUCT(SUB) TO DEDUCT-FORMAT(SUB)
               MOVE DEDUCT-FORMAT(SUB) TO D3-DEDUCTOTHERS
               WRITE OUTPUT-RECORD FROM OUTPUT-DATA3
                   AFTER ADVANCING 1 LINE
               PERFORM 310-PAGE-COUNTER
           END-PERFORM.
      *    PRINT THE TOTAL EMP DEDUCT
           MOVE TOTAL-EMP-DEDUCT TO D4-DEDUCT-TOTAL.
           WRITE OUTPUT-RECORD FROM OUTPUT-DATA4
               AFTER ADVANCING 1 LINE.
           PERFORM 310-PAGE-COUNTER.
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
               AFTER ADVANCING 1 LINE.
           PERFORM 310-PAGE-COUNTER.
           COMPUTE DEDUCT-COUNT = DEDUCT-COUNT + 1.
           COMPUTE TOTAL-DEDUCT = TOTAL-DEDUCT + TOTAL-EMP-DEDUCT.
           MOVE 0 TO TOTAL-EMP-DEDUCT.
      ******************************************************************
      *    PRINT THE FOOTER DATA
      ******************************************************************
       800-PRINT-FOOTER.
      *    PRINT FOOTER HEADER.
           MOVE SPACES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD
               AFTER ADVANCING PAGE.
           ACCEPT H1-CURR-DATE FROM DATE.
           WRITE OUTPUT-RECORD FROM OUTPUT-HEADING1
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 1.
           MOVE EMP-COUNTER TO F1-EMP-COUNTER.
           WRITE OUTPUT-RECORD FROM OUTPUT-FOOTER1
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 2.
           MOVE EMP-H-COUNT TO F2-EMP-H-COUNT.
           COMPUTE TOTAL-H-RATE = TOTAL-H-RATE / EMP-H-COUNT.
           MOVE TOTAL-H-RATE TO F2-AVG-H-RATE.
           COMPUTE TOTAL-AVG-DEDUCT = TOTAL-DEDUCT / DEDUCT-COUNT.
           MOVE TOTAL-AVG-DEDUCT TO F2-TOTAL-AVG-DEDUCT.
           WRITE OUTPUT-RECORD FROM OUTPUT-FOOTER2
               AFTER ADVANCING 1 LINE.
      *    PRINT FOOTER LINE 3.
           MOVE EMP-S-COUNT TO F3-EMP-S-COUNT
           COMPUTE TOTAL-S-RATE = TOTAL-S-RATE / EMP-S-COUNT.
           MOVE TOTAL-S-RATE TO F3-AVG-S-RATE.
           MOVE TOTAL-DEDUCT TO F3-TOTAL-DEDUCT.
           WRITE OUTPUT-RECORD FROM OUTPUT-FOOTER3
               AFTER ADVANCING 1 LINE.
      ******************************************************************
      *    READ IN NEWEMP FILE
      ******************************************************************
       900-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
      ******************************************************************
      *    ENDING LINE
      ******************************************************************
       END PROGRAM project1.
