       IDENTIFICATION DIVISION.                                         00001000
       PROGRAM-ID.    CALLIVP1.                                         00002000
       AUTHOR.        IBM PROGRAMMER.                                   00003000
       INSTALLATION.  STL                                               00004000
       DATE-WRITTEN.  JAN 25, 1997.                                     00005000
       DATE-COMPILED.                                                   00006000
                                                                        00007000
      ******************************************************************00008000
      * PRODUCT.      Enterprise COBOL                                 *00009000
      *                                                                *00010000
      * TEST FUNCTION.                                                 *00011000
      *   THIS PROGRAM IS USED TO VERIFY A SUCCESSFUL INSTALLATION OF  *00012000
      *   THE Enterprise COBOL compiler and Language Environment       *00013000
      *   run-time library.                                            *00014000
      *                                                                *00015000
      *   THE MAIN PROGRAM 'CALLIVP1' CALLS 'NSTSUBA' WHICH THEN CALLS *00016000
      *   'NSTSUBB', 'NSTSUBC' AND 'NSTSUBD'. 'CALLIVP1' ALSO MAKES    *00017000
      *   CALLS TO 'NSTSUBE', 'NSTSUBF' AND 'NSTSUBG'.                 *00018000
      *                                                                *00019000
      *   CALLIVP1---------        THIS DIAGRAM ILLUSTRATES THE LEVELS *00020000
      *   | NSTSUBA------ |        OF NESTED PROGRAMS.                 *00021000
      *   | | NSTSUBB-- | |                                            *00022000
      *   | | |       | | |                                            *00023000
      *   | | --------- | |                                            *00024000
      *   | | NSTSUBC-- | |                                            *00025000
      *   | | |       | | |                                            *00026000
      *   | | --------- | |                                            *00027000
      *   | | NSTSUBD-- | |                                            *00028000
      *   | | |       | | |                                            *00029000
      *   | | --------- | |                                            *00030000
      *   | ------------- |                                            *00031000
      *   | NSTSUBE------ |                                            *00032000
      *   | |           | |                                            *00033000
      *   | ------------- |                                            *00034000
      *   | NSTSUBF------ |                                            *00035000
      *   | |           | |                                            *00036000
      *   | ------------- |                                            *00037000
      *   | NSTSUBG------ |                                            *00038000
      *   | |           | |                                            *00039000
      *   | ------------- |                                            *00040000
      *   -----------------                                            *00041000
      *                                                                *00042000
      *  EXPECTED MESSAGES.                                            *00043000
      *        **** START OF CALLIVP1 ****                             *00044000
      *        **** CALLIVP1 SUCCESSFUL ****                           *00045000
      *                   OR                                           *00046000
      *        **** START OF CALLIVP1 ****                             *00047000
      *        **** CALLIVP1 FAILED ****                               *00048000
      *                                                                *00049000
      *  CRITERIA FOR SUCCESS.                                         *00050000
      *    SELF CHECKING - CALLIVP1 'SUCCESSFUL' SHOULD BE DISPLAYED   *00051000
      *    AT END OF RUN.                                              *00052000
      *                                                                *00053000
      ******************************************************************00054000
      /                                                                 00055000
       ENVIRONMENT DIVISION.                                            00056000
       CONFIGURATION SECTION.                                           00057000
       SOURCE-COMPUTER.  IBM-390.                                       00058000
       OBJECT-COMPUTER.  IBM-390.                                       00059000
                                                                        00060000
       DATA DIVISION.                                                   00061000
       WORKING-STORAGE SECTION.                                         00062000
                                                                        00063000
       01  CALL-REC IS GLOBAL.                                          00064000
           05  CUST-NUM              PIC X(4).                          00065000
           05  FILLER REDEFINES CUST-NUM.                               00066000
               10  CUST-NUM12        PIC 9(2).                          00067000
               10  CUST-NUM34        PIC 9(2).                          00068000
           05  CUST-NAME             PIC X(10).                         00069000
           05  CALLS-MADE            PIC 9(2).                          00070000
           05  NUM-CALLS OCCURS 1 TO 10 TIMES                           00071000
               DEPENDING ON CALLS-MADE.                                 00072000
               10  CASE4-LINK1.                                         00073000
                   15  AREA-CODE         PIC 9(3).                      00074000
                   15  NUM-MINUTES       PIC 9(3).                      00075000
                   15  CITY              PIC X(5).                      00076000
                   15  COST              PIC 9(3).                      00077000
           05  CUST-RATE-COST.                                          00078000
               10  CUST-RATE             PIC 9.                         00079000
               10  TOTAL-COST            PIC 9(6).                      00080000
                                                                        00081000
       01  WS-VARIABLES.                                                00082000
           05  SUB1                      PIC 9(02).                     00083000
           05  TEST-STATUS               PIC X         VALUE "Y".       00084000
                                                                        00085000
      ****************************************************************  00086000
      *                  PROCEDURE DIVISION                          *  00087000
      ****************************************************************  00088000
                                                                        00089000
       PROCEDURE DIVISION.                                              00090000
                                                                        00091000
      *                                                                 00092000
       A000-MAIN-DRIVER.                                                00093000
      *                                                                 00094000
           DISPLAY "***** START OF CALLIVP1 *****".                     00095000
                                                                        00096000
           PERFORM B100-CALL.                                           00097000
                                                                        00098000
           IF TEST-STATUS = "Y"                                         00099000
             DISPLAY "***** CALLIVP1 SUCCESSFUL *****"                  00100000
           ELSE                                                         00101000
             DISPLAY "+++++ CALLIVP1 FAILED +++++".                     00102000
           STOP RUN.                                                    00103000
                                                                        00104000
      *                                                                 00105000
       B100-CALL.                                                       00106000
      *                                                                 00107000
           MOVE 1111 TO CUST-NUM.                                       00108000
           MOVE "AAAAAAAAAA" TO CUST-NAME.                              00109000
           MOVE 5   TO CALLS-MADE.                                      00110000
           MOVE 408 TO AREA-CODE(1) AREA-CODE(3) AREA-CODE(5).          00111000
           MOVE 409 TO AREA-CODE(2).                                    00112000
           MOVE 410 TO AREA-CODE(4).                                    00113000
           MOVE 5   TO NUM-MINUTES(1).                                  00114000
           MOVE 10  TO NUM-MINUTES(2).                                  00115000
           MOVE 15  TO NUM-MINUTES(3).                                  00116000
           MOVE 20  TO NUM-MINUTES(4).                                  00117000
           MOVE 25  TO NUM-MINUTES(5).                                  00118000
           MOVE 1   TO CUST-RATE.                                       00119000
                                                                        00120000
           PERFORM C120-CALL1.                                          00121000
                                                                        00122000
           IF CUST-NUM   NOT = 1111 OR                                  00123000
              CUST-NAME  NOT = "AAAAAAAAAA" OR                          00124000
              TOTAL-COST NOT = 493                                      00125000
             MOVE "N" TO TEST-STATUS                                    00126000
             DISPLAY "+++ TROUBLE WITH CUSTOMER " CUST-NUM " +++"       00127000
             DISPLAY "TOTAL-COST = " TOTAL-COST.                        00128000
                                                                        00129000
           MOVE 2222 TO CUST-NUM.                                       00130000
           MOVE "BBBBBBBBBB" TO CUST-NAME.                              00131000
           MOVE 3   TO CALLS-MADE.                                      00132000
           MOVE 408 TO AREA-CODE(1).                                    00133000
           MOVE 409 TO AREA-CODE(2).                                    00134000
           MOVE 410 TO AREA-CODE(3).                                    00135000
           MOVE 5   TO NUM-MINUTES(1).                                  00136000
           MOVE 50  TO NUM-MINUTES(2).                                  00137000
           MOVE 500 TO NUM-MINUTES(3).                                  00138000
           MOVE 2   TO CUST-RATE.                                       00139000
                                                                        00140000
           PERFORM C120-CALL1.                                          00141000
                                                                        00142000
           IF CUST-NUM   NOT = 2222 OR                                  00143000
              CUST-NAME  NOT = "BBBBBBBBBB" OR                          00144000
              TOTAL-COST NOT = 846                                      00145000
             MOVE "N" TO TEST-STATUS                                    00146000
             DISPLAY "TROUBLE WITH CUSTOMER " CUST-NUM                  00147000
             DISPLAY "TOTAL-COST = " TOTAL-COST.                        00148000
                                                                        00149000
      ****************************************************************  00150000
      *  MAKE CALLS TO NSTSUBA, NSTSUBE, NSTSUBF, AND NSTSUBG.       *  00151000
      ****************************************************************  00152000
      *                                                                 00153000
       C120-CALL1.                                                      00154000
      *                                                                 00155000
           CALL "NSTSUBA"                                               00156000
             ON EXCEPTION                                               00157000
               DISPLAY "< TROUBLE IN C120-CALL1 >"                      00158000
               DISPLAY "< ON EXCEPTION BRANCH TAKEN >"                  00159000
               DISPLAY "< TESTCASE IS HALTING >"                        00160000
               STOP RUN                                                 00161000
             NOT ON EXCEPTION                                           00162000
               CALL "NSTSUBE"                                           00163000
                 NOT ON EXCEPTION                                       00164000
                   EVALUATE CUST-RATE                                   00165000
                     WHEN 1 CALL "NSTSUBF"                              00166000
                     WHEN 2 CALL "NSTSUBG"                              00167000
                   END-EVALUATE                                         00168000
               END-CALL                                                 00169000
           END-CALL.                                                    00170000
                                                                        00171000
      /                                                                 00172000
       IDENTIFICATION DIVISION.                                         00173000
       PROGRAM-ID.  NSTSUBA.                                            00174000
      ****************************************************************  00175000
      *   THIS SUB-PROGRAM IS NESTED INSIDE CALLIVP1 AND IS CALLED BY*  00176000
      *   CALLIVP1.                                                  *  00177000
      *   THIS SUB-PROGRAM HAS NESTED WITHIN IT NSTSUBB, NSTSUBC, AND*  00178000
      *   NSTSUBD AND MAKES CALLS TO THEM.                           *  00179000
      ****************************************************************  00180000
                                                                        00181000
       DATA DIVISION.                                                   00182000
                                                                        00183000
       WORKING-STORAGE SECTION.                                         00184000
                                                                        00185000
       01  WS-VARIABLES.                                                00186000
           05  SUB1               PIC 9(02).                            00187000
                                                                        00188000
      ****************************************************************  00189000
      *                  PROCEDURE DIVISION                          *  00190000
      ****************************************************************  00191000
                                                                        00192000
       PROCEDURE DIVISION.                                              00193000
                                                                        00194000
       A100-ENTRY.                                                      00195000
                                                                        00196000
           PERFORM B200-CALL WITH TEST AFTER                            00197000
             VARYING SUB1 FROM 1 BY 1                                   00198000
             UNTIL SUB1 = CALLS-MADE.                                   00199000
                                                                        00200000
           GOBACK.                                                      00201000
                                                                        00202000
       B200-CALL.                                                       00203000
           EVALUATE AREA-CODE(SUB1)                                     00204000
             WHEN 408 CALL "NSTSUBB" USING CASE4-LINK1(SUB1)            00205000
                        NOT ON EXCEPTION                                00206000
                          MOVE "AAAAA" TO CITY(SUB1)                    00207000
                      END-CALL                                          00208000
             WHEN 409 CALL "NSTSUBC" USING CASE4-LINK1(SUB1)            00209000
                        NOT ON EXCEPTION                                00210000
                          MOVE "BBBBB" TO CITY(SUB1)                    00211000
                      END-CALL                                          00212000
             WHEN 410 CALL "NSTSUBD" USING CASE4-LINK1(SUB1)            00213000
                        NOT ON EXCEPTION                                00214000
                          MOVE "CCCCC" TO CITY(SUB1)                    00215000
                      END-CALL                                          00216000
             WHEN OTHER DISPLAY "INCORRECT AREA CODE"                   00217000
           END-EVALUATE.                                                00218000
                                                                        00219000
      /                                                                 00220000
       IDENTIFICATION DIVISION.                                         00221000
       PROGRAM-ID.  NSTSUBB, IS INITIAL.                                00222000
      ****************************************************************  00223000
      *   THIS SUB-PROGRAM IS NESTED WITHIN NSTSUBA AND IS CALLED BY *  00224000
      *   NSTSUBA.                                                   *  00225000
      ****************************************************************  00226000
                                                                        00227000
       DATA DIVISION.                                                   00228000
                                                                        00229000
       WORKING-STORAGE SECTION.                                         00230000
                                                                        00231000
       01  WS-VARIABLES.                                                00232000
           05  INIT-COST       PIC 9(06)       VALUE 1.                 00233000
                                                                        00234000
       LINKAGE SECTION.                                                 00235000
       01  CASE4-LINK1.                                                 00236000
           05  AREA-CODE       PIC 9(3).                                00237000
           05  NUM-MINUTES     PIC 9(3).                                00238000
           05  CITY            PIC X(5).                                00239000
           05  COST            PIC 9(3).                                00240000
                                                                        00241000
      ****************************************************************  00242000
      *                  PROCEDURE DIVISION                          *  00243000
      ****************************************************************  00244000
                                                                        00245000
       PROCEDURE DIVISION USING CASE4-LINK1.                            00246000
                                                                        00247000
       A100-ENTRY.                                                      00248000
                                                                        00249000
           EVALUATE TRUE                                                00250000
             WHEN NUM-MINUTES > 0 AND <= 20                             00251000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 4)       00252000
             WHEN NUM-MINUTES > 20 AND <= 40                            00253000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 3)       00254000
             WHEN NUM-MINUTES > 40 AND <= 100                           00255000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 2)       00256000
             WHEN NUM-MINUTES > 100                                     00257000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 1)       00258000
             WHEN OTHER DISPLAY "PROBLEM WITH NUMBER OF MINUTES"        00259000
           END-EVALUATE.                                                00260000
                                                                        00261000
           MOVE INIT-COST TO COST.                                      00262000
                                                                        00263000
           EXIT PROGRAM.                                                00264000
                                                                        00265000
       END PROGRAM NSTSUBB.                                             00266000
                                                                        00267000
      /                                                                 00268000
       IDENTIFICATION DIVISION.                                         00269000
       PROGRAM-ID.  NSTSUBC, IS INITIAL.                                00270000
      ****************************************************************  00271000
      *   THIS SUB-PROGRAM IS NESTED WITHIN NSTSUBA AND IS CALLED BY *  00272000
      *   NSTSUBA.                                                   *  00273000
      ****************************************************************  00274000
                                                                        00275000
       DATA DIVISION.                                                   00276000
                                                                        00277000
       WORKING-STORAGE SECTION.                                         00278000
                                                                        00279000
       01  WS-VARIABLES.                                                00280000
           05  INIT-COST       PIC 9(06)       VALUE 2.                 00281000
                                                                        00282000
       LINKAGE SECTION.                                                 00283000
       01  CASE4-LINK1.                                                 00284000
           05  AREA-CODE       PIC 9(3).                                00285000
           05  NUM-MINUTES     PIC 9(3).                                00286000
           05  CITY            PIC X(5).                                00287000
           05  COST            PIC 9(3).                                00288000
                                                                        00289000
      ****************************************************************  00290000
      *                  PROCEDURE DIVISION                          *  00291000
      ****************************************************************  00292000
                                                                        00293000
       PROCEDURE DIVISION USING CASE4-LINK1.                            00294000
                                                                        00295000
       A100-ENTRY.                                                      00296000
                                                                        00297000
           EVALUATE TRUE                                                00298000
             WHEN NUM-MINUTES > 0 AND <= 20                             00299000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 8)       00300000
             WHEN NUM-MINUTES > 20 AND <= 40                            00301000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 7)       00302000
             WHEN NUM-MINUTES > 40 AND <= 90                            00303000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 6)       00304000
             WHEN NUM-MINUTES > 90                                      00305000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 5)       00306000
             WHEN OTHER DISPLAY "PROBLEM WITH NUMBER OF MINUTES"        00307000
           END-EVALUATE.                                                00308000
                                                                        00309000
           MOVE INIT-COST TO COST.                                      00310000
                                                                        00311000
           EXIT PROGRAM.                                                00312000
                                                                        00313000
       END PROGRAM NSTSUBC.                                             00314000
                                                                        00315000
      /                                                                 00316000
       IDENTIFICATION DIVISION.                                         00317000
       PROGRAM-ID.  NSTSUBD, IS INITIAL.                                00318000
      ****************************************************************  00319000
      *   THIS SUB-PROGRAM IS NESTED WITHIN NSTSUBA AND IS CALLED BY *  00320000
      *   NSTSUBA.                                                   *  00321000
      ****************************************************************  00322000
                                                                        00323000
       DATA DIVISION.                                                   00324000
                                                                        00325000
       WORKING-STORAGE SECTION.                                         00326000
       01  WS-VARIABLES.                                                00327000
           05  INIT-COST       PIC 9(06)       VALUE 3.                 00328000
                                                                        00329000
       LINKAGE SECTION.                                                 00330000
       01  CASE4-LINK1.                                                 00331000
           05  AREA-CODE       PIC 9(3).                                00332000
           05  NUM-MINUTES     PIC 9(3).                                00333000
           05  CITY            PIC X(5).                                00334000
           05  COST            PIC 9(3).                                00335000
                                                                        00336000
      ****************************************************************  00337000
      *                  PROCEDURE DIVISION                          *  00338000
      ****************************************************************  00339000
                                                                        00340000
       PROCEDURE DIVISION USING CASE4-LINK1.                            00341000
                                                                        00342000
       A100-ENTRY.                                                      00343000
                                                                        00344000
           EVALUATE TRUE                                                00345000
             WHEN NUM-MINUTES > 0 AND <= 20                             00346000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 12)      00347000
             WHEN NUM-MINUTES > 20 AND <= 40                            00348000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 11)      00349000
             WHEN NUM-MINUTES > 40 AND <= 90                            00350000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 10)      00351000
             WHEN NUM-MINUTES > 90                                      00352000
                COMPUTE INIT-COST = INIT-COST + (NUM-MINUTES * 9)       00353000
             WHEN OTHER DISPLAY "PROBLEM WITH NUMBER OF MINUTES"        00354000
           END-EVALUATE.                                                00355000
                                                                        00356000
           MOVE INIT-COST TO COST.                                      00357000
                                                                        00358000
           EXIT PROGRAM.                                                00359000
                                                                        00360000
       END PROGRAM NSTSUBD.                                             00361000
                                                                        00362000
       END PROGRAM NSTSUBA.                                             00363000
                                                                        00364000
      /                                                                 00365000
       IDENTIFICATION DIVISION.                                         00366000
       PROGRAM-ID.  NSTSUBE.                                            00367000
      ****************************************************************  00368000
      *   THIS SUB-PROGRAM IS NESTED INSIDE CALLIVP1 AND IS CALLED BY*  00369000
      *   CALLIVP1.                                                  *  00370000
      ****************************************************************  00371000
                                                                        00372000
       DATA DIVISION.                                                   00373000
                                                                        00374000
       WORKING-STORAGE SECTION.                                         00375000
       01  WS-VARIABLES.                                                00376000
           05  SUB1                PIC 9(2).                            00377000
                                                                        00378000
      ****************************************************************  00379000
      *                  PROCEDURE DIVISION                          *  00380000
      ****************************************************************  00381000
                                                                        00382000
       PROCEDURE DIVISION.                                              00383000
                                                                        00384000
       A100-ENTRY.                                                      00385000
                                                                        00386000
           MOVE 0 TO TOTAL-COST.                                        00387000
           PERFORM WITH TEST AFTER                                      00388000
           VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 = CALLS-MADE             00389000
               COMPUTE TOTAL-COST = TOTAL-COST + COST(SUB1)             00390000
           END-PERFORM.                                                 00391000
                                                                        00392000
           GOBACK.                                                      00393000
                                                                        00394000
       END PROGRAM NSTSUBE.                                             00395000
                                                                        00396000
      /                                                                 00397000
       IDENTIFICATION DIVISION.                                         00398000
       PROGRAM-ID.  NSTSUBF.                                            00399000
      ****************************************************************  00400000
      *   THIS SUB-PROGRAM IS NESTED INSIDE CALLIVP1 AND IS CALLED BY*  00401000
      *   CALLIVP1.                                                  *  00402000
      ****************************************************************  00403000
                                                                        00404000
       DATA DIVISION.                                                   00405000
                                                                        00406000
       WORKING-STORAGE SECTION.                                         00407000
       01  WS-VARIABLES.                                                00408000
           05  INIT-COST       PIC 9(6)      VALUE 5.                   00409000
                                                                        00410000
      ****************************************************************  00411000
      *                  PROCEDURE DIVISION                          *  00412000
      ****************************************************************  00413000
                                                                        00414000
       PROCEDURE DIVISION.                                              00415000
                                                                        00416000
       B100-ENTRY.                                                      00417000
                                                                        00418000
           COMPUTE INIT-COST = INIT-COST + TOTAL-COST + 5.              00419000
           MOVE INIT-COST TO TOTAL-COST.                                00420000
                                                                        00421000
           GOBACK.                                                      00422000
                                                                        00423000
       END PROGRAM NSTSUBF.                                             00424000
                                                                        00425000
      /                                                                 00426000
       IDENTIFICATION DIVISION.                                         00427000
       PROGRAM-ID.  NSTSUBG.                                            00428000
      ****************************************************************  00429000
      *   THIS SUB-PROGRAM IS NESTED INSIDE CALLIVP1 AND IS CALLED BY*  00430000
      *   CALLIVP1.                                                  *  00431000
      ****************************************************************  00432000
                                                                        00433000
       DATA DIVISION.                                                   00434000
                                                                        00435000
       WORKING-STORAGE SECTION.                                         00436000
       01  WS-VARIABLES.                                                00437000
           05  INIT-COST       PIC 9(6)      VALUE 10.                  00438000
                                                                        00439000
      ****************************************************************  00440000
      *                  PROCEDURE DIVISION                          *  00441000
      ****************************************************************  00442000
                                                                        00443000
       PROCEDURE DIVISION.                                              00444000
                                                                        00445000
       C100-ENTRY.                                                      00446000
                                                                        00447000
           COMPUTE INIT-COST = INIT-COST + TOTAL-COST + 10.             00448000
           MOVE INIT-COST TO TOTAL-COST.                                00449000
                                                                        00450000
           GOBACK.                                                      00451000
                                                                        00452000
       END PROGRAM NSTSUBG.                                             00453000
                                                                        00454000
       END PROGRAM CALLIVP1.                                            00455000
