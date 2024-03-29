*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - OUTSTRN                             *
*                 3270 OUTPUT STRING PROCESSOR        *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - NOVEMBER 1991                       *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: OUTSTRN                    *
*     OTHER ENTRIES:                                  *
*        PARMLIST = PARAMETER FOR SCREEN AND PRINT    *
*        BOXPARML = BOX (TOP ROW/COL) (BOT ROW/COL)   *
*        SCREENL  = OUTPUT STRING LENGTH              *
*        SCREEN   = SCREEN OUTPUT BUFFER              *
*        SCREENE  = SCREEN OUTPUT BUFFER DELIMITER    *
*        SCRSYSNM = SYSTEM DESCRIPTION FROM TREE FILE *
*                                                     *
*     EXTERNAL REFERENCES:                            *
*        PRINTS = PRINT DCB ADDRESS                   *
*                                                     *
*******************************************************
*        ENTRY OUTSTRN,PARMLIST,BOXPARML,SCREENL,SCREEN,SCRSYSNM
*        ENTRY SCREENE
*        EXTRN PRINTS
*
*        PRINT NOGEN
         TITLE '                                * * *   OUTSTRN  * * *'
**************************************************************
*        OUTSTRN ROUTINE
**************************************************************
OUTSTRN  CSECT
         ENTRY
XX1      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XX1,R12,R11
         LA    R15,XX1SAV1     GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)      SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)      SAVE PROGRAMS SAVE AREA IN CALLERS AREA
         LR    R13,R15         GET PROGRAMS SAVE AREA ADDR
         L     R11,XX1SAV2     LOAD 2ND BASE REG
         B     AROUND          SKIP SAVE AREA FLD
XX1SAV1  DS    18F             RETURN SAVE REGS AREA
XX1SAV2  DC    A(XX1+4096)     2ND BASE REG ADDR
         DC    CL12'<<OUTSTRN>>'  DUMP PROGRAM ID
AROUND   DS    0H
*
         L     R5,0(R1)              GET PARAM LIST ADDRESS
         CLI   PARMLIST,C'C'         IS THIS A CLEAR REQUEST?
         BNE   OUTNOCLS              NO, JUST UPDATE SCREEN
         CLC   CLSCOOR(4),=F'0'      ANY CLEAR COORDINATES?
         BE    OUTNRMCL              NO, DO NORMAL CLEAR
         BAL   R14,GETDATE           GET DATE & TIME
         MVC   SCRDAT(6),DATE        MOVE DATE TO OUT STR
         MVC   SCRDAT+6(2),DATE+8    MOVE YEAR SUFIX
         MVC   SCRTIM,TIME           MOVE TIME TO OUT STR
         LA    R8,SCRDATIM           SEND DATE & TIME ONLY
         BAL   R14,GENSTR            GENERATE OUTPUT STRING
         MVC   BOXCSAVE(8),BOXCHARS  SAVE BOX CHARACTERS
         MVC   BOXCHARS(8),=CL8' '   ERASE BOX CHARACTERS
         MVC   BOXPSAVE(4),BOXPARML  SAVE BOX COORDINATES
         MVC   BOXPARML(4),CLSCOOR   USE CLEAR COORDINATES
         BAL   R14,DOBOX             GENERATE BLANK BOX
         MVC   BOXCHARS(8),BOXCSAVE  RESTORE BOX CHARACTERS
         MVC   BOXPARML(4),BOXPSAVE  RESTORE BOX COORDINATES
         B     OUTGOON               SKIP NORMAL CLEAR PROCEDURE
OUTNRMCL DS    0H
*        MVC   STRPOINT,=A(SCREEN)   RESET STRING BUFFER PIONTER
         MVC   SCREENL(4),=F'0'      RESET STRING BUFFER PIONTER
         BAL   R14,GETDATE           GET DATE & TIME
         MVC   SCRDAT(6),DATE        MOVE DATE TO OUT STR
         MVC   SCRDAT+6(2),DATE+8    MOVE YEAR SUFIX
         MVC   SCRTIM,TIME           MOVE TIME TO OUT STR
         MVC   GENBUF,GENCLS         MOVE CLEAR SCREEN COMMAND
         LA    R2,GENBUF             CLEAR SCR OR UPDATE SCR COMMAND
         LA    R3,2                  LENGTH
         BAL   R14,PUSHST            PILE IT UP ON OUTPUT STR
         LA    R8,PANT1              CLEAR SCREEN PANEL
         BAL   R14,GENSTR            GENERATE OUTPUT STRING
         B     OUTGOON               CONTINUE ON
OUTNOCLS DS    0H
OUTGOON  DS    0H
         CLC   0(3,R5),=C'BOX'       IS THIS A REQUEST FOR BOX?
         BE    OUTBOX                YES, DO BOX
         CLC   0(3,R5),=C'LIN'       IS THIS A REQUEST FOR LINE?
         BE    OUTBOX                YES, DO LINE AT BOX ROUT
         CLC   0(3,R5),=C'CLS'       IS THIS A CLEAR SCREEN REQ?
         BE    OUTEX                 YES, EXIT
         CLC   PARMLIST+5(4),=F'0'   ARE THERE ANY PANEL INSTRUCTIONS?
         BE    OUTEX                 NO, EXIT
         MVC   GENBUF,GENUPD         MOVE UPDATE SCREEN COMMAND
         ICM   R8,B'1111',PARMLIST+5 CALLER'S UPDATE SCREEN PANEL
         BAL   R14,GENSTR            GENERATE OUTPUT STRING
         B     OUTEX
OUTBOX   DS    0H
         BAL   R14,DOBOX             GENERATE BOX
OUTEX    DS    0H
         EXIT  SAVE=YES
*
*******************************************************
*                   SUBROUTINES                       *
*******************************************************
*     DOBOX - GENERATE BOX SIDES                      *
*     ON ENTRY: R14 = RETURN ADDR                     *
*     ON EXIT:  OUTPUT STRING AT SCREEN               *
*******************************************************
DOBOX    DS    0H
         ST    R14,SVBOX              PUSH RET ADDR
         STM   R1,R8,BOXRR            PUSH REGS
         CLC   0(3,R5),=C'LIN'        IS THIS A REQUEST FOR LINE?
         BNE   DOBOX1                 NO, DO BOX
         SR    R8,R8                  SET FOR IC
         IC    R8,BOXPARML            GET TOP ROW
         LA    R8,1(R8)               DO LINE BELOW HORIZONTAL MENU
         STC   R8,SCRLINE             POKE ROW LINE
         MVC   ROW(1),SCRLINE         POKE ROW
         MVI   COL,80                 POKE TOP RITE COLUMN
         BAL   R14,TWELVE             GENERATE 3270 ADDRESS CODE
         MVC   SCRLINET(2),CHABUFAD   POKE SCR ADDR IN LINE COMMAND
         LA    R8,SCRLINE             GET LINE COMMAND ADDRESS
         BAL   R14,GENSTR             GENERATE OUTPUT STRING
         B     DOBOXEX                EXIT
*
DOBOX1   DS    0H
         MVC   BOXP(4),BOXPARML       SET TOP ADDRESSES
         MVC   BOXP+2(1),BOXPARML     SET TOP ADDRESSES
         MVC   BOXC(3),BOXCHARS       SET TOP CHARS
         BAL   R14,DOBOXLIN           GENERATE BOX TOP LINE
         MVC   BOXP(4),BOXPARML       SET BOTTOM ADDRESSES
         MVC   BOXP(1),BOXPARML+2     SET BOTTOM ADDRESSES
         MVC   BOXC(3),BOXCHARS+5     SET BOTTOM BOX CHARS
         BAL   R14,DOBOXLIN           GENERATE BOX BOTTOM LINE
*
         MVC   BOXP(4),BOXPARML       SET MIDDLE ADDRESSES
         MVC   BOXC(1),BOXCHARS+3     SET MIDDLE BOX CHARS
         MVI   BOXC+1,C' '            SET MIDDLE BOX CHARS
*        MVI   BOXC+1,X'00'           SET MIDDLE BOX CHARS
         MVC   BOXC+2(1),BOXCHARS+4   SET MIDDLE BOX CHARS
         XR    R6,R6                  SET FOR USE
         IC    R6,BOXP+2              GET DELIMITER ROW
         BCTR  R6,0                   ADJUST FOR INBETWIN
         XR    R5,R5                  SET FOR USE
         IC    R5,BOXP                GET TOP ROW
DOBOXLP  DS    0H
         LA    R5,1(R5)               BUMP TO NEXT ROW
         STC   R5,BOXP                AND POKE FROM
         STC   R5,BOXP+2              AND POKE TO
         BAL   R14,DOBOXLIN           GENERATE OUTPUT STRING
         CR    R5,R6                  IS THIS THE LIMIT ROW?
         BL    DOBOXLP                NO, KEEP ROLLING IT
*
DOBOXEX  DS    0H
         L     R14,SVBOX              POP RET ADDR
         LM    R1,R8,BOXRR            POP REGS
         BR    R14
*
DOBOXLIN DS    0H
         ST    R14,SVBOXL             PUSH RET ADDR
         STM   R1,R8,BOXRRL           PUSH REGS
         MVC   SCRBOX(2),BOXP         POKE TOP LEFT CORNER ROW/COL
         MVC   SCRBOX+4(1),BOXC       POKE TOP LEFT CORNER CHAR
         MVC   SCRBOX+8(1),BOXC+1     POKE TOP HORIZ LINE CHAR
         MVC   SCRBOX+9(1),BOXC+2     POKE TOP RITE CORNER CHAR
         MVC   ROW(1),BOXP+2          POKE TOP RITE CORNER ROW
         MVC   COL(1),BOXP+3          POKE TOP RITE CORNER COLUMN
         BAL   R14,TWELVE             GENERATE 3270 ADDRESS CODE
         MVC   SCRBOX+6(2),CHABUFAD   POKE TOP RITE CORNER ADDRRESS
         LA    R8,SCRBOX              PASS ADDRESS TO GENERATOR
         BAL   R14,GENSTR             GENERATE OUTPUT STRING
         L     R14,SVBOXL             POP RET ADDR
         LM    R1,R8,BOXRRL           POP REGS
         BR    R14
*
BOXP      DC    AL1(0,0,0,0)  R/C R/C  EACH BOX LINE ADDRESSES
BOXC      DC    CL3'   '               EACH BOX LINE CHARS
SCRBOX    EQU   *
 DC AL1(00,00),X'00',AL1(06),C'+',X'3C',AL1(00,00),C'-',C'+',X'00'
*   FROM R/C   ATTRIB   LENG  LRCR TOADDR TO R/C    TOP  RTCR  END
*        0  1     2       3    4     5       6  7    8    9    10
*BOXPARML DC    AL1(0,0,0,0)  R/C R/C
*BOXCHARS DC    CL8'+-+||+-+'  BOX CHARACTERS
*SCRVERT  DC    AL1(00,00),X'E8',AL1(01),C' ',X'00'
*SCRHORZ  DC    AL1(00,00),X'E8',AL1(00),CL80' ',X'00'
*
*******************************************************
*     GENSTR -  GENERATE PANEL OUTPUT STRING          *
*     ON ENTRY: R14 = RETURN ADDR                     *
*               R8  = INPUT BUFFER ADDRESS            *
*     ON EXIT:  OUTPUT STRING AT SCREEN               *
*******************************************************
GENSTR   DS    0H
         ST    R14,SVGEN       PUSH RET ADDR
         STM   R1,R8,GENRR     PUSH REGS
*
GENLP    DS    0H
         MVC   ROW,0(R8)       POKE ROW
         MVC   COL,1(R8)       POKE COL
         BAL   R14,TWELVE      GENERATE 3270 LOC CODE
         MVC   GENRC,CHABUFAD  MOVE GENERATED R/C
         LA    R2,GENRCC       FIELD COMMAND
         LA    R3,3            LENGTH
         BAL   R14,PUSHST      PILE IT UP ON OUTPUT STR
         CLI   2(R8),X'00'     USING AN ATTRIBUTE?
         BE    GENNOAT         NO, SKIP IT
         MVC   GENATT,2(R8)    POKE ATTRIBUTE
         LA    R2,GENATC       FIELD COMMAND
         LA    R3,2            LENGTH
         BAL   R14,PUSHST      PILE IT UP ON OUTPUT STR
GENNOAT  DS    0H
         XR    R3,R3           CLEAR R3
         IC    R3,3(R8)        GET STRING LENGTH
         C     R3,=F'0'        DO WE HAVE A STRING TO PILE?
         BNH   GENOSTR         NO, SKIP PILEING IT
         LA    R2,4(R8)        GET 1ST CHAR LOC
         BAL   R14,PUSHST      PILE IT UP ON OUTPUT STR
GENOSTR  DS    0H
         LA    R8,4(R3,R8)     BUMP INPUT ADDRESS
         CLI   0(R8),X'00'     WAS THIS ALL FOR NOW?
         BNE   GENLP           LOOP SOME MORE
*
         LM    R1,R8,GENRR     POP REGS
         L     R14,SVGEN       POP RET ADDR
         BR    R14
*
GENBUF   DC    X'F1C3'         DEFAULT CLEAR SCREEN/WRITE COMMAND
GENCLS   DC    X'F1C3'         CLEAR SCREEN/WRITE COMMAND
*GENBUF   DC    X'F5C3'         DEFAULT CLEAR SCREEN/WRITE COMMAND
*GENCLS   DC    X'F5C3'         CLEAR SCREEN/WRITE COMMAND
GENUPD   DC    X'F1C3'         UPDATE SCREEN/WRITE COMMNAD
GENRCC   DC    X'11'           ADDRESS COMMAND
GENRC    DC    X'0000'         ROW/COL LOCATION
GENATC   DC    X'1D'           ATTRIBUTE COMMMAD
GENATT   DC    X'00'           ATTRIBUTE
         DC    X'00'           ATTRIBUTE
*
*******************************************************
*     PUSHST -  PUSH STRING ON SCREEN OUTPUT BUFFER   *
*     ON ENTRY: STRPOINT MUST BE SET PRIOR ENTRY      *
*               R14 = RETURN ADDR                     *
*               R2  = DATA ADDRESS                    *
*               R3  = LENGTH                          *
*     ON EXIT:  DATA STACKED                          *
*******************************************************
PUSHST   DS    0H
         ST    R14,SVPUS             PUSH RET ADDR
         STM   R1,R8,PUSRR           PUSH REGS
*
         L     R15,=A(SCREENL)       SCREEN LENGTH ADDR
         L     R1,0(R15)             GET SCREEN SAPCE USED
         A     R1,=A(SCREEN)         SCREEN START ADDR
         LA    R4,0(R3,R1)           GET REACH
         C     R4,=A(SCREENE)        WILL IT THRESSPASS THE LIMIT?
         BNL   PUSHEX                YES, CAN PILE NO MORE
         LR    R5,R2                 SOURCE FIELD ADDRESS
         LR    R6,R3                 FIELD LENGTH
         LR    R8,R1                 DESTINATION FIELD ADDRESS
         BAL   R14,LONGMV            UPDATE SCREEN OUTPUT BUFF
         LA    R1,0(R1,R3)           BUMP
         S     R1,=A(SCREEN)         CALCULATE LENGTH
         L     R15,=A(SCREENL)       SCREEN SPACE USED
         ST    R1,0(R15)             UPDATE LENGTH
PUSHEX   DS    0H
         LM    R1,R8,PUSRR           POP REGS
         L     R14,SVPUS             POP RET ADDR
         BR    R14
*
*******************************************************
*    LONGMV - MOVE FIELDS LONGER THAN 256 BYTES       *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R5  = SOURCE ADDRESS                   *
*              R6  = SOURCE FIELD LENGTH              *
*              R8  = DESTINATION FIELD ADDRESS        *
*              R6  = DESTINATION FIELD LENGTH         *
*    ON EXIT:  FIELD MOVED                            *
*******************************************************
LONGMV   DS    0H
         ST    R14,SVLON               PUSH RET ADDR
         STM   R1,R8,LONRR             PUSH
*
         LR    R2,R5                   SOURCE FIELD ADDRESS
         LR    R3,R6                   SOURCE FIELD LENGTH
         LR    R4,R8                   DESTINATION FIELD ADDRESS
         LR    R5,R3                   DESTINATION FIELD LENGTH
         MVCL  R4,R2                   MOVE IT
*
         LM    R1,R8,LONRR             POP
         L     R14,SVLON               POP RET ADDR
         BR    R14
*
*
*******************************************************
*     PUSHST -  PUSH STRING ON SCREEN OUTPUT BUFFER   *
*     ON ENTRY: STRPOINT MUST BE SET PRIOR ENTRY      *
*               R14 = RETURN ADDR                     *
*               R2  = DATA ADDRESS                    *
*               R3  = LENGTH                          *
*     ON EXIT:  DATA STACKED, LENGTH UPDATED          *
*******************************************************
*PUSHST   DS    0H
*         ST    R14,SVPUS             PUSH RET ADDR
*         STM   R1,R3,PUSRR           PUSH REGS
*         L     R1,STRPOINT           GET LOCATION
*         LA    R1,0(R3,R1)           GET REACH
*         C     R1,=A(STRPOINL)       WILL IT THRESSPASS THE LIMIT?
*         BNL   PUSHEX                YES, CAN PILE NO MORE
*         L     R1,STRPOINT           GET LOC
*         L     R1,SCREENL            GET LOC
*         BCTR  R3,0                  ADJUST FOR EX
*         EX    R3,PUSMVC             INCLUDE PANEL DATA
*         LA    R1,1(R1,R3)           BUMP
*         ST    R1,STRPOINT           SAVE FOR NEXT USE
*         ST    R1,SCREENL            UPDATE LENGTH
*PUSHEX   DS    0H
*         LM    R1,R3,PUSRR           POP REGS
*         L     R14,SVPUS             POP RET ADDR
*         BR    R14
*
*PUSMVC   MVC   0(*-*,R1),0(R2)       INCLUDE PASS MARK
*STRPOINT DC    A(SCREEN)             GET LAST UPD ADDR
*STRPOINL EQU   SCREEN+L'SCREEN       LAST SCREEN BUFFER LOC
*
*******************************************************
*    MOVEL - MOVE LARGE AREA  (MORE THAN 256)         *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R15 = ADDRESS LIST = F'SOURCE ADDRESS' *
*                                 = F'LENGTH'         *
*                                 = F'DEST ADDRESS'   *
*                     ZERO LENGTH = FILL WITH CHAR    *
*    ON EXIT:  DATA MOVED                             *
*******************************************************
MOVEL    DS    0H
         ST    R14,SVMOVE              PUSH RET ADDR
         STM   R2,R5,MOVERR            PUSH
*
         L     R2,0(15)                LOAD SOURCE ADDRESS
         L     R3,4(15)                LOAD DATA LENGTH
         C     R3,=F'0'                ANY LENGTH?
         BNH   MOVREP                  NO, FILL OUTPUT WITH VALUE
         LR    R5,R3                   SAME FOR DEST LENGTH
         B     MOVEON                  GO MOVE
MOVREP   DS    0H
         SR    R5,R5                   USE INP CHAR TO FILL OUTPUT
MOVEON   DS    0H
         LA    R4,8(R15)               LOAD DEST ADDR
         MVCL  R4,R2                   MOVE WITH NO PADDING
*
         LM    R2,R5,MOVERR            POP
         L     R14,SVMOVE              POP RET ADDR
         BR    R14
*
*******************************************************
*    GETDATE - CALCULATE DATE USING SVC 18            *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R0 = X'HHMMSSTT'  HOUR/MIN/SEC/HTH     *
*              R1 = X'00000YYY'  YEAR                 *
*              R2 = X'MMDDWW00'  MONTH/DAY/WEEK DAY   *
*    ON EXIT: DATE = MM/DD/YYYY                       *
*             TIME = HH/MM/SS                         *
*******************************************************
GETDATE  DS    0H
         ST    R14,DATASV               PUSH EM REGS
         SVC   TIMER
         CVD   R1,PWORK                 PIC YEAR
         MVC   DYEAR,=X'402020202020'   MASK
         ED    DYEAR,PWORK+5            EDIT
*
         ST    R0,WORK                  TAKE HOUR
         SR    R0,R0                    PREPARE FOR IC
         IC    R0,WORK                  HOURS
         BAL   R1,CVT                   EDIT
         MVC   TIME(2),DWORK+2          OUTPUT HOUR
         IC    R0,WORK+1                MINUTES
         BAL   R1,CVT                   EDIT
         MVC   TIME+3(2),DWORK+2        OUTPUT MINUTES
         IC    R0,WORK+2                SECONDS
         BAL   R1,CVT                   EDIT
         MVC   TIME+6(2),DWORK+2        OUTPUT SECONDS
*
         ST    R2,WORK                  SAVE DATE
         IC    R0,WORK                  MONTH
         BAL   R1,CVT                   EDIT
         MVC   MONTH,DWORK+2            OUTPUT MONTH
         IC    R0,WORK+1                DAY
         BAL   R1,CVT                   EDIT
         MVC   DAY,DWORK+2              OUTPUT DAY
         MVC   YEAR,DYEAR+2             YEAR
*
         L     R14,DATASV               POP EM REGS
         BR    R14
*
CVT      EQU   *
         CVD   R0,PWORK                 CONVERT BINARY TO DECIMAL
         MVC   DWORK,MASKDATE           MASK
         ED    DWORK,PWORK+6            EDIT
         BR    R1
*
DATASV   DS    F
TIMER    EQU   18                       SVC FOR TIME AND DATE
WORK     DC    F'0'
PWORK    DC    D'0'
DWORK    DC    C' Z99'
MASKDATE DC    X'40212020'
DYEAR    DC    C' ZZZZZ'
*******************************************************
*    GETDATE - CALCULATE DATE USING TIME MACRO        *
*    ON ENTRY: R14 = RETURN ADDR                      *
*       DATE OF THE FORM: X'0CYYDDDF' IN R1           *
*       TIME OF THE FORM: X'HHMMSSTH' IN R0           *
*    ON EXIT: DATE = DD/MM/YYYY                       *
*             TIME = HH/MM/SS                         *
*******************************************************
GETDATEJ DS    0H
         ST    R14,SVDATE              PUSH RET ADDR
         STM   R0,R6,DATERR            PUSH
*        TIME  DEC,ERRET=NOTIME        ASK IBM THE TIME & DATE
         B     VALTIME                 GO COMPUTE IF VALID TIME
NOTIME   DS    0H
         MVC   DATE,=C'00/00/0000'     ERROR, USE FUNNY DATE
         MVC   TIME,=C'00:00:00'       ERROR, USE FUNNY TIME
         B     DATEX                   SKIP TIME COMPUTE
*
VALTIME  DS    0H
         ST    R1,DATETIM              MOVE RAW DATE
         XR    R2,R2                   USE IT UP
         IC    R2,DATETIM              GET PRODUCT OF MACRO TIME
         LTR   R2,R2                   IS IT 21ST CENTURY
         BZ    TWENTYTH                NO, WE'R STILL AT 20TH
         MVC   YEAR(2),=C'20'          MOVE 21ST'S CENTURY PREFIX
*
TWENTYTH DS    0H
         IC    R2,DATETIM+1            GET YEAR SUFIX
         LTR   R2,R2                   ARE WE AT YEAR XX00?
         BNZ   NOTYEAR0                NO, DO CALC FOR EVERY 4 YEARS
         LA    R3,JULIAN               GET DEFAULT TABLE ADDR
         B     DATEDEF                 SKIP 4TH YEAR CALC
*
NOTYEAR0 DS    0H
         SRDA  R2,32(0)                SCOOD R2 OVER TO R3
         LA    R4,4                    SET FOR DIV YEARS BY 4
         DR    R2,R4                   YEARS(R2-R3) / 4(R4)
         LA    R3,JULIAN               GET DEFAULT JUST IN CASE
         LTR   R2,R2                   ARE WE ON AN EVEN 4TH YEAR?
         BNZ   DATEDEF                 NO, USE DEFAULT TABLE
         LA    R3,JULIANB              YES, USE THE OTHER TABLE
*
DATEDEF  DS    0H
         XC    DOBLE,DOBLE             CLEAR DAYS BUFFER
         MVC   DOBLE+6(2),DATETIM+2    GET JULIAN DAYS
         CVB   R2,DOBLE                CONVERT DAYS TO BINARY
         XR    R4,R4                   CLEAR FOR INCERT CHAR
         LA    R5,11                   MONTHS
         LA    R6,1                    SET MONTH COUNTER TO 1
*
DATELP   DS    0H
         ICM   R4,B'0011',0(R3)        MONTHS TO DATE DAYS
         CR    R2,R4                   ARE WE AT THIS MONTH?
         BNH   THEMONTH                YES, THIS IS IT
         A     R6,=F'1'                BUMP MONTH COUNTER
         LA    R3,2(R3)                BUMP TABLE POINTER
         BCT   R5,DATELP               LOOP
*        LA    R2,365                  DEFAULT TO LAST YEAR'S DAY
*
THEMONTH DS    0H
         S     R3,=F'2'                DEBUMP TABLE POINTER
         ICM   R4,B'0011',0(R3)        GET PAST MONTH YTD DAYS
         SR    R2,R4                   GET ELAPSED DAYS THIS MTH
*
         CVD   R2,DOBLE                FIX
         UNPK  MASK(3),DOBLE+6(2)      DAYS
         MVZ   MASK+2(1),=X'F0'        TO
         MVC   DAY,MASK+1              PRINT
*
         CVD   R6,DOBLE                FIX
         UNPK  MASK(3),DOBLE+6(2)      MONTH
         MVZ   MASK+2(1),=X'F0'        TO
         MVC   MONTH,MASK+1            PRINT
         UNPK  MASK(7),DATETIM         GET WHOLE DATE
         MVC   YEAR+2(2),MASK+2        MOVE ONLY YEAR SUFIX
*
         ST    R0,DATETIM              MOVE RAW TIME
         LR    R6,R0                   GET TIME TO PLAY WITH
         SRL   R6,24                   ONLY MSB WORKS
         STC   R6,HOURADJ              PUT IN NUMERIC FIELD
         CP    HOURADJ,=PL2'120'       IS IT PAST NOON?
         BNH   HOUROK                  NO, NO NEED FOR ADJUST
         SP    HOURADJ,=PL2'120'       TAKE 12 OFF
         MVC   DATETIM(1),HOURADJ      MOVE RESULT
HOUROK   DS    0H
         MVC   MASK(10),=X'F020207A20207A202040'   AND
         ED    MASK(10),DATETIM        EDIT VALUE
         MVC   TIME,MASK+1             SYSTEM TIME
*
DATEX    DS    0H
         LM    R0,R6,DATERR            POP
         L     R14,SVDATE              POP RET ADDR
         BR    R14
*
HOURADJ  DC    PL2'0'                  HOUR ADJUST 24-12
***********************************************************************
***                                                                 ***
*** SUBRUTINAS PARA EL MANEJO DE CARACTERES DE CONTROL 3270         ***
***                                                                 ***
*** CONVIERTE UN HALFWORD BINARIO QUE REPRESENTA EL "OFFSET"        ***
*** RELATIVO A LA POSICION 0 DE UN BUFFER 3270 A SU EQUIVALENTE     ***
*** DIRECCION 3270 (DOS CARACTERES)                                 ***
***   ENTRADA: R1 -> DIRECCION DE LA MEDIA PALABRA                  ***
***   SALIDA : R15-> DIRECCION DE LOS DOS CARACTERES                ***
***   INVOCACION:   BAL R14,ADDR3270                                ***
***********************************************************************
ADDR3270 DS    0H
         LH    2,0(1)           TAKE BINARY BUFFER ADDRESS
         STC   2,1(15)          SAVE IN TWO CHAR ANSWER FIELD +1
         SRL   2,6              TAKE FIRST PART
         STC   2,0(15)          SAVE IN TWO CHAR ANSWER FIELD +0
         NI    1(15),X'3F'      REMOVE EXCESS BITS
         TR    0(2,15),ADDBYTES TRANSFORM
         BR    14               RETURN
ADDBYTES DS    0H
         DC    X'40C1C2C3C4C5C6C7C8C94A4B'
         DC    X'4C4D4E4F50D1D2D3D4D5D6D7'
         DC    X'D8D95A5B5C5D5E5F6061E2E3'
         DC    X'E4E5E6E7E8E96A6B6C6D6E6F'
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B'
         DC    X'7C7D7E7F'
*
***********************************************************************
*** OBTIENE LA DIRECCION DE BUFFER DADOS FILA, COLUMNA Y            ***
*** CANTIDAD DE COLUMNAS POR FILA.                                  ***
***   ENTRADA: R1 -> DIRECCION DE 4 BYTES CONTIGUOS ALINEADOS       ***
***                  |C|F|CF|  F:FILA; C:COLUMNA; CF:COLS/FILA      ***
***   SALIDA : R15-> DIRECCION DE BUFFER                            ***
***   INVOCACION:   BAL R14,BUFF327B                                ***
***********************************************************************
BUFF327B DS    0H
         SR    2,2               CLEAR WORK REG
         IC    2,0(1)            GET COLUMN
         BCTR  2,0               MINUS 1 FOR ADD
         SR    15,15             CLEAR WORK REG
         IC    15,1(1)           GET ROW
         BCTR  15,0              MINUS 1 FOR MULT
         MH    15,2(1)           TIMES NO OF COLS/ROW
         AR    15,2              RESULT IN R15
         BR    14                GOBACK
*
***********************************************************************
***                                                                 ***
*** COMBINES CALLS TO PREVIOUS SUB-ROUTINES IN ORDER TO OBTAIN      ***
*** BUFFER ADDRESS GIVEN ROW & COLUMN.                              ***
*** USAGE: BAL  R14,TWELVE                                          ***
***                                                                 ***
***********************************************************************
TWELVE   DS    0H
         ST    R14,SVTWE            PUSH RET ADDR
         STM   R1,R2,TWERR          PUSH REGS
*
         MVC   CHABUFAD(1),ROW      MOVE RAW ROW
         MVC   CHABUFAD+1(1),COL    MOVE RAW COLUMN
         B     TEMPEXIT             PC EXIT OF ROUTINE
*
         LA    R1,COL
         BAL   R14,BUFF327B         COMPUTE BINARY BUFFER ADDRESS
         STH   R15,BINBUFAD         SAVE IT
         LA    R1,BINBUFAD          PASS IT TO CONVERSION SUBROUTINE
         LA    R15,CHABUFAD         RESPONSE AREA
         BAL   R14,ADDR3270         GO GET IT
*
TEMPEXIT DS    0H
         LM    R1,R2,TWERR          POP REGS
         L     R14,SVTWE            POP RET ADDR
         BR    R14
*
*******************************************************
*    PRINT - PRINT A LINE & RESET CURSOR TO NEXT LINE *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              LINE = PRINT LN BUFFER WITH DATA       *
*    ON EXIT:  NOTHING                                *
*******************************************************
PRINT    DS    0H
         ST    R14,SVPRIN      PUSH RET ADDR
         STM   R0,R2,PRINTRR   PUSH
         L     R2,PRTDCB       GET DCB ADDRESS
         PUT   (R2),LINE       PRINT LINE
         L     R0,LINECNT              BUMP
         A     R0,=F'1'                 COUNTER
         ST    R0,LINECNT                BY 1
         MVC   LINE(L'LINE),LINE-1  CLEAR LINE BUFF
         LM    R0,R2,PRINTRR   POP
         L     R14,SVPRIN      POP RET ADDR
         BR    R14
*
*******************************************************
*                                                     *
*    CONSTANTS AND VARIABLES                          *
*                                                     *
*******************************************************
         LTORG
         DS    0D
COL      DS    X          SCREEN COLUMN
ROW      DS    X          SCREEN ROW
COLROW   DC    H'80'      CULUMS PER ROW
CURSOR   DS    1H         CURSOR ADDRESS
COMMLEN  DS    1H         OUTPUT AREA LENGTH
BINBUFAD DS    1H         BUFFER ADDRESS
CHABUFAD DS    1H         BUFFER ADDRESS (CHAR)
MAXROW   DS    1H         WINDOW MAX ROW POS
MAXCOL   DS    1H         WINDOW MAX COLUMN POS
SAVEPRO  DS    18F        MODULE REGS SAVE AREA
PRTDCB   DC    V(PRINTS)  PRINT DCB ADDRESS
         DC    C' '       ERASE CHAR
LINE     DC    CL132' '   PRINT LINE
LINECNT  DC    F'0'       LINE COUNTER
DOBLE    DS    D           CVD BUFF AREA
MSK      DC    XL16'40202020202020202020202020202020'
MASK     DC    XL16'40202020202020202020202020202020'
         DC    XL16'20202020202020202020202020202020'
DATETIM  DS    F               MACRO TIME OUTPUT WORD
DATE     DS    0CL10           DATE BUFFER
MONTH    DC    CL2'00'
         DC    C'/'
DAY      DC    CL2'00'
         DC    C'/'
YEAR     DC    CL4'1900'
TIME     DC    CL8'00:00:00'    TIME BUFF
DATE1    DC    CL10'00/00/0000'   GLOBAL DATE
TIME1    DC    CL8'00:00:00'      GLOBAL TIME
         DC    AL2(0)
JULIAN   DC    AL2(31,59,90,120,151,181,212,243,273,304,334,365)
         DC    AL2(0)
JULIANB  DC    AL2(31,60,91,121,152,182,213,244,274,305,335,366)
SAVEOUT  DS    18F                MODULE SAVE AREA
************** AREAS TO STACK REGS FOR SUBROUTINES
SVMOVE   DS    F               MOVEL PUSH RET ADDR
MOVERR   DS    4F              MOVEL PUSH REGS
SVDATE   DS    F               DATE PUSH RET ADDR
DATERR   DS    7F              DATE PUSH REGS
SVPRIN   DS    F               PRINT RET ADDR
PRINTRR  DS    3F              PRINT R0-R2 SAVE AREA
SVGEN    DS    F               GENSTR RET ADDR
GENRR    DS    8F              GENSTR R1-R8 SAVE AREA
SVTWE    DS    F               TWELVE RET ADDR
TWERR    DS    2F              TWELVE R1-R2 SAVE AREA
SVPUS    DS    F               PUSHST RET ADDR
PUSRR    DS    3F              PUSHST R1-R3 SAVE AREA
SVHOR    DS    F               DOHORIZ RET ADDR
HORRR    DS    8F              DOHORIZ R1-R8 SAVE AREA
SVVER    DS    F               DOVERT RET ADDR
VERRR    DS    8F              DOVERT R1-R8 SAVE AREA
SVBOX    DS    F               DOBOX RET ADDR
BOXRR    DS    8F              DOBOX R1-R8 SAVE AREA
SVBOXL   DS    F               DOBOXLIN RET ADDR
BOXRRL   DS    8F              DOBOXLIN R1-R8 SAVE AREA
SVLON    DS    F               LONGMV PUSH RET ADDR
LONRR    DS    8F              LONGMV PUSH REGS
**************************************************
         ENTRY PARMLIST
PARMLIST EQU   *
         DC    CL1'C'       PARAMETER FOR CLEAR SCREEN AND PRINT
CLSCOOR  DC    AL1(0,0,0,0) CLEAR COORDINATES
         DC    AL4(0)       PANEL INSTRUCTIONS ADDRESS
**************************************************************
PANT1    DS    0F              INITIAL SCREEN MAP
         DC    AL1(01,01),X'00',AL1(04) ROW/COL/ATT/LEN
         DC    X'3C174F40' DUP TO 23/79 X'40' CLEAR SCREEN
*        DC    X'3C5D7F00' ERASE TO 24/80 (R/C)
         DC    AL1(02,00),X'60',AL1(52) ROW/COL/ATT/LEN
         DC    CL04'DBG ',X'1DE8'
         ENTRY SCRSYSNM
SCRSYSNM EQU   *
         DC    CL46' '    SYSTEM DESCRIPTION FROM TREE LEVEL FILE
*        DC    AL1(02,06),X'60',AL1(36) ROW/COL/ATT/LEN
*        DC    CL36'PROCESS   REPORTS   FILES   SYSTEM  '
         DC    AL1(02,44),X'60',AL1(18) ROW/COL/ATT/LEN
         DC    CL18'PF1=HELP PF3=EXIT '
SCRDATIM DC    AL1(02,62),X'E8',AL1(19) ROW/COL/ATT/LEN
SCRDAT   DC    CL8'XX/XX/XX'
         DC    CL1' '
SCRTIM   DC    CL8'XX:XX:XX'
         DC    X'1D6000'       DELIMITER
*SCRLINE DC    AL1(03,00),X'E8',AL1(06),X'3C'
SCRLINE  DC    AL1(03,00),X'E4',AL1(06),X'3C'
SCRLINET DC    X'C36F',C'-',X'1D6000'
*        DC    AL1(03,00),X'E8',AL1(81) ROW/COL/ATT/LEN
*        DC    79C'-',X'1D6000'
*MAIN1    EQU   *
*DC X'02066009',C'PROCESS',X'1D60'
*DC X'02106009',C'REPORTS',X'1D60'
*DC X'021A6007',C'FILES',X'1D60'
*DC X'02226008',C'SYSTEM',X'1D6000'
*
*        DC    AL1(02,14),X'40',AL1(01) ROW/COL/ATT/LEN
*        DC    X'13'         SET CURSOR TO LAST FIELD
*        DC    AL1(02,24),X'40',AL1(00) ROW/COL/ATT/LEN
*        DC    AL1(02,32),X'40',AL1(00) ROW/COL/ATT/LEN
*        DC    AL1(02,41),X'40',AL1(00) ROW/COL/ATT/LEN
*        DC    X'00'           PANEL END INDICATOR
*
PANT2    DS    0F              UPDATE TITLE PART
         DC    AL1(02,62),X'E8',AL1(17) ROW/COL/ATT/LEN
SCRDAT1  DC    CL8'XX/XX/XX'
         DC    CL1' '
SCRTIM1  DC    CL8'XX:XX:XX'
         DC    AL1(02,17),X'40',AL1(01) ROW/COL/ATT/LEN
         DC    X'13'         SET CURSOR TO LAST FIELD
         DC    X'00'           PANEL END INDICATOR
*
**************************************************************
BOXCSAVE DC    CL8'+-+||+-+' BOX CHARACTERS SAVE AREA
BOXPSAVE DC    AL1(0,0,0,0)  BOX COORDINATE SAVE AREA
         ENTRY BOXPARML
BOXPARML EQU   *
         DC    AL1(0,0,0,0) BOX (TOP ROW/COL) (BOTTOM ROW/COL)
BOXCHARS DC    CL8'+-+||+-+' BOX HORIZ CORNER, CENTER, CORNE & SIDES
**************************************************************
SCRVERT  DC    AL1(00,00),X'00',AL1(01) R/C/A/L
         DC    CL1' ',X'00'             VERTICAL SIDES BOX CHARACTER
SCRHORZ  DC    AL1(00,00),X'00',AL1(00) R/C/A/L
         DC    CL80' ',X'00'            HORIZONTAL LINES BOX CHARACTER
*
**************************************************
         DC    C'HERE  OUTSTRN  COMM BUFF'
         DS    0D              ALIGN SCREENL
         ENTRY SCREENL
SCREENL  EQU   *
         DC    F'0'            OUTPUT STRING LENGTH
         ENTRY SCREEN
SCREEN   EQU   *
         DS    CL4096          SCREEN OUTPUT BUFFER
         ENTRY SCREENE
SCREENE  EQU   *               SCREEN OUTPUT BUFFER DELIMITER
         DC    C'HERE  OUTSTRN  COMM BUFF'
         COPY  \R42\CPY\EQUREGS.CPY
         END   OUTSTRN
