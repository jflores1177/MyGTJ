*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - DBG                                 *
*                 APPLICATION EXTERNAL ADDRESSES      *
*                 TABLE FOR FORWARD REFERENCE FOR     *
*                 ONE PASS LIKAGE EDITORS             *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - SEPTEMBER 1995                      *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: DBG                        *
*     OTHER ENTRY POINTS:                             *
*                                                     *
*     EXTERNAL REFERENCES:                            *
*                                                     *
*******************************************************
*        PRINT NOGEN
         TITLE '                                * * *  DBG       * * *'
**************************************************************
DBG      CSECT
         ENTRY
XX7      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XX7,R12,R11
         LA    R15,XX7SAV1     GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)      SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)      SAVE PROGRAMS SAVE AREA
         LR    R13,R15         GET PROGRAMS SAVE AREA ADDR
         L     R11,XX7SAV2     LOAD 2ND BASE REG
         B     AROXX7          SKIP SAVE AREA FLD
XX7SAV1  DS    18F             RETURN SAVE REGS AREA
XX7SAV2  DC    A(XX7+4096)     2ND BASE REG ADDR
         DC    CL12'<<DBG>>'   DUMP PROGRAM ID
AROXX7   DS    0H
*
         BR    R14
*
**************************************************************
* EXTERNAL REFERENCES ADDRESS TABLE
* NECESARY FOR FORWARD REFERENCING ONLY LINKAGE EDITORS
ADDRTAB  EQU   *
         DC    V(WIN)             0 - APPLICATION MAIN PROGRAM
         DC    V(APPLCURS)        1 - APPLICATION PARAMETERS ADDR
         DC    V(AREA1)           2 - INPUT BUFFER START
         DC    V(AREA1END)        3 - INPUT BUFFER END ADDR
         DC    V(BINOD)           4 - BINARY TREE PROGRAM ADDRESS
         DC    V(BOXPARML)        5 - BOX PARAMETER LIST ADDRESS
         DC    A(HELP)            6 - HELP PROCEDURE ADDRESS
         DC    V(INSCRN)          7 - COMMON WORK SCREEN ADDR
         DC    V(INSCRNL)         8 - COMMON WORK SCREEN LIMIT ADDR
         DC    V(MENUTAB)         9 - MENU TABLE ADDR
*         DC    V(PANEL)          10 - PANEL COBOL PROGRAM ADDRESS
         DC    V(PANLENG)        11 - COMMON OUTPUT AREA LENGTH
         DC    V(PARMLIST)       12 - OUTPUT STRING AREA ADDRESS LIST
         DC    V(PRINTS)         13 - PRINT DCB ADDRESS
         DC    V(SCREEN)         14 - SCREEN START ADDR
         DC    V(SCREENE)        15 - SCREEN DELIMITER ADDRES
         DC    V(SCREENL)        16 - SCREEN LENGTH ADDR - SPACE USED
         DC    V(SCRSYSNM)       17 - SYSTEM DESCR ADDR IN OUTSTR
         DC    V(SUBSNAM)        18 - WINDOWS SUB-SYSTEM NAME
         DC    V(SWDISCO)        19 - COMMON AREA DISCONNECT SWITCH
         DC    V(SYSTEMCD)       20 - WINDOWS SYSTEM NAME
         DC    V(TREELEV)        21 - WINDOWS TREE LEVEL FLD ADDR
         DC    V(TREETAB)        22 - TREE TABLE ADDRESS POINTER
         DC    V(USERFLDL)       23 - BUFFPRO TABLE LENGTH
         DC    V(USERFLDT)       24 - BUFFPRO FIELD TABLE ADDR
         DC    V(CICS)           25 - <<< THIS TABLE REFERENCE >>>
*
         TITLE 'HELP TEST PROGRAM'
*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - HELP                                *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - OCTOBER 1995                        *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: HELP                       *
*     OTHER ENTRIES:                                  *
*     EXTERNAL REFERENCES:                            *
*                                                     *
*******************************************************
*        PRINT NOGEN
*
*******************************************************
         DS    0D
         ENTRY HELP
HELP     DS    0H
XXI      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XXI,R12
         LA    R15,XXISAV1        GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)         SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)         SAVE PROG SAVE AREA IN CALLERS
         LR    R13,R15            GET PROGRAMS SAVE AREA ADDR
         B     AROXXI             SKIP SAVE AREA FLD
XXISAV1  DS    18F                RETURN SAVE REGS AREA
         DC    CL12'<<HELP>>'     DUMP PROGRAM ID
AROXXI   DS    0H
*         XFILO =C'CON:'
*         XFILI =C'CON:'
********************************************************************
*
         L     R10,0(0,R1)        PARAMETER ADDRS
*        L     R10,TESTPADD       PARAMETER ADDRS (TEST)
         USING INPARM,R10         MAP INPUT PARAMETERS
*
LINESERA DS    0H
         CLI   INPKEY,X'F3'        INPUT-KEY = PF3 = EXIT?
         BE    EXIMMED             YES, EXIT IMMEDIATE
*
         BAL   R14,CLEARSCR        CLEAR SCREEN AREA
         CLI   SWINIT,X'01'        INITIAL SWITCH ON
         BNE   SKIPINIT            NO, SKIP INITIALIZATION
         BAL   R14,INITIAL         INITIALIZE FILE, ETC
SKIPINIT DS    0H
         BAL   R14,MOVETEXT        MOVE NEXT HELP TEXT
*
**********************************************************************
         CLI   SWMOVE,X'01'        MOVE SWITCH ON?
         BE    SHOWMORE            YES, SHOW SCREEN
         MVC   PROSWI,=C'END'      SHOW AND EXIT
         MVI   SWINIT,X'01'        INITIAL SWITCH ON
         B     SENDSCR             GO TO SEND SCREEN
SHOWMORE DS    0H
         MVC   PROSWI,=C'   '      SHOW SCREEN AND RETURN
         B     SENDSCR             GO TO SEND SCREEN
*
EXIMMED  DS    0H
         CLOSE HELPT               CLOSE FILE
         MVC   PROSWI,=C'EXT'      EXIT IMMEDIATELY (EXT)
         MVI   SWINIT,X'01'        INITIAL SWITCH ON
         L     R8,SCREENA          POINT TO COMMON OUTPUT AREA LENGTH
         L     R15,=V(PANLENG)     POINT TO COMMON OUTPUT AREA LENGTH
         MVC   0(4,R8),0(R15)      RESTORE PANEL FORMER LENGTH
         XC    0(4,R15),0(R15)     RESET PANEL RESTORE LENGTH
         B     EXITROUT            GO TO EXIT ROUTINE
*
SENDSCR  DS   0H
*        XDUMP SCRAREA,2000
*        XPRNT SCRAREA+479,1360    PRINT WHOLE SCREEN 17 LINES
*        XREAD INP,1               TEST READ
         L     R8,SCREENA          POINT TO COMMON OUTPUT AREA LENGTH
         L     R15,=V(PANLENG)     POINT TO COMMON OUTPUT AREA LENGTH
         MVC   0(4,R8),0(R15)      RESTORE PANEL FORMER LENGTH
*        XC    0(4,R15),0(R15)     RESET PANEL RESTORE LENGTH
         CALL  CICS,SCRAREA,SCRAREAE,OUTPAR
*
EXITROUT DS   0H
*        CLI   SWINIT,X'01'        IS INITIAL SWITCH ON?
*        BNE   LINESERA            DO SOME MORE HELP
         EXIT SAVE=YES,RC=15
*
EOF      DS   0H
         CLOSE HELPT               CLOSE FILE
         MVI   SWMOVE,X'00'        TURN SWITCH MOVE OFF
         CLI   SWINIT,X'01'        IS INITIAL SWITCH STIL ON?
         BNE   JUSTEXIT            NO, JUST DO AN EXIT
         LA    R1,1840     80X24   BOTTOM SCREEN MESSAGE OFFSET
         LA    R1,SCRAREA(R1)      PLUS SCREEN AREA ADDR
         MVC   30(22,R1),NOHELP    NO HELP FOR ITEM MSSG
         MVC   PROSWI,=C'END'      SHOW AND EXIT
         B     SENDSCR             GO TO SEND SCREEN
*
JUSTEXIT DS    0H
         MVC   PROSWI,=C'END'      SHOW LAST SCREEN AND EXIT
         B     SENDSCR             GO TO SEND SCREEN
*
*******************************************************
*     MOVETEXT: MOVE TEXT TO SCREEN AREA              *
*     ON ENTRY:                                       *
*     ON EXIT:                                        *
*******************************************************
MOVETEXT DS    0H
         ST    R14,MOVSV           PUSH RET ADDR
         STM   R1,R4,MOVRR         PUSH REGS
*
         L     R1,STARTADD         BUFFER START LOCATION
         LA    R1,SCRAREA(R1)      PLUS BUFFER ADDDRESS
         L     R2,POSCNT           LENGTH OF DATA TO MOVE
         BCTR  R2,0                ADJUST FOR EXECUTE
         L     R3,LINCNT           NUMBER OF LINES TO MOVE
*
         LR    R4,R2               COPY FOR COMPUTE
         SH    R4,=H'6'            LENGTH OF [ HELP ]
         SRL   R4,1                DIVIDE BY 2
         LA    R4,0(R4,R1)         BUFFER ADDRESS
         MVC   0(6,R4),HELPBXT     MOVE TITLE BY HAND
         LA    R1,80(0,R1)         BUMP OUTPUT POINTER
         MVI   0(R1),X'15'         TITLE ATTRIBUTE (E8)
         MVC   1(30,R1),PTITLE     MOVE TITLE BY HAND
         LA    R4,0(R2,R1)         LAST LINE POSITION
         MVI   0(R4),X'11'         TITLE ATTRIBUTE (60)
         LA    R1,80(0,R1)         BUMP OUTPUT POINTER
*        BCTR  R3,0                ADJUST FOR 1 TITLE MOVE
*        BCTR  R3,0                ADJUST FOR 1 TITLE MOVE
         BCTR  R3,0                ADJUST FOR 1 TITLE MOVE
         BCTR  R3,0                ADJUST FOR 1 TITLE MOVE
         L     R4,MESSAGEA         MESSAGE SCREEN LINE OFFS
         MVC   30(22,R4),EXITMORE  CONTINUATION MESSAGE
MOVELP   DS    0H
         BAL   R14,READ            READ HELP TEXT FILE
         CLI   WCOMMD,C'@'         ANOTHER HELP COMMAND?
         BE    MOVEMSSG            YES, GO CHANGE MESSAGE
         MVI   0(R1),X'11'         INITIAL LINE ATTR (60)
         EX    R2,MOVEHELP         MOVE HELP TEXT LINE
         LA    R4,0(R2,R1)         LAST WINDOW COLUMN
         MVI   0(R4),X'11'         FINAL LINE ATTR (60)
         LA    R1,80(0,R1)         BUMP OUTPUT POINTER
         CLC   WDELIM,=CL8'99999999' TEXT DELIMITER?
         BE    MOVEMSSG            YES, GO CHANGE MESSAGE
         BCT   R3,MOVELP           LOOP
*
MOVEEX   DS    0H
         LM    R1,R4,MOVRR         POP REGS
         L     R14,MOVSV           POP RET ADDR
         BR    R14
*
MOVEMSSG DS    0H
         CLOSE HELPT               CLOSE FILE
         L     R4,MESSAGEA         MESSAGE SCREEN LINE OFFS
         MVC   30(22,R4),PRESSENT  PRESS ENTER MSSG
         MVI   SWMOVE,X'00'        TURN SWITCH MOVE OFF
         B     MOVEEX              EXIT
*
MOVEHELP MVC   1(*-*,R1),HELPRC    MOVE FROM INPUT RECORD
MOVSV    DS    F                   PUSH RET ADDR
MOVRR    DS    4F                  PUSH REGS
*******************************************************
*     INITIAL: FIRST TURN ON THE PROGRAM ROUTINE      *
*     ON ENTRY:                                       *
*     ON EXIT:                                        *
*******************************************************
INITIAL  DS    0H
         ST    R14,INISV           PUSH RET ADDR
         STM   R1,R3,INIRR         PUSH REGS
*
*      SAVE ORIGINAL SCREEN POINTER
         L     R1,SCREENA          POINT TO COMMON OUTPUT AREA LENGTH
         L     R15,=V(PANLENG)     POINT TO COMMON OUTPUT AREA LENGTH
         MVC   0(4,R15),0(R1)      SAVE PANEL LENGTH FOR AFTER SEND
*
*      MEANS MOVE PROCESS NOT IN PROGRESS
         MVI   SWMOVE,X'00'        TURN SWITCH MOVE OFF
         MVI   SWINIT,X'01'        TURN INITIAL SWITCH ON
*
         OPEN  HELPT
         OI    HELPT+10,X'08'      TURN TRASLATE ASCII TO EBDCIC
INITLP   DS    0H
         BAL   R14,READ            READ HELP TEXT FILE
         CLI   WCOMMD,C'@'         HELP COMMAND?
         BNE   INITLP              NO, READ ANOTHER REC
         CLC   WSYSTEM,SYSCOD      SAME SYSTEM?
         BNE   INITLP              NO, READ ANOTHER REC
         CLC   WLEVEL,LEVCOD       SAME LEVEL?
         BNE   INITLP              NO, READ ANOTHER REC
*
*      MEANS MOVE PROCESS IN PROGRESS
         MVI   SWMOVE,X'01'        TURN SWITCH MOVE ON
*
*      VALIDATE CONVERT BOX COORDINATES FOR NUMERIC VALUES
         MVC   WINPCOO,WTROW       TOP ROW
         MVC   WINPCOO+2(2),WTLCOL TOP LEFT COLUMN
         MVC   WINPCOO+4(2),WBROW  BOTTOM ROW
         MVC   WINPCOO+6(2),WBRCOL BOTTOM RIGHT COLUMN
         MVC   WCOMP,=C'00000000'  SET COMPARAND FIELD
         MVN   WCOMP,WINPCOO       MOVE NUMERICS ONLY
         CLC   WCOMP,WINPCOO       COMPARAND SAME AS COORS?
         BE    INICOOK             YES, NUMERIC COORDS
         MVC   WTROW,=C'06'        DEFAULT TOP ROW
         MVC   WTLCOL,=C'03'       DEFAULT TOP LEFT COLUMN
         MVC   WBROW,=C'22'        DEFAULT BOTTOM ROW
         MVC   WBRCOL,=C'77'       DEFAULT BOTTOM RIGHT COLUMN
         MVC   WINPCOO,=C'06032277'  SEND DEFAULT COORDS
*
*      CONVERT BOX COORDINATES TO BINARY
INICOOK  DS    0H
         MVC   BOXCOO,WINPCOO      SEND INPUT COORDS
         LA    R2,WTROW            TOP ROW
         LA    R3,WTROW1           BINARY TOP ROW
         LA    R1,2                TWO BYTES LONG
         BAL   R14,SHRINK          CONVERT TO BINARY
         LA    R2,WTLCOL           TOP LEFT COL
         LA    R3,WTLCOL1          BINARY TOP LEFT COL
         LA    R1,2                TWO BYTES LONG
         BAL   R14,SHRINK          CONVERT TO BINARY
         LA    R2,WBROW            BOTTOM ROW
         LA    R3,WBROW1           BINARY BOTTOM ROW
         LA    R1,2                TWO BYTES LONG
         BAL   R14,SHRINK          CONVERT TO BINARY
         LA    R2,WBRCOL           BOTTOM RIGHT COL
         LA    R3,WBRCOL1          BINARY BOTTOM RIGHT COL
         LA    R1,2                TWO BYTES LONG
         BAL   R14,SHRINK          CONVERT TO BINARY
*
*      GET BOX CHARACTERS
         CLC   WBOXCHR,=CL8' '     FRAME CHARACTERS?
         BE    ININOFRM            NO, USE DEFAULT FRAME CHRS
         MVC   BOXCHR,WBOXCHR      YES, USE THEM
         B     INITNEXT            EXIT
ININOFRM DS    0H
         MVC   BOXCHR,=C'---||---' DEFAULT BOX CHARS
*
INITNEXT DS    0H
*
*      GET BUFFER START RELATIVE ADDRESS
         L     R1,WTLCOL1          TOP LEFT COL
         LA    R1,2(0,R1)          BUMP TO SKIP 2 POS
         STC   R1,COLROW           SET FOR OFFSET CONVERTION
         L     R1,WTROW1           TOP ROW
         LA    R1,1(0,R1)          BUMP 1 TO FIT & 1 FOR TITLE
         STC   R1,COLROW+1         SET FOR OFFSET CONVERTION
         LA    R1,COLROW           R/C FIELD ADDRESS
         BAL   R14,BUFF327B        GET BUFFER OFFSET
         ST    R15,STARTADD        SAVE BUFFER START ADDR
*
*      GET LINE COUNT TO PRINT ON THE SCREEN
         L     R1,WBROW1           BOTTOM ROW
         S     R1,WTROW1           MINUS TOP ROW
*        BCTR  R1,0                MINUS 1
         ST    R1,LINCNT           SAVE LINE COUNT
*
*      GET COLUMN COUNT TO PRINT ON THE SCREEN
         L     R1,WBRCOL1          BOTTOM RIGHT COLUMN
         S     R1,WTLCOL1          MINUS TOP LEFT COLUMN
*        BCTR  R1,0                MINUS 1
         BCTR  R1,0                MINUS 1 LEFT
         BCTR  R1,0                MINUS 1 RIGHT
         ST    R1,POSCNT           SAVE LINE COUNT
*
*      GET MESSAGE SCREEN LOCATION
         LA    R1,1840     80X24   BOTTOM SCREEN MESSAGE OFFSET
         LA    R1,SCRAREA(R1)      PLUS SCREEN AREA ADDR
         ST    R1,MESSAGEA         MESSAGE SCREEN LINE OFFS
         MVI   SWINIT,X'00'        TURN INITIAL SWITCH OFF
*
INITEX   DS    0H
         LM    R1,R3,INIRR         POP REGS
         L     R14,INISV           POP RET ADDR
         BR    R14
*
INISV    DS    F                   PUSH RET ADDR
INIRR    DS    3F                  PUSH REGS
*******************************************************
*     CLEARSCR: ERASE SCREEN BUFFER AREA              *
*******************************************************
CLEARSCR DS    0H
         LA    R1,SCRAREA          SCREEN AREA
*
CLEALP   DS    0H
         C     R1,=A(SCRAREAE)     END?
         BH    CLEAREX             YES, EXIT
         MVI   0(R1),C' '          MOVE SPACE
         LA    R1,1(0,R1)          BUMP ADDRESS
         B     CLEALP              LOOP
*
CLEAREX  DS    0H
         BR    R14
*
*******************************************************
*     READ: READ RECORD SUB-ROUTINE                   *
*******************************************************
READ     DS    0H
         ST    R14,REASV           PUSH RET ADDR
         STM   R1,R4,REARR         PUSH REGS
*
READNEXT DS    0H
         GET   HELPT,HELPRC        READ A RECORD
         L     R1,INRECC           GET PRIOR INPUT COUNT
         LA    R1,1(0,R1)          BUMP COUNT
         ST    R1,INRECC           UPDATE IT
*
         LM    R1,R4,REARR         POP REGS
         L     R14,REASV           POP RET ADDR
         BR    R14
*
REARR    DS    4F                  POP REGS
REASV    DS    F                   POP RET ADDR
*******************************************************
*     SHRINK: CONVERTS TEXT NUMBERS TO A BINARY WORD  *
*     ON ENTRY: R2 = INPUT FIELD                      *
*               R3 = OUTPUT FIELD                     *
*               R1 = INPUT FIELD LENGTH               *
*     ON EXIT:  WORD CONTAINS BINARY VALUE            *
*******************************************************
SHRINK   DS    0H
         ST    R14,SHRSV           PUSH RET ADDR
         STM   R1,R5,SHRRR         PUSH REGS
         LA    R5,0                RESET RESULT
         ST    R5,0(0,R3)          AND FIELD
*
         LTR   R1,R1               LENGTH RANGE OK?
         BZ    SHREXIT             NO, EXIT
         CH    R1,=H'12'           LENGTH RANGE OK?
         BNH   SHRANGOK            YES, GO ON
         LA    R1,12               MAXIMUM LENGTH
SHRANGOK DS    0H
         BCTR  R1,0                ADJUST -1 FOR EXECUTE
         EX    R1,SHRZONE          RESET ALL ZONES TO 0
         LA    R1,1(0,R1)          RESTORE LIKE BEFORE EX
         SR    R4,R4               FIX FOR IC
*
SHRINKLP DS    0H
         IC    R4,0(R2)            GET 1ST NUM CHAR
         CH    R4,=H'9'            NUMBER RANGE OK?
         BNH   SHRNUMOK            YES, GO ON
         LA    R4,9                HIGHEST VALUE
SHRNUMOK DS    0H
         MH    R5,=H'10'           MULTIPLY BY 10
         AR    R5,R4               ADD TO TOTAL
         LA    R2,1(0,R2)          POINT TO PRIOR BYTE
         BCT   R1,SHRINKLP         DO LOOP R1 TIMES
         ST    R5,0(0,R3)          OUTPUT RESULT
*
SHREXIT  DS    0H
         LM    R1,R5,SHRRR         POP REGS
         L     R14,SHRSV           POP RET ADDR
         BR    R14
*
SHRZONE  NC    0(*-*,R2),=X'0F0F0F0F0F0F0F0F0F0F0F0F' ZONES TO 0
SHRRR    DS    5F
SHRSV    DS    F
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
******** GENERAL VARS ****************************************
INP      DC    H'0'            INPUT TEST FIELD
INRECC   DC    F'0'            RECORD COUNT
COLROW   DS    H               CONVERTION TO BUFF OFFSET
         DC    H'80'           COLUMNS PER ROW
MESSAGEA DS    F               MESSAGE SCREEN LINE OFFSET
STARTADD DS    F               BUFFER START ADDR
LINCNT   DS    F               SCREEN LINE COUNT
POSCNT   DS    F               SCREEN POSITION COUNT
SWINIT   DC    X'01'           INITIAL SWITCH INITIALLY ON
SWMOVE   DC    X'01'           MOVE SWITCH INITIALLY ON
WTROW1   DS    F               TOP ROW
WTLCOL1  DS    F               TOP LEFT COLUMN
WBROW1   DS    F               BOTTOM ROW
WBRCOL1  DS    F               BOTTOM RIGHT COLUMN
WINPCOO  DS    CL8             INPUT COORDINATES
WCOMP    DS    CL8             NUMERIC COMPARAND FIELD
         DS    0D
EXITMORE DC    X'15',C'PF3=EXIT  ENTER=MORE',X'11'
NOHELP   DC    C'  ',X'15',C'NO HELP FOR ITEM',X'11',C'  '
HELPBXT  DC    X'15',C'HELP',X'11'
PRESSENT DC    C'    ',X'15',C'PRESS ENTER',X'11',C'     '
******** DCBS ************************************************
HELPT    DCB   LRECL=82,RECFM=F,MACRF=G,EODAD=EOF,
               DDNAME='HELPT.DAT',BLKSZ=82
**************************************************************
HELPRC   DS    0CL82
WCOMMD   DS    C               HELP TEXT COMMAND LINE
         DS    C               COMMA FILLER
WSYSTEM  DS    CL8             SYSTEM NAME
         DS    C               COMMA FILLER
WLEVEL   DS    CL20            LEVEL ITEM CODE
         DS    C               COMMA FILLER
WSUBPR   DS    CL8             SUB PROCESS
         DS    C               COMMA FILLER
WTROW    DS    CL2             TOP ROW
         DS    C               COMMA FILLER
WTLCOL   DS    CL2             TOP LEFT COLUMN
         DS    C               COMMA FILLER
WBROW    DS    CL2             BOTTOM ROW
         DS    C               COMMA FILLER
WBRCOL   DS    CL2             BOTTOM RIGHT COLUMN
         DS    C               COMMA FILLER
WBOXCHR  DS    CL8             TLC-TL-TRC-LL-RL-BLC-BL-BRC  BOX
         DS    CL11            FILLER
WDELIM   DS    CL8             TEXT DELIMITER CL8'99999999'
         DS    CL2             CR|LF OF LINE
*****************************************************************
*                ATTRIBUTES TABLE EQUIVALENCE                   *
*  HEX:   1  .  .  4  .  .  7  .  .  A  .  .  D  .  . 10        *
*  DEC:   1  .  .  .  5  .  .  .  . 10  .  .  .  . 15  .        *
*        40 C1 C4 C5 C8 C9 4C 4D 50 D1 D4 D5 D8 D9 5C 5D        *
*  HEX:  11  .  . 14  .  . 17  .  . 1A  .  . 1D  .  . 20        *
*  DEC:   .  .  . 20  .  .  .  . 25  .  .  .  . 30  .  .        *
*        60 61 E4 E5 E8 E9 6C 6D F0 F1 F4 F5 F8 F9 7C 7D        *
*****************************************************************
*   THIS AREA MUST BE PASSED TO THE SCREEN BUFFER PROCESOR
*   THIS IS WHERE YOU DEFINE THE SCREEN LINES
*****************************************************************
SCRAREA  DS    0CL1920
SL00     DC    CL80' '
SL01     DC    CL80' '
SL02     DC    CL80' '
SL03     DC    CL80' '
SL04     DC    CL80' '
SL05     DC    CL80' '
SL06     DC    CL80' '
SL07     DC    CL80' '
SL08     DC    CL80' '
SL09     DC    CL80' '
SL10     DC    CL80' '
SL11     DC    CL80' '
SL12     DC    CL80' '
SL13     DC    CL80' '
SL14     DC    CL80' '
SL15     DC    CL80' '
SL16     DC    CL80' '
SL17     DC    CL80' '
SL18     DC    CL80' '
SL19     DC    CL80' '
SL20     DC    CL80' '
SL21     DC    CL80' '
SL22     DC    CL80' '
SL23     DC    CL80' '
SCRAREAE DC    X'0000'
*****************************************************************
*   THIS ARE PARAMETERS TO BE PASSED TO THE SCREEN BUFFER PROC
*****************************************************************
OUTPAR   EQU   *           01  OUTPUT-PARAMETERS.
*                          A 'C' = CLEAR, BLANK = DON'T CLEAR
CLS      DC    C' '          05  CLEAR-SCREEN    PIC X(01) VALUE ' '.
CLSCOOR  DC    CL8'00000000' 05  CLEAR-COORDI  PIC X(08) VALUE '0'.
*                          THIS IS THE CURSOR (DEFALUT 1/1)
INSCUR   EQU   *             05  CURSOR-INCERT.
INSROW   DC    C'01'           10  INCERT-ROW  PIC X(02) VALUE '01'.
INSCOL   DC    C'01'           10  INCERT-COL  PIC X(02) VALUE '01'.
*                          THIS ARE THE BOX COORDINATES
BOXCOO   DS    0CL8          05  BOX-COORDINATES.
BTROW    DC    C'06'           10  BOX-TROW    PIC X(02) VALUE '06'.
BTLCOL   DC    C'03'           10  BOX-TLCOL   PIC X(02) VALUE '03'.
BBROW    DC    C'22'           10  BOX-BROW    PIC X(02) VALUE '22'.
BBRCOL   DC    C'77'           10  BOX-BRCOL   PIC X(02) VALUE '77'.
BOXCHR   DC    C'---||---'   05  BOX-CHARS PIC X(08) VALUE '+-+||+-+'.
RESOB    DC    X'FF'         05  RESET-OUT-BUFF  PIC X(01) VALUE ' '.
*
SCREENA  DC    V(SCREENL)   COMMON OUTPUT STRING AREA ADDRESS
******************************************************
*TESTPADD DC    A(TESTPARM)        TEST DATA
*TESTPARM EQU   *
*         DC    CL8'INITIAL '
*         DC    CL20'01010100000000000000'
*         DC    CL8' '
*         DC    CL62'TITULO DE PRUEBA:'
*         DC    C'E'
*         DC    CL3' '
*         DC    CL2'01'
*         DC    CL2'01'
         LTORG
*****************************************************************
*   LINKAGE SECTION.
*   THIS ARE THE PARAMETERS FROM THE CALLING PROGRAM (DBG)
*****************************************************************
INPARM   DSECT           01  INPUT-PARAMETERS.
*                          THE KEY TO LOOK UP IN THE HELP FILE
IDKEY    DS    0CL36       05 IDENTIFICATION-KEY.
SYSCOD   DS    CL8            10  SYSTEM-CODE          PIC X(08).
LEVCOD   DS    CL20           10  LEVEL-CODE           PIC X(20).
PROCOD   DS    CL8            10  PROCEDURE-CODE       PIC X(08).
*                          SENT BACK FROM DBG WINDOW TREE
PTITLE   DS    CL62        05  PROCESS-TITLE            PIC X(62).
*                          LOOKIT UP IN THE KEY TABLE
INPKEY   DS    C           05  INPUT-KEY                PIC X(01).
*                          THIS ARE THE SERVICE IMPLEMENTED:
*                          '   ' = 3 BLANKS = RETURN HERE AFTER
*                          'END' = PRINT LAST SCREEN & STAY THERE
*                          'EXT' = EXIT IMMEDIATELY & STAY THERE
PROSWI   DS    CL3         05  PROCESSING-SWITCH        PIC X(03).
*                          THIS IS WHERE THE CURSOR BEFORE
INPCURL  DS    0CL4        05  INPUT-CURSOR-LOCATION.
INCUROW  DS    CL2           10  INPUT-CURSOR-ROW        PIC X(02).
INCUCOL  DS    CL2           10  INPUT-CURSOR-COLUMN     PIC X(02).
*****************************************************************
         COPY  \R42\CPY\EQUREGS.CPY
         END   DBG
