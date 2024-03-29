*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - BUFFPROI                            *
*                 SCREEN BUFFER CONVERTION FROM 3270  *
*                 TO DBG STANDARD                     *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - SEPTEMBER 1995                      *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: BUFFPROI                   *
*     OTHER ENTRIES:                                  *
*                                                     *
*     EXTERNAL REFERENCES:                            *
*        APPLCURS = APPLICATION'S CURSOR LOCATION     *
*        AREA1    = RECEIVE DATA AREA                 *
*        AREA1END = RECEIVE DATA AREA LIMIT           *
*        USERFLDT = 512 USER FIELD TABLE (4096 BYTES) *
*        USERFLDL = USER FIELD TABLE DELIMITER        *
*                                                     *
*     PARAMETERS:                                     *
*        CALLBUFN = CALLING APPLICATION PANEL START   *
*        CALLBUFL = CALLING APPLICATION PANEL END     *
*        PROCEDUR = APPLICATION ENTRY PIONT           *
*                                                     *
*******************************************************
*        ENTRY
*        EXTRN APPLCURS,AREA1,AREA1END
*        EXTRN USERFLDT,USERFLDL
*
*        PRINT NOGEN
         TITLE '                              * * *   BUFFPROI  * * *'
**************************************************************
*        BUFFPROI ROUTINE
**************************************************************
BUFFPROI CSECT
         ENTRY
XX2      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XX2,R12
         LA    R15,XX2SAV1     GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)      SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)      SAVE PROGRAMS SAVE AREA IN CALLERS AREA
         LR    R13,R15         GET PROGRAMS SAVE AREA ADDR
         B     AROUND          SKIP SAVE AREA FLD
XX2SAV1  DS    18F             RETURN SAVE REGS AREA
         DC    CL12'<<BUFFPROI>>'  DUMP PROGRAM ID
AROUND   DS    0H
*
         L     R11,=V(AREA1)        CALLING OUTPUT BUFFER START
         USING INPSTR,R11
*
         XC    RETCODE,RETCODE      PROGRAM RETURN CODE
*         XDUMP 0(R1),100
         L     R2,0(R1)             CALLING OUTPUT BUFFER START
*         XDUMP 0(R2),100
         L     R2,0(R2)             CALLING OUTPUT BUFFER START
*         XDUMP 0(R2),100
         ST    R2,CALLBUFN          CALLING OUTPUT BUFFER START
         A     R2,=F'1920'          CALLING OUTPUT BUFFER END
         L     R2,4(R1)             CALLING OUTPUT BUFFER END
         L     R2,0(R2)             CALLING OUTPUT BUFFER END
*        ST    R2,CALLBUFL          CALLING OUTPUT BUFFER END
*        MVI   0(R2),X'00'          SET DELIMITER
*
SCRESLP  DS    0H
         BAL   R14,SCANFLD         SCAN VTAM OUTPUT & BUILD APPL INP
*
SCREEX   DS    0H
         LA    R1,CURLOCT          CURSOR BUFFER ADDR TO CONVERT ROUT
         BAL   R14,BUFF327B        GET BUFFER OFFSET ADDR FROM 3270 BA
         STH   R15,BOFFSET         SAVE OUTPUT BUFFER ADDR LOC
         LA    R1,BOFFSET          GET OUTPUT BUFFER ADDR LOC
         BAL   R14,ROWCOL          SET CURSOR ADDR FROM SBA
         L     R1,=V(APPLCURS)     GET APPLICATION PARAMETERS ADDR
         MVC   0(6,R1),FLDROWCH    MOVE ROW/COL, ROW/COL (2 SIZES)
*
*        L     R15,RETCODE         PROGRAM RETURN CODE
         EXIT  SAVE=YES,RC=RETCODE
*
*******************************************************
*     SCANFLD:  SCANS INPUT USER FIELDS               *
*     ON ENTRY: SAME REGS AS LFTSCN                   *
*               R4 = START LOCATION ADDRESS           *
*     ON EXIT:  R15 = 0 = NO TEXT FOUND / END ADDRESS *
*               R0 = STRING START ADDRESS             *
*******************************************************
SCANFLD  DS    0H
         ST    R14,SVSCC           PUSH RET ADDR
         STM   R1,R9,SCCRR         PUSH REGS
         CLI   ORDERCD,X'11'       ANY ORDERS BACK FROM VTAM?
         BNE   SCCNEX              NO, EXIT
         LA    R4,ORDERCD          1ST POSSIBLE INPUT ORDER CODE
         LA    R9,960              MAXIMUM INPUT FIELDS (80X24)
         MVC   INPSBA1(2),1(R4)     SAVE BUFFER ADDR (BA)
*
SCCLP    DS    0H
*        CLI   0(R4),X'00'         IS THIS VTAM INPUT END?
*        BE    SCCNEX              YES, EXIT
         LA    R2,1                VALUE LENGTH
         LA    R3,=X'12'           VALUE ADDREESS (SBA)
         L     R7,=V(AREA1END)     INPUT BUFFER END ADDR
         LR    R5,R4               SAVE STR START ADDR
         BAL   R14,LFTSCN          SEARCH FOR A (SBA) < X'12'
         LR    R10,R15             GET LFTSCN ROUTINE ANSWER
         LR    R4,R0               GET VALUE ADDRESS
         LR    R6,R4               COPY VALUE POINTER
         SR    R6,R5               CALCULATE PARM LENGTH
*
SCCNON   DS    0H
         MVC   INPSBA(2),1(R4)     SAVE BUFFER ADDR (BA)
         LA    R1,INPSBA           PASS BUFFER ADDR TO CONVERTION ROUT
         BAL   R14,BUFF327B        GET BUFFER OFFSET ADDR FROM 3270 BA
         L     R8,CALLBUFN         OUTPUT BUFFER START ADDR
         LA    R8,0(R8,R15)        OUTPUT BUFFER ADDR LOC
         LA    R2,0(R8,R6)         GET FIELD REACH
         BAL   R14,ERASEMAP        GO ERASE MAP TO NEXT ATTRIBUTE
         LA    R1,INPSBA1          PASS BUFFER ADDR TO UPDATE ROUT
         BAL   R14,UPDATEFL        ERASE USER FIELD WITH MAP DATA
         CH    R6,=H'0'            IS IT A DOBBLE FINDING? **ERROR**
         BNH   SCCNOMV1            YES, DON'T MOVE ANYTHING
         CLC   INPSBA1(2),=X'0000' ANY PRIOR FIELD TO MOVE?
         BE    SCCNOMV1            NO, DON'T MOVE ANYTHING
         LA    R1,INPSBA1          PASS BUFFER ADDR TO CONVERTION ROUT
         BAL   R14,BUFF327B        GET BUFFER OFFSET ADDR FROM 3270 BA
         L     R8,CALLBUFN         OUTPUT BUFFER START ADDR
         LA    R8,0(R8,R15)        OUTPUT BUFFER ADDR LOC
         LA    R2,0(R8,R6)         GET FIELD REACH
         C     R2,CALLBUFL         OUTPUT BUFFER END TRESSPASSED?
         BNH   SCCNOMV2            NO, SKIP ERROR SETTING
         MVC   RETCODE,=F'1'       SET OUT SIDE BUFFER AREA MOVE ERR
         B     SCCNOMV1            & SKIP MOVE
SCCNOMV2 DS    0H
         BAL   R14,LONGMV          UPDATE APPLICATION SCREEN AREA
         LA    R1,INPSBA1          PASS BUFFER ADDR TO UPDATE ROUT
         BAL   R14,UPDATEFL        UPDATE USER FIELD WITH MAP DATA
*
SCCNOMV1 DS    0H
         MVC   INPSBA1(2),INPSBA   SAVE SBA FOR NEXT TIME ARROWND
         CH    R10,=H'1'           BUFFER END REACHED?
         BE    SCCNEX              YES, EXIT SCAN
         C     R4,=V(AREA1END)     INPUT BUFFER END REACHED?
         BNL   SCCNEX              YES, EXIT SCAN
         CLI   0(R4),X'00'         IS THIS VTAM INPUT END?
         BE    SCCNEX              YES, EXIT
         LA    R4,3(R4)            BUMP INPUT POINTER
         BCT   R9,SCCLP            ROLL IT FOR MAX INPUT TIMES
*
SCCNEX   DS    0H
         LM    R1,R9,SCCRR         POP REGS
         L     R14,SVSCC           POP RET ADDR
         BR    R14
*
*******************************************************
*    UPDATEFL  UPDATE FIELD USING BUFFPRO FIELD TAB   *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R1  = INPUT 3270 BUFFER ADDRESS        *
*    ON EXIT:  FIELD UPDATED USING LEGTH IN TABLE     *
*******************************************************
UPDATEFL DS    0H
         ST    R14,SVUDF           PUSH RET ADDR
         STM   R1,R9,UDFRR         PUSH
         LR    R9,R5               SAVE FOR EXECUTE
*
******** LA    R1,INPSBA1          INPUT BUFFER ADDR
         L     R4,=V(USERFLDT)     BUFFPRO FIELD TABLE ADDR
UPDAFTLP DS    0H
         CLI   0(R4),X'00'         END OF TABLE?
         BE    UPDAEX              YES, SEARCH NO MORE
         C     R4,=V(USERFLDL)     END OF TABLE?
         BH    UPDAEX              YES, SEARCH NO MORE
         CLC   0(2,R4),0(R1)       OUR 3270 BUFFER ADDRESS?
         BE    UPDAMVFL            YES, GO MOVE TO FIELD
         LA    R4,8(R4)            BUMP FIELD TABLE ADDR
         B     UPDAFTLP            DO TILL TABLE END
*
UPDAMVFL DS    0H
         ICM   R3,B'1111',4(R4)    FLD TAB + 4 = OUTPUT FIELD ADDRESS
         SR    R7,R7               CLEAR FOR ICM
         ICM   R7,B'0011',2(R4)    FLD TAB + 2 = OUTPUT FIELD LENGTH
******** LA    R1,INPSBA1          PASS BUFFER ADDR TO CONVERTION ROUT
         BAL   R14,BUFF327B        GET BUFFER OFFSET ADDR FROM 3270 BA
         L     R8,CALLBUFN         OUTPUT BUFFER START ADDR
         LA    R8,0(R8,R15)        OUTPUT BUFFER DISPLACEMENT ADDR
         BCTR  R7,0                FIX FOR EX
         EX    R7,SCCMVC           MOVE VTAM DATA TO USER FIELD
*
UPDAEX   DS    0H
         LM    R1,R9,UDFRR         POP
         L     R14,SVUDF           POP RET ADDR
         BR    R14
*
*SCCMVC   MVC   0(*-*,R3),0(R9)     MOVE VTAM DATA TO USER FIELD
SCCMVC   MVC   0(*-*,R3),0(R8)     MOVE VTAM DATA TO USER FIELD
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
*******************************************************
*    ERASEMAP - ERASE MAP FIELD UP TO NEXT ATTR       *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R8  = DESTINATION FIELD ADDRESS        *
*    ON EXIT:  PANEL ERASED UP TO NEXT ATTR OR END    *
*******************************************************
ERASEMAP DS    0H
         ST    R14,SVERA               PUSH RET ADDR
         STM   R1,R8,ERARR             PUSH
******** LA    R8,SOURCE               SOURCE FIELD ADDRESS
*
ERASLP   DS    0H
         CLI   0(R8),X'33'             IS THIS AN ATTRIBUTE?
         BL    ERASEX                  YES, EXIT
         MVI   0(R8),C' '              ERASE IT
         LA    R8,1(R8)                BUMP
         C     R8,CALLBUFL             CALLING OUTPUT BUFFER END?
         BNH   ERASLP                  DO UNTIL ATTRIBUTE OR BUFF END
*
ERASEX   DS    0H
         LM    R1,R8,ERARR             POP
         L     R14,SVERA               POP RET ADDR
         BR    R14
*
*******************************************************
*     LFTSCN -  SCAN VALUE FROM LEFT TO RIGHT         *
*     ON ENTRY: R14 = RETURN ADDR                     *
*               R7  = END OF STRING                   *
*               R4  = START OF STRING                 *
*               R3  = ADDRESS OF VALUE TO SCAN        *
*               R2  = LENGTH OF VALUE TO SCAN         *
*               R0  = BRANCHING CONDITION             *
*                     X'80'/128 = EQUAL               *
*                     X'40'/64  = LOWER               *
*                     X'20'/32  = GRATER              *
*     ON EXIT:  R15 = 1 = VALUE NOT FOUND & BUFF END  *
*               R15 = 2 = VALUE FOUND                 *
*               R0  = LAST ADDRESS TESTED             *
*******************************************************
LFTSCN   DS    0H
         ST    R14,SVLFT       PUSH RET ADDR
         STM   R1,R9,LFTRR     PUSH REGS
*
*        C     R0,=F'0'        ANY CONDITION OTHER THAN EQUAL?
*        BE    LFTEQU          NO, LEAVE BRANCHING ON EQUAL COND
*        STC   R0,LFTSW+1      POKE CALLING PROCEDURE CONDITION
LFTEQU   DS    0H
******** LA    R7,STREND       END OF STRING
******** LA    R4,STRSTART     START OF STRING
******** LA    R3,=C'SCAN'     ADDRESS OF VALUE
******** LA    R2,4            LENGTH OF VALUE
         LR    R5,R4           ALSO R5 FOR COMPUTE
         LR    R6,R2           STEP BUMP
         BCTR  R2,0            DEBUMP FOR EX
LFTLP    DS    0H
         EX    R2,LFTCLC       IS IT THE VALUE?
         BL    LFTEX           YES, EXIT
         BXLE  R4,R6,LFTLP     R4=IF(R4+R6<=R7,R4+R6 & GOTO,R4)
         LA    R15,1           SHOW VALUE NOT FOUND & END OF BUFF
         B     LFTEX1          EXIT
LFTEX    DS    0H
         LA    R15,2           SHOW VALUE FOUND
*
LFTEX1   DS    0H
*        MVI   LFTSW+1,X'80'   RESET EQUAL CONDITION
         LR    R0,R4           SET LAST ADDRESS TESTED
         LM    R1,R9,LFTRR     POP REGS
         L     R14,SVLFT       POP RET ADDR
         BR    R14
*
LFTCLC   CLC   0(*-*,R4),0(R3)  COMPARE POS OF STRING WITH VALUE
*
*******************************************************
*     EXPAND: CONVERTS ONE HEX BYTES INTO TWO CHARS   *
*     ON ENTRY: R2 = INPUT SINGLE HEX BYTE            *
*               R3 = OUTPUT DOBBLE CHARACTERS         *
*               R4 = LENGTH OF INPUT FIELD            *
*     ON EXIT:  CHARACTERS MOVED TO OUTPUT            *
*******************************************************
EXPAND   DS    0H
         ST    R14,SVEXP           PUSH RET ADDR
         STM   R1,R6,EXPRR         PUSH REGS
*
******** LA    R2,FLDROW           INPUT FIELD ADDR
******** LA    R3,FLDROWCH         OUTPUT DOBBLE CHARACTERS
******** LA    R4,2                INPUT SIZE
*
EXPLP    DS    0H
         XR    R5,R5               FIX FOR IC
         IC    R5,0(R2)            GET BYTE TO CONVERT
         CVD   R5,DOBLE            CONVERT TO DECIMAL
         UNPK  MASK(3),DOBLE+6(2)  UNPACK IT
         MVZ   MASK+2(1),=X'F0'    ELIMINATE SIGN
         MVC   0(2,R3),MASK+1      PILE'EM UP ON OUTPUT
         LA    R2,1(R2)            BUMP INPUT
         LA    R3,2(R3)            BUMP OUTPUT
         BCT   R4,EXPLP            DO R4 TIMES
*
         LM    R1,R6,EXPRR         POP REGS
         L     R14,SVEXP           POP RET ADDR
         BR    R14
*
*******************************************************
*    ROWCOL - CALCULATE ROW/COL VALUES FROM BUFF OFFS *
*    ON ENTRY:                                        *
*              R14 = RETURN ADDR                      *
*              R1  = BUFFER OFFSET LOCATION           *
*           COLROW = SCREEN COLUMNS                   *
*    ON EXIT:                                         *
*         FLDROWCH = FIELD ROW LOCATION (2 CHARS)     *
*         FLDCOLCH = FIELD COLUMN LOCATION (2 CHARS)  *
*          FLDROW  = FIELD ROW LOCATION (1 BYTE)      *
*          FLDCOL  = FIELD COLUMN LOCATION (1 BYTE)   *
*******************************************************
ROWCOL   DS    0H
         ST    R14,SVBFF               PUSH RET ADDR
         STM   R1,R4,BFFRR             PUSH
*
         XR    R2,R2                   USE THE WHOLE OF IT
         ICM   R2,B'0011',0(R1)        GET BUFF OFFSET LOC
         AH    R2,=H'1'                MAKE RELATIVE TO ONE
         SRDA  R2,32(0)                SHIFT OVER TO R3 FOR SIGN
         XR    R4,R4                   USE THE WHOLE OF IT
         ICM   R4,B'0011',COLROW       GET PANEL COLUMNS
         DR    R2,R4                   BUFF-LOC / PANEL-COLS
         LTR   R2,R2                   IS IT EVEN?
         BZ    ROWCEV                  YES, LOC AT END OF LINE
         AH    R3,=H'1'                BUMP FOR ROW LOC
         STC   R3,FLDROW               SAVE ROW LOC
         STC   R2,FLDCOL               SAVE COL LOC FROM DIV
         B     ROWCEX                  AND EXIT
ROWCEV   DS    0H
         STC   R3,FLDROW               SAVE ROW LOC
         STC   R4,FLDCOL               SAVE COL LOC FROM PAN COLS
*
ROWCEX   DS    0H
         LA    R2,FLDROW               INPUT FIELD ADDR
         LA    R3,FLDROWCH             OUTPUT DOBBLE CHARACTERS
         LA    R4,2                    INPUT SIZE
         BAL   R14,EXPAND              CONVERT 1 CHAR COORD INTO 2
*
         LM    R1,R4,BFFRR             POP
         L     R14,SVBFF               POP RET ADDR
         BR    R14
*
***********************************************************************
***   3270 BUFFER ADDRESS CONVERTION TO STORAGE BUFFER OFFSET       ***
***   ON ENTRY: R1  = LOCATION OF 3270 BUFFER ADDRESS (2 CHARS)     ***
***                                                                 ***
***   ON EXIT:  R15 = BUFFER OFFSET                                 ***
***********************************************************************
BUFF327C DS    0H
         ST    R14,SVBUF         PUSH RET ADDR
         STM   R1,R2,BUFRR       PUSH REGS
         SR    2,2               CLEAR WORK REG
         SR    15,15             CLEAR WORK REG
         IC    2,0(1)            GET FIRST ADDR CHAR
         N     2,=F'63'          TURN OFF ALL BITS BUT LAST SIX
         IC    15,1(1)           GET SECONF ADDR CHAR
         N     15,=F'63'         TURN OFF ALL BITS BUT LAST SIX
         SLL   2,6               SHIFT FIRST CHAR SIX BITS TO LEFT
         AR    15,2              RESULT IN R15
         LM    R1,R2,BUFRR       POP REGS
         L     R14,SVBUF         POP RET ADDR
         BR    14
*
***********************************************************************
***                                                                 ***
*** SUBRUTINAS PARA EL MANEJO DE CARACTERES DE CONTROL 3270         ***
***                                                                 ***
*** CONVIERTE UN HALFWORD BINARIO QUE REPRESENTA EL "OFFSET"        ***
*** RELATIVO A LA POSICION 0 DE UN BUFFER 3270 A SU EQUIVALENTE     ***
*** DIRECCION 3270 (DOS CARACTERES)                                 ***
***   ENTRADA: R1 -> DIRECCION DE LA MEDIA PALABRA                  ***
***   SALIDA : R15-> DIRECCION DE LOS DOS CARACTERES                ***
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
***                  |C|F|CF|  F=FILA; C=COLUMNA; CF=COLS/FILA      ***
***   SALIDA : R15-> DIRECCION DE BUFFER                            ***
***********************************************************************
BUFF327B DS    0H
         SR    2,2               CLEAR WORK REG
         IC    2,0(1)            GET COLUMN
         BCTR  2,0               MINUS 1 FOR ADD
         SR    15,15             CLEAR WORK REG
         IC    15,1(1)           GET ROW
         BCTR  15,0              MINUS 1 FOR MULT
         MH    15,=H'80'         TIMES NO OF COLS/ROW
*        MH    15,2(1)           TIMES NO OF COLS/ROW
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
         LA    R1,COL
         BAL   R14,BUFF327B         COMPUTE BINARY BUFFER ADDRESS
         STH   R15,BINBUFAD         SAVE IT
         LA    R1,BINBUFAD          PASS IT TO CONVERSION SUBROUTINE
         LA    R15,CHABUFAD         RESPONSE AREA
         BAL   R14,ADDR3270         GO GET IT
         LM    R1,R2,TWERR          POP REGS
         L     R14,SVTWE            POP RET ADDR
         BR    R14
*
*******************************************************
*    CONSTANTS AND VARIABLES                          *
*******************************************************
         LTORG
*******************************************************
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
*******************************************************
         DC    C'HERE ARE THE CURSORS'
*APPLCURS EQU   *   *************** ONLY FOR TEST *****************
FLDROWCH DC    X'0000'    ROW LOCATION 2 EBCDIC CHARS
FLDCOLCH DC    X'0000'    COLUMN LOCATION 2 EBCDIC CHARS
FLDROW   DC    X'00'      ROW LOCATION 1 BYTE
FLDCOL   DC    X'00'      COLUMN LOCATION 1 BYTE
BOFFSET  DC    H'0'       BUFFER OFFSET SAVE AREA
INPSBA   DC    X'0000'    INPUT BUFFER ADDRESS
INPSBA1  DC    X'0000'    PRIOR INPUT BUFFER ADDRESS
DOBLE    DS    D           CVD BUFF AREA
MSK      DC    XL16'40202020202020202020202020202020'
MASK     DC    XL16'40202020202020202020202020202020'
*******************************************************
SAVESCR  DS    18F        PROGRAM SAVE AREA
*******************************************************
SVLFT    DS    F          LFTSCN PUSH RET ADDR
LFTRR    DS    9F         LFTSCN R1-R9 SAVE AREA
SVTWE    DS    F          TWELVE PUSH RET ADDR
TWERR    DS    2F         TWELVE R1-R2 PUSH REGS
SVEXC    DS    F          EXCHAN PUSH RET ADDR
EXCRR    DS    2F         EXCHAN R1-R2 PUSH REGS
SVLON    DS    F          LONGMV PUSH RET ADDR
LONRR    DS    8F         LONGMV R1-R8 PUSH AREA
SVBUF    DS    F          BUFF327C PUSH RET ADDR
BUFRR    DS    2F         BUFF327C PUSH REGS
SVBFF    DS    F          BUFFOFFC PUSH RET ADDR
BFFRR    DS    4F         BUFFOFFC PUSH REGS
SVEXP    DS    F          EXPAND PUSH RET ADDR
EXPRR    DS    6F         EXPAND PUSH REGS
SVSCC    DS    F          SCANFLD PUSH RET ADDR
SCCRR    DS    9F         SCANFLD PUSH REGS
SVERA    DS    F          ERASEMAP PUSH RET ADDR
ERARR    DS    8F         ERASEMAP PUSH REGS
SVUDF    DS    F          UPDATEFL PUSH RET ADDR
UDFRR    DS    9F         UPDATEFL PUSH REGS
         DS    16F'0'     DELIMITER
*******************************************************
         DC    C'HERE IS RETURN CODE BUFFPROI'
RETCODE  DC    F'0'         PROGRAM RETURN CODE
CALLBUFN DC    A(0)         CALLING SCREEN BUFFER START (OUTPUT)
CALLBUFL DC    A(0)         CALLING SCREEN BUFFER END (OUTPUT)
FLDTABOF DC    V(USERFLDT)  BUFFPRO FIELD TABLE OFFSET
*******************************************************
*         DC    C'HERE INPUT  AREA'
*AREA1    DC    0CL80' '    OUTPUT SCREEN AREA
*         DC    X'F14040'   KEY & CURSOR LOC
*         DC    X'114040'   ORDER & FIELD LOC
*         DC    C'A'        ORDER & FIELD LOC
*         DC    X'1140C4'   ORDER & FIELD LOC
*         DC    C'B'        ORDER & FIELD LOC
*         DC    X'1140C9'   ORDER & FIELD LOC
*         DC    C'C'        ORDER & FIELD LOC
*         DC    X'11404E'   ORDER & FIELD LOC
*         DC    C'D'        ORDER & FIELD LOC
*         DC    X'1140D3'   ORDER & FIELD LOC
*         DC    C'E'        ORDER & FIELD LOC
*         DC    X'115CF0'   ORDER & FIELD LOC
*         DC    C'X'        ORDER & FIELD LOC
*         DC    CL67' '     REST OF DATA
*AREA1END EQU   *          OUTPUT SCREEN AREA LIMIT
*         DC    X'00'      LIMIT
*         DC    C'HERE OUTPUT AREA'
*PANT1    DC    CL80' '    OUTPUT SCREEN AREA
*         DC    CL80' '    OUTPUT SCREEN AREA
*         DC    CL80' '    OUTPUT SCREEN AREA
*         DC    CL80' '    OUTPUT SCREEN AREA
*PANTL    EQU   *          OUTPUT SCREEN AREA LIMIT
*         DC    C'HERE OUTPUT AREA END'
*
INPSTR   DSECT
KEYCODE  DS    X               INPUT KEY CODE
CURLOCT  DS    XL2             INPUT CURSOR LOCATION
ORDERCD  DS    X               INPUT ORDER CODE
FLDLOCT  DS    XL2             INPUT FIELD LOCATION
INPDATA  DS    C               INPUT DATA STRING
*
         COPY  \R42\CPY\EQUREGS.CPY
         END   BUFFPROI
