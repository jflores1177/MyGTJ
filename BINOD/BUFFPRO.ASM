*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - BUFFPRO                             *
*                 SCREEN BUFFER CONVERTION TO 3270    *
*                 NO SCREEN BUFFER USED AREA SHOWN    *
*                 WILL ALWAYS START AT SAME LOCATION  *
*                 IN SCREEN BUFFER.                   *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - SEPTEMBER 1995                      *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: BUFFPRO                    *
*                          DBG                        *
*     OTHER ENTRIES:                                  *
*        CALLSCRN = CALLING SCREEN BUFFER START       *
*        CALLSCRL = CALLING SCREEN BUFFER END         *
*        USERFLDT = 512 USER FIELD TABLE (4096 BYTES) *
*        USERFLDL = USER FIELD TABLE DELIMITER        *
*                                                     *
*     EXTERNAL REFERENCES:                            *
*        SCREENL  = COMMON SCREEN AREA ADDR & LIMIT   *
*        PARMLIST = COMMON SCREEN AREA PARAMETER LIST *
*        BOXPARML = COMMON SCREEN AREA BOX PARM LIST  *
*                                                     *
*******************************************************
*        ENTRY BUFFPRO,INSCRN,INSCRNL,CALLSCRN,CALLSCRL
*        ENTRY USERFLDT,USERFLDL,DBG
*        EXTRN SCREENL,SCREEN,SCREENE,PARMLIST,BOXPARML
*        EXTRN PANLENG
*
*        PRINT NOGEN
         TITLE '                                * * *   BUFFPRO  * * *'
**************************************************************
*        BUFFPRO ROUTINE
**************************************************************
BUFFPRO  CSECT
         ENTRY
         ENTRY CICS
CICS     EQU   *
XX5      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XX5,R12,R11
         LA    R15,XX5SAV1     GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)      SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)      SAVE PROGRAMS SAVE AREA IN CALLERS AREA
         LR    R13,R15         GET PROGRAMS SAVE AREA ADDR
         L     R11,XX5SAV2     LOAD 2ND BASE REG
         B     AROUND          SKIP SAVE AREA FLD
XX5SAV1  DS    18F             RETURN SAVE REGS AREA
XX5SAV2  DC    A(XX5+4096)     2ND BASE REG ADDR
         DC    CL12'<<BUFFPRO>>'  DUMP PROGRAM ID
AROUND   DS    0H
*
         L     R2,0(R1)             CALLING SCREEN BUFFER START
         LTR   R3,R2                IS THIS THE LAST PARAMETER?
         BL    SCREEX               YES, WRONG, EXIT
         ST    R2,CALLSCRN          CALLING SCREEN BUFFER START
         L     R2,4(R1)             CALLING SCREEN BUFFER END
         LTR   R3,R2                IS THIS THE LAST PARAMETER?
         BL    SCREEX               YES, WRONG, EXIT
         ST    R2,CALLSCRL          CALLING SCREEN BUFFER END
         MVI   0(R2),X'00'          SET DELIMITER
*
         L     R11,8(R1)            BUMP TO GET PARAMETERS
         USING INPSTR,R11           ADDRESSABILITY
*
         CLI   RESET,C' '          IS THIS AN ACCUMULATOIN MAP?
         BE    SCCRESE1            NO, DO RESETING BUFF TO START
         B     SCRESLP1            GO DO REST
SCCRESE1 DS    0H
         MVC   FLDTABOF(4),=A(USERFLDT) SET FLD TAB OFFS TO START
SCRESLP1 DS    0H
         LTR   R3,R11               IS THIS THE LAST PARAMETER?
         BL    BUFFLAST             YES, SKIP FIELD ADDR TABLE UPD
         LA    R3,12(R1)            GET FIELD ADDR PARAMETERS
*        L     R2,=A(USERFLDT)      512 USER FIELD TABLE (4096 BYTES)
         L     R2,FLDTABOF          512 USER FIELD TABLE (4096 BYTES)
BUFFFLP  DS    0H
         C     R2,=A(USERFLDL)      ARE WE AT THE DELIMITER?
         BNL   BUFFLAST             YES, CAN PILE NO MORE
         L     R4,0(R3)             GET ADDR IN PARAM LIST
         LTR   R4,R4                IS THIS THE LAST PARAMETER?
         BL    BUFFLAST             YES, MUST BE DELIMETER LABEL
*
         BAL   R14,FLDCOORD         GO GET FIELD SCREEN COORDINATES
         LTR   R15,R15              ANY BUGS?
         BNZ   SCREEX               YES, WRONG, EXIT
         LR    R5,R0                JUST USE R5 INSTEAD OF R0
         MVC   0(2,R2),0(R5)        SAVE 3270 BUFFER ADDRESS
         LA    R3,4(R3)             BUMP CALLING PARAMETER TABLE ADDR
         L     R5,0(R3)             GET ADDR IN PARAM LIST
         LTR   R5,R5                IS THIS THE LAST PARAMETER?
         BL    BUFFLAST             YES, MUST BE DELIMETER LABEL
*
         MVC   4(4,R2),0(R3)        SAVE FIELD ADDRESS
         LA    R3,4(R3)             BUMP CALLING PARAMETER TABLE ADDR
         L     R5,0(R3)             GET TO USE IN CALCULATION
         SR    R5,R4                GET DISPLACEMENT FROM DIFERENCE
         SH    R5,=H'4'             MINUS COORDINATE FIELD LEN
         STCM  R5,B'0011',2(R2)     SAVE FIELD LENGTH
         BAL   R14,UPDATEFL         UPDATE MAP FROM USER FIELD
         LA    R2,8(R2)             BUMP FIELD TABLE ADDR
         B     BUFFFLP              DO TILL LAST PARAMETER FROM LIST
*
BUFFLAST DS    0H
         XC    0(8,R2),0(R2)        RESET LAST FIELD TABLE ARG
         ST    R2,FLDTABOF          SAVE FOR NEXT TURN
         OC    INPCONV,=C'00000000000000000000' CONVERT TO NUMERICS
         MVC   SCRPARM,CLEARSC      CALLING CLEAR SCREEN ORDER
         MVC   SCRBXCHR,BOXCHR      CALLING BOX CHARACTERS
         MVC   SCRCURSR,INCROW      INCERT CURSOR IN ROW
         MVC   SCRCURSC,INCCOL      INCERT CURSOR IN COLUMN
*
         LA    R2,BOXTR             BOX INPUT CHARACTER DIGITS
         LA    R3,SCRBXPLS          BOX OUTPUT HEX DIGITS
         LA    R0,4                 OUTPUT LENGTH
         BAL   R14,SHRINK           SHRINK 2 NUMBERS IN 1 & MOVE
*
         LA    R2,CLSCORD           CLEAR COORDINATES INP CHAR DIGITS
         LA    R3,SCRCLSC           CLEAR COORDINATES OUT HEX DIGITS
         LA    R0,4                 OUTPUT LENGTH
         BAL   R14,SHRINK           SHRINK 2 NUMBERS IN 1 & MOVE
         CLI   RESET,C' '          IS THIS AN ACCUMULATOIN MAP?
         BE    SCCRESET            NO, DO RESETING BUFF TO START
         B     SCRESLP             GO DO SCAN
SCCRESET DS    0H
         L     R4,=V(SCREENL)       GET COMMON SCR BUFF USED UP SPC
         L     R3,=V(PANLENG)       GET SCR BUFF START POINTER
         MVC   0(4,R4),0(R3)        START OUT WITH SAME LENGTH
*
SCRESLP  DS    0H
         BAL   R14,SCANFLD         SCAN INPUT & BUILD OUTPUT
         CLC   RETCODE,=F'0'       ANY BUGGS?
         BNE   SCREEX              YES, EXIT
         BAL   R14,DISPSCR         GO UPDATE OUTPUT SCREEN AREA
*        DROP  R11
*
SCREEX   DS    0H
         EXIT  SAVE=YES
*
*******************************************************
*  ~  FLDCOORD: PROCESS FIELD SCREEN COORDINATES      *
*     ON ENTRY: R4 = COORDINATES ADDREESS             *
*     ON EXIT:  R15 = 0 = OK / INVALID FIELD          *
*               R0 = OUTPUT FIELD ADDRESS             *
*******************************************************
FLDCOORD DS    0H
         ST    R14,SVFLD           PUSH RET ADDR
         STM   R1,R4,FLDRR         PUSH REGS
*
         MVC   FIELDCOI,0(R4)      MOVE NUMBERS TO WORK AREA
         LA    R0,4                NUMBER FIELD LENGTH
         LA    R2,FIELDCOI         NUMBER FIELD ADDRESS
         BAL   R14,NUMBERS         VALIDATE NUMBER CHAR FIELD (F1-F9)
         LTR   R15,R15             VALID CHAR NUMBERS?
         BNZ   FLDCEX              NO, EXIT
*
         LA    R2,FIELDCOI         NUMBER FIELD ADDRESS
         LA    R3,FIELDCOO         OUTPUT HEX DIGITS ADDR
         LA    R0,2                OUTPUT LENGTH
         BAL   R14,SHRINK          SHRINK 2 NUMBERS IN 1 & MOVE
*
         MVC   ROW,FIELDCOO        POKE ROW IN TWELVE PARM
         MVC   COL,FIELDCOO+1      POKE COL IN TWELVE PARM
         BAL   R14,TWELVE          GO GET SCREEN ADDRESS
         LA    R0,CHABUFAD         PASS 3270 SCREEN BUFFER ADDRESS
         XR    R15,R15             SHOW GOOD RET CODE
*
FLDCEX   DS    0H
         LM    R1,R4,FLDRR         POP REGS
         L     R14,SVFLD           POP RET ADDR
         BR    R14
*
*******************************************************
*    NUMBERS: TEST IF VALID NUMBER CHARACTERS (F1-F9) *
*     ON ENTRY: R2 = INPUT FIELD ADDRESS              *
*               R0 = OUTPUT FIELD LENGTH              *
*     ON EXIT:  R15 = 0 = OK | NOT NUMERIC            *
*******************************************************
NUMBERS  DS    0H
         ST    R14,SVNUM          PUSH RET ADDR
         STM   R1,R3,NUMRR        PUSH REGS
         LR    R3,R0              INPUT LENGTH
         LA    R15,1              DEFAULT TO WRONG RETURN CODE
*
NUMLP    DS    0H
         CLI   0(R2),C'0'         LOWER THAN 0?
         BL    NUMEX              YES, EXIT
         CLI   0(R2),C'9'         GREATER THAN 9?
         BH    NUMEX              YES, EXIT
         LA    R2,1(R2)           BUMP INPUT ADDR
         BCT   R3,NUMLP           DO TILL LENGTH EXAUSTED
         LA    R15,0              SHOW GOOD NUMBER RET CODE
*
NUMEX    DS    0H
         LM    R1,R3,NUMRR        POP REGS
         L     R14,SVNUM          POP RET ADDR
         BR    R14
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
         BAL   R14,CLEAR           CLEAR OUTPUT TEXT FIELD
*
         LA    R9,960              MAXIMUM INPUT FIELDS
         L     R4,CALLSCRN         INPUT SCREEN START ADDR
         L     R8,=A(OUTSCRN)      OUTPUT SCREEN START ADDR
*
SCCLP    DS    0H
         LA    R2,1                VALUE LENGTH
         LA    R3,=X'21'           VALUE ADDREESS (DEC 33)
         L     R7,CALLSCRL         INPUT SCREEN END ADDR
         LR    R5,R4               SAVE STR START ADDR
         LA    R0,64               SET BRANCHING COND (X'40' LOWER)
         BAL   R14,LFTSCN          SEARCH FOR AN ATTRIBUTE
         LR    R10,R15             GET LFTSCN ROUTINE ANSWER
         LR    R4,R0               GET START POINTER
         LR    R6,R4               COPY START POINTER
         SR    R6,R5               CALCULATE PARM LENGTH
*
SCCNON   DS    0H
         LTR   R8,R8               ARE WE DOING ANY OUTPUT?
         BZ    SCCNOMVC            NO, SKIP OUTPUT
*
         CH    R6,=H'0'            IS IT A DOBBLE FINDING? **ERROR**
         BNH   SCCNOMV1            YES, DON'T MOVE ANYTHING
         BAL   R14,TRIMLBLK        GO TRIM LEFT MOST BLANKS
         LTR   R15,R15             DO WE HAVE NON BLANKS?
         BZ    SCCNOMV1            NO, SKIP MOVING
         CLC   REPETADD,=F'0'      ANY REPEAT TO ADDRESS?
         BE    SCCNOLTR            NO, ASSUME NO LEFT TRIM DONE
         LR    R5,R0               UPDATE NON-BLK START POINTER
SCCNOLTR DS    0H
         BAL   R14,TRIMRBLK        GO TRIM RITE MOST BLANKS
         LTR   R15,R15             DO WE HAVE NON BLANKS?
         BZ    SCCNOMV1            NO, SKIP MOVING
         LA    R6,0(R15)           BUMP TO COUNT EXACT LENGTH FOR MVCL
*
         CLC   REPETADD,=F'0'      ANY REPEAT TO ADDRESS?
         BE    SCCNOLT1            NO, ASSUME NO LEFT TRIM DONE
         L     R1,REPETADD         GET REPEAT TO ADDRESS
         S     R1,CALLSCRN         GET INPUT DISPLACEMENT
         STH   R1,BINBUFAD         SAVE INPUT BUFFER OFFSET
         LA    R1,BINBUFAD         GET OFFSET FIELD ADDR
         LA    R15,CHABUFAD        GET RESPOSE FIELD ADDR
         BAL   R14,ADDR3270        GO GET 3270 SCREEN ADDR
         MVC   REPETSBA,CHABUFAD   REPEAT TO BUFFER ADDRESS
         MVC   0(4,R8),REPETCOM    MOVE REPEAT TO ADDRESS COMMAND
         LA    R8,4(R8)            BUMP OUTPUT STRING
         XC    REPETADD,REPETADD   RESET ANY REPEAT TO ADDRESS
*
SCCNOLT1 DS    0H
         LR    R1,R5               USE A COPY OF THE INPUT ADDR
         S     R1,CALLSCRN         GET INPUT DISPLACEMENT
         STH   R1,BINBUFAD         SAVE INPUT BUFFER OFFSET
         LA    R1,BINBUFAD         GET OFFSET FIELD ADDR
         LA    R15,CHABUFAD        GET RESPOSE FIELD ADDR
         BAL   R14,ADDR3270        GO GET 3270 SCREEN ADDR
         MVI   0(R8),X'11'         MOVE START BUFFER ADDRESS ORDER
         MVC   1(2,R8),CHABUFAD    MOVE SCREEN ADDRESS
         LA    R8,3(R8)            BUMP OUTPUT STRING
         BAL   R14,LONGMV          STRING (LONG) DATA UP ON OUTPUT
         LA    R8,0(R6,R8)         BUMP OUTPUT STRING
*
SCCNOMV1 DS    0H
         CH    R10,=H'1'           BUFFER END REACHED?
         BE    SCCNEX              YES, EXIT SCAN
         C     R4,CALLSCRL         BUFFER END REACHED?
         BNL   SCCNEX              YES, EXIT SCAN
         LR    R1,R4               USE A COPY OF THE INPUT ADDR
         S     R1,CALLSCRN         GET INPUT DISPLACEMENT
         STH   R1,BINBUFAD         SAVE INPUT BUFFER OFFSET
         LA    R1,BINBUFAD         GET OFFSET FIELD ADDR
         LA    R15,CHABUFAD        GET RESPOSE FIELD ADDR
         BAL   R14,ADDR3270        GO GET 3270 SCREEN ADDR
         MVI   0(R8),X'11'         MOVE START BUFFER ADDRESS ORDER
         MVC   1(2,R8),CHABUFAD    MOVE SCREEN ADDRESS
         MVI   3(R8),X'1D'         MOVE START FIELD ORDER
         SR    R1,R1               FIX FOR IC
         IC    R1,0(R4)            GET INPUT ATTR REPLACE CODE
         BAL   R14,EXCHAN          GO GET REPLACEMENT FOR ATTRIBUTE
         LTR   R0,R0               ANY VALID ATTRIBUTE?
         BNZ   SCCNATOK            YES, ATTRIBUTE OK
         MVC   RETCODE,=F'1'       NO, SET INVALID ATTR RET CODE
         B     SCCNEX              EXIT
*
SCCNATOK DS    0H
         STC   R0,4(R8)            POKE ATTRIBUTE
         LA    R8,5(R8)            BUMP OUTPUT STRING
*
*******************************************************
*        CLI   0(R4),17            AN UNPROTECTED FIELD?
*        BNL   SCCNOMVC            NO, SKIP SBA SAVING
*        LA    R1,1(R4)            MAP FIELD START ADDR
*        S     R1,CALLSCRN         GET INPUT DISPLACEMENT
*        STH   R1,BINBUFAD         SAVE INPUT BUFFER OFFSET
*        LA    R1,BINBUFAD         GET OFFSET FIELD ADDR
*        LA    R15,CHABUFAD        GET RESPOSE FIELD ADDR
*        BAL   R14,ADDR3270        GO GET 3270 SCREEN ADDR
*        MVC   FIELDCOO,CHABUFAD   USE COORDINATE FIELD AS PARM
*        LA    R1,FIELDCOO         USE SBA TO CALL FIELD MOVE ROUT
*        BAL   R14,UPDATEFL        GO MOVE FLD TO MAP
*******************************************************
*
SCCNOMVC DS    0H
         CH    R10,=H'1'           BUFFER END REACHED?
         BE    SCCNEX              YES, EXIT SCAN
*        C     R4,=A(INSCRNL)      BUFFER END REACHED?
         C     R4,CALLSCRL         BUFFER END REACHED?
         BNL   SCCNEX              YES, EXIT SCAN
         LA    R15,1(R6)           RETURN TEXT LENGTH
         LR    R0,R5               RETURN EFECTIVE START ADDRESS
         LA    R4,1(R4)            BUMP INPUT POINTER
         BCT   R9,SCCLP            ROLL IT FOR MAX INPUT TIMES
*
SCCNEX   DS    0H
         CLC   SCRCURSR,=C'00'     ANY INCERT CURSOR IN ROW?
         BE    SCCNEX1             NO, EXIT SCAN
         CLC   SCRCURSC,=C'00'     ANY INCERT CURSOR IN COLUMN?
         BE    SCCNEX1             NO, EXIT SCAN
         LA    R2,SCRCURSR         INCERT CURSOR IN ROW (2 CHARS)
         LA    R3,ROW              BINARY OUTPUT ROW (1 CHAR)
         LA    R0,1                OUTPUT LENGTH
         BAL   R14,SHRINK          SHRINK 2 NUMBERS IN 1 & MOVE
         CLI   ROW,24              IS IT A VALID ROW?
         BNH   SCCNROK             YES, ROW OK
         MVI   ROW,1               NO, DEFAULT IT TO 1
SCCNROK  DS    0H
         LA    R2,SCRCURSC         INCERT CURSOR IN COLUMN (2 CHARS)
         LA    R3,COL              BINARY OUTPUT COLUMN (1 CHAR)
         LA    R0,1                OUTPUT LENGTH
         BAL   R14,SHRINK          SHRINK 2 NUMBERS IN 1 & MOVE
         CLI   COL,80              IS IT A VALID COLUMN?
         BNH   SCCNCOK             YES, ROW OK
         MVI   COL,1               NO, DEFAULT IT TO 1
SCCNCOK  DS    0H
         BAL   R14,TWELVE          GO GET SCREEN ADDRESS
         MVI   0(R8),X'11'         MOVE SET BUFFER ADDEESS ORDER
         MVC   1(2,R8),CHABUFAD    MOVE SCREEN ADDRESS
         MVI   3(R8),X'13'         MOVE INCERT CURSOR ORDER
         LA    R8,4(R8)            BUMP OUTPUT POINTER
*
SCCNEX1  DS    0H
*        ST    R8,BUFFERST         SAVE FOR SCREEN ACCUMULATION
         S     R8,=A(OUTSCRN)      GET OUTPUT SCREEN SPACE USED
         ST    R8,BUFFERLN         SAVE FOR SCREEN BUFFER MOVE
         LM    R1,R9,SCCRR         POP REGS
         L     R14,SVSCC           POP RET ADDR
         BR    R14
*
*SCCMVC   MVC   0(*-*,R2),0(R3)     MOVE FIELD DATA TO USER MAP
*
*******************************************************
*    UPDATEFL  UPDATE MAP FROM USER FIELD             *
*    ON ENTRY: R14 = RETURN ADDR                      *
*              R2  = INPUT 3270 BUFFER ADDRESS        *
*              R2  = FIELD TABLE ADDRESS POINTER      *
*    ON EXIT:  MAP UPDATED USING LEGTH IN TABLE       *
*******************************************************
UPDATEFL DS    0H
         ST    R14,SVUDF           PUSH RET ADDR
         STM   R1,R8,UDFRR         PUSH
*
******** LA    R2,SBA              SCREEN BUFFER ADDR
******** LA    R2,FIELD            USER FIELD TABLE ARGUMENT
         LR    R1,R2               GET SCREEN ADDRESS ADDR
         BAL   R14,BUFF327C        GO GET BUFFER OFFSET FROM 3270 BA
         L     R8,CALLSCRN         GET CALLING BUFFER START ADDR
         LA    R8,0(R15,R8)        GET CALLING BUFFER DISPLACEMENT
*
         ICM   R3,B'1111',4(R2)    FLD TAB + 4 = FIELD ADDRESS
         SR    R7,R7               CLEAR FOR ICM
         ICM   R7,B'0011',2(R2)    FLD TAB + 2 = FIELD LENGTH
         BCTR  R7,0                FIX FOR EX
         EX    R7,SCCMVC           MOVE USER FIELD TO MAP
*
         LM    R1,R8,UDFRR         POP
         L     R14,SVUDF           POP RET ADDR
         BR    R14
*
SCCMVC   MVC   0(*-*,R8),0(R3)     MOVE USER FIELD TO MAP
*
*******************************************************
*     PUSHST -  PUSH STRING ON SCREEN OUTPUT BUFFER   *
*     ON ENTRY: STRPOINT MUST BE SET PRIOR ENTRY      *
*               R14 = RETURN ADDR                     *
*               R2  = DATA ADDRESS                    *
*               R3  = LENGTH                          *
*     ON EXIT:  DATA STACKED, << NO LENGTH UPDATED >> *
*******************************************************
PUSHST   DS    0H
         ST    R14,SVPUS             PUSH RET ADDR
         STM   R1,R8,PUSRR           PUSH REGS
*
         L     R15,=V(SCREENL)       SCREEN LENGTH ADDR
*        L     R15,=V(PANLENG)       SCREEN LENGTH ADDR
         L     R1,0(R15)             GET SCREEN SAPCE USED
         A     R1,=V(SCREEN)         SCREEN START ADDR
         LA    R4,0(R3,R1)           GET REACH
         C     R4,=V(SCREENE)        WILL IT THRESSPASS THE LIMIT?
         BNL   PUSHEX                YES, CAN PILE NO MORE
         LR    R5,R2                 SOURCE FIELD ADDRESS
         LR    R6,R3                 FIELD LENGTH
         LR    R8,R1                 DESTINATION FIELD ADDRESS
         BAL   R14,LONGMV            UPDATE SCREEN OUTPUT BUFF
         LA    R1,0(R1,R3)           BUMP
         S     R1,=V(SCREEN)         CALCULATE LENGTH
         L     R15,=V(SCREENL)       SCREEN SPACE USED
         ST    R1,0(R15)             UPDATE LENGTH
PUSHEX   DS    0H
         LM    R1,R8,PUSRR           POP REGS
         L     R14,SVPUS             POP RET ADDR
         BR    R14
*
*******************************************************
*     TRIMRBLK: TRIM RITE BLANK POSITIONS FROM FIELD  *
*     ON ENTRY: R4 = STRING END LOCATION ADDR         *
*               R7 = STRING START LOCATION ADDRESS    *
*     ON EXIT:  R15 = ZERO = NOT FOUND / NON-BLK LEN  *
*               R0  = 1ST NON BLANK POS ADDR          *
*******************************************************
TRIMRBLK DS    0H
         ST    R14,SVTRI           PUSH RET ADDR
         STM   R1,R9,TRIRR         PUSH REGS
         LR    R7,R5               GET STRING START ADDR
******** LR    R4,END              GET STR END ADDR (ALREADY IN R4)
         BCTR  R4,0                DEBUMP TO SKIP NON-BLANK DELIMITER
         BAL   R14,RITNBL          DO RITE TRIM
         LTR   R15,R15             ANY LUCK?
         BZ    TRIMREX             NO, EXIT
*
TRIMREX  DS    0H
         LM    R1,R9,TRIRR         POP REGS
         L     R14,SVTRI           POP RET ADDR
         BR    R14
*
*******************************************************
*     TRIMLBLK: TRIM LEFT BLANK POSITIONS FROM FIELD  *
*     ON ENTRY: R4 = STRING END LOCATION ADDR         *
*               R7 = STRING START LOCATION ADDRESS    *
*     ON EXIT:  R15 = ZERO = NOT FOUND / FOUND        *
*               R0  = 1ST NON BLANK POS ADDR          *
*******************************************************
TRIMLBLK DS    0H
         ST    R14,SVTLI           PUSH RET ADDR
         STM   R1,R9,TLIRR         PUSH REGS
*
         XC    REPETADD,REPETADD   RESET REPEAT TO ADDRESS
         LA    R2,1                VALUE LENGTH
         LA    R3,=X'40'           VALUE ADDREESS (BLANK)
         LA    R0,96               SET BRANCHING COND (X'60' UNEQUAL)
         LR    R7,R4               STR END FOR LTRIM
         LR    R4,R5               STR START FOR LTRIM
         BAL   R14,LFTSCN          DO LEFT TRIM
         CH    R15,=H'2'           ANY LUCK?
         BE    TRIMLWHE            YES, CHECK WHERE
         XR    R15,R15             NO, SHOW CONDITION NOT MET
         B     TRIMLEX             EXIT
TRIMLWHE DS    0H
         LR    R6,R0               GET NON-BLK LOCATION
         SR    R6,R4               CALCULATE DISTANCE FROM START
         CH    R6,=H'4'            MORE THAN 4 BYTES AWAY?
         BNH   TRIMLEX             NO, LEAVE BLANKS IN DATA STREAM
         LR    R6,R0               GET NON-BLK LOCATION
         BCTR  R6,0                POINT JUST BEFORE NON-BLK
         ST    R6,REPETADD         SET REPEAT TO ADDRESS
*
TRIMLEX  DS    0H
         LM    R1,R9,TLIRR         POP REGS
         L     R14,SVTLI           POP RET ADDR
         BR    R14
*
*******************************************************
*    EXCHAN - SUBSTITUTE ATTRIBUTE EXCHANGE           *
*    ON ENTRY: R1 = BINARY CODE TO EXCHANGE           *
*    ON EXIT:  R0 = ATTRIBUTE                         *
*******************************************************
EXCHAN   DS    0H
         ST    R14,SVEXC               PUSH RET ADDR
         STM   R1,R2,EXCRR             PUSH
*
         CH    R1,=H'32'               IS IT PASSED THE LIMIT?
         BNH   EXCON                   NO, GO ON
         SR    R0,R0                   YES, SHOW NO ATTRIBUTE
         B     EXCEX                   EXIT
*
EXCON    DS    0H
         BCTR  R1,0                    MAKE RELATIVE TO 0
         SR    R2,R2                   FIX FOR IC
         IC    R2,ATTRTAB(R1)          REPLACE WITH ATTRIBUTE
         LR    R0,R2                   RETURN ATTRIBUTE
*
EXCEX    DS    0H
         LM    R1,R2,EXCRR             POP
         L     R14,SVEXC               POP RET ADDR
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
*******************************************************
*    CLEAR - ERASE WORK AREAS                         *
*    ON ENTRY: R14 = RETURN ADDR                      *
*******************************************************
CLEAR    DS    0H
         ST    R14,SVCLE               PUSH RET ADDR
         STM   R1,R5,CLERR             PUSH
*
         LA    R2,=X'00'               SOURCE ADDRESS
         LA    R3,1                    GET SOURCE FLD LEN
         L     R4,=A(OUTSCRN)          GET DEST FLD ADDR
         L     R5,=A(OUTSCRNL-OUTSCRN) GET DEST FLD LEN
         MVCL  R4,R2                   MOVE PADDING ZEROES
*
         LM    R1,R5,CLERR             POP
         L     R14,SVCLE               POP RET ADDR
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
         C     R0,=F'0'        ANY CONDITION OTHER THAN EQUAL?
         BE    LFTEQU          NO, LEAVE BRANCHING ON EQUAL COND
         STC   R0,LFTSW+1      POKE CALLING PROCEDURE CONDITION
LFTEQU   DS    0H
******** LA    R7,STREND       END OF STRING
******** LA    R4,STRSTART     START OF STRING
******** LA    R3,=C'SCAN'     ADDRESS OF VALUE
******** LA    R2,4            LENGTH OF VALUE
         LR    R5,R4           ALSO R5 FOR COMPUTE
         LR    R6,R2           STEP BUMP
         BCTR  R2,0            DEBUMP FOR EX
LFTLP    DS    0H
         CLI   0(R4),X'00'     IS THIS A HEX 00?
         BE    LFTSKIP         YES, TREAT AS IF BLANK
         EX    R2,LFTCLC       IS IT THE VALUE?
LFTSW    BE    LFTEX           YES, EXIT
LFTSKIP  DS    0H
         BXLE  R4,R6,LFTLP     R4=IF(R4+R6<=R7,R4+R6 & GOTO,R4)
         LA    R15,1           SHOW VALUE NOT FOUND & END OF BUFF
         B     LFTEX1          EXIT
LFTEX    DS    0H
         LA    R15,2           SHOW VALUE FOUND
*
LFTEX1   DS    0H
         MVI   LFTSW+1,X'80'   RESET EQUAL CONDITION
         LR    R0,R4           SET LAST ADDRESS TESTED
         LM    R1,R9,LFTRR     POP REGS
         L     R14,SVLFT       POP RET ADDR
         BR    R14
*
LFTCLC   CLC   0(*-*,R4),0(R3)  COMPARE POS OF STRING WITH VALUE
*
*******************************************************
*     RITNBL - STRING 1ST RITE NON BLANK              *
*     ON ENTRY: R14 = RETURN ADDR                     *
*               R7  = START OF STRING                 *
*               R4  = END OF STRING                   *
*     ON EXIT:  R15 = ZERO = NOT FOUND / NON-BLK LEN  *
*               R0  = 1ST NON BLANK POS ADDR          *
*******************************************************
RITNBL   DS    0H
         ST    R14,SVRIT       PUSH RET ADDR
         STM   R1,R7,RITRR     PUSH REGS
******** LA    R7,START        START OF STRING
         LR    R5,R7           ALSO R5 FOR COMPUTE
******** LA    R4,END          END OF STRING
         LA    R3,R4           END OF STRING FOR COMPUTE
         L     R6,=F'-1'       ARG LEN
RITLP    DS    0H
         CLI   0(R4),X'00'     IS IT A HEX 00?
         BE    RITSKIP         YES, TREAT AS IF BLANK
         CLI   0(R4),C' '      IS IT A BLANK?
         BNE   RITEX           NO, EXIT
RITSKIP  DS    0H
         BXH   R4,R6,RITLP     DO WHILE (R4 + R6) > R7
         CLI   0(R4),C' '      IS IT A BLANK?
         BNE   RITEX           NO, EXIT
         SR    R15,R15         SHOW STRING EMPTY
         B     RITEX1          SKIP COMPUTE
RITEX    DS    0H
*         SR    R3,R4           GET HOW MUCH DID IT TRAVEL
*         CH    R3,=H'1'        JUST ONE POSITION?
*         BH    RITMORE         NO, MORE
*         LA    R4,1(R4)        PUT BYTE BACK
*RITMORE  DS    0H
         LR    R0,R4           COMPUTE
         SR    R0,R5           EXACT
         A     R0,=F'1'        NON BLANK INPUT LENGTH
         LR    R15,R0          IN R15
RITEX1   DS    0H
         LR    R0,R4           SHOW LAST ADDRESS
         LM    R1,R7,RITRR     POP REGS
         L     R14,SVRIT       POP RET ADDR
         BR    R14
*
*******************************************************
*     DISPSCR - DISPLAY SCREEN                        *
*     ON ENTRY: R14 = RETURN ADDR                     *
* ~   ON EXIT:  OUTPUT SCREEN WITH BOX                *
*******************************************************
DISPSCR  DS    0H
         ST    R14,SVDIS              PUSH RET ADDR
         STM   R1,R8,DISRR            PUSH REGS
         CLC   SCRBXPLS(4),=F'0'      ANY BOX
         BE    DISPNBOX               NO, SKIP BOX
         L     R8,SCRPLST             GET PROCESS PARAMETER LIST
         MVC   0(9,R8),SCRPARM        MOVE PARAM LIST
         L     R8,SCRBOXPL            GET BOX PARAMETER LIST
         MVC   0(12,R8),SCRBXPLS      MOVE BOX PARAM LIST
         CALL  OUTSTRN,SCRBOXI        GENERATE BOX ON PANEL
DISPNBOX DS    0H
         L     R2,=A(OUTSCRN)      OUTPUT SCREEN START ADDR
         L     R3,BUFFERLN         SCREEN BUFFER SPACE USED
         BAL   R14,PUSHST          GO PUSH DATA IN COMMON SCR BUFF
*
         LM    R1,R8,DISRR            POP REGS
         L     R14,SVDIS              POP RET ADDR
         BR    R14
*
*******************************************************
*     SHRINK: CONVERTS TWO CHAR DIGIT INTO ONE & MOVE *
*     ON ENTRY: R2 = INPUT COUPLE OF BYTES            *
*               R3 = OUTPUT SINGLE BYTE               *
*               R0 = OUTPUT FIELD LENGTH              *
*     ON EXIT:  CHARACTERS MOVED TO OUTPUT HEX DIGITS *
*******************************************************
SHRINK   DS    0H
         ST    R14,SVSHR           PUSH RET ADDR
         STM   R1,R6,SHRRR         PUSH REGS
         LR    R4,R0              INPUT LENGTH
*
SHRINKLP DS    0H
         NC    0(2,R2),=X'0F0F'   RESET ZONES TO 0
         SR    R5,R5              FIX FOR IC
         IC    R5,0(R2)           GET 1ST NUM CHAR
         LR    R6,R5              GET A COPY FOR COMPUTE
         MH    R6,=H'10'          SHIFT TO THE TENTH
         IC    R5,1(R2)           GET 2ND NUM CHAR
         AR    R6,R5              GET TENTH + UNITS
         STC   R6,0(R3)           STORE SHRUNK DIGITS
         LA    R2,2(R2)           BUMP INPUT
         LA    R3,1(R3)           BUMP OUTPUT
         BCT   R4,SHRINKLP        DO LOOP R4 TIMES
*
         LM    R1,R6,SHRRR         POP REGS
         L     R14,SVSHR           POP RET ADDR
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
***   INVOCACION:   BAL R14,ADDR3270                                ***
***********************************************************************
ADDR3270 DS    0H
         ST    R2,DIVRR           PUSH
         LH    R1,0(0,R1)         RESET COUNTER
         LA    R2,0               RESET COUNTER
DIVIDELP DS    0H
         CH    R1,=H'80'          LAST LINE?
         BL    LASTLINE           YES, LAST LINE
         SH    R1,=H'80'          DIVIDE
         LA    R2,1(0,R2)         COUNT
         B     DIVIDELP           LOOP
LASTLINE DS    0H
         SLL   R2,8               MAKE ROOM FOR COL
         OR    R2,R1              GET COLUMN
         STH   R2,0(0,R15)        SAVE
         L     R2,DIVRR           POP
         B     ADDREX           EXIT
*
         LH    2,0(1)           TAKE BINARY BUFFER ADDRESS
         STC   2,1(15)          SAVE IN TWO CHAR ANSWER FIELD +1
         SRL   2,6              TAKE FIRST PART
         STC   2,0(15)          SAVE IN TWO CHAR ANSWER FIELD +0
         NI    1(15),X'3F'      REMOVE EXCESS BITS
         TR    0(2,15),ADDBYTES TRANSFORM
ADDREX   DS    0H
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
DIVRR    DS    F          PUSH R2
*******************************************************
SAVESCR  DS    18F        PROGRAM SAVE AREA
*******************************************************
SVSSC    DS    F          SCANSCR PUSH RET ADDR
SSCRR    DS    9F         SCANSCR R1-R9 SAVE AREA
SVLFT    DS    F          LFTSCN PUSH RET ADDR
LFTRR    DS    9F         LFTSCN R1-R9 SAVE AREA
SVCLE    DS    F          CLEAR PUSH RET ADDR
CLERR    DS    5F         CLEAR R1-R5 SAVE AREA
SVSCC    DS    F          SCANCOM PUSH RET ADDR
SCCRR    DS    9F         SCANCOM R1-R9 SAVE AREA
SVTWE    DS    F          TWELVE PUSH RET ADDR
TWERR    DS    2F         TWELVE R1-R2 PUSH REGS
SVEXC    DS    F          EXCHAN PUSH RET ADDR
EXCRR    DS    2F         EXCHAN R1-R2 PUSH REGS
SVDIS    DS    F          DISPSCR PUSH RET ADDR
DISRR    DS    8F         DISPSCR R1-R8 PUSH REGS
SVRIT    DS    F          RITNBL PUSH RET ADDR
RITRR    DS    7F         RITNBL PUSH REGS
SVTRI    DS    F          TRIMRBLK PUSH RET ADDR
TRIRR    DS    9F         TRIMRBLK R1-R9 PUSH REGS
SVTLI    DS    F          TRIMLBLK PUSH RET ADDR
TLIRR    DS    9F         TRIMLBLK R1-R9 PUSH REGS
SVLON    DS    F          LONGMV PUSH RET ADDR
LONRR    DS    8F         LONGMV R1-R8 PUSH AREA
SVPUS    DS    F          PUSHTS PUSH RET ADDR
PUSRR    DS    8F         PUSHTS R1-R3 PUSH REGS
SVSHR    DS    F          SHRINK PUSH RET ADDR
SHRRR    DS    6F         SHRINK R1-R6 PUSH REGS
SVUDF    DS    F          UPDATEFL PUSH RET ADDR
UDFRR    DS    8F         UPDATEFL PUSH REGS
SVFLD    DS    F          FLDCOORD PUSH RET ADDR
FLDRR    DS    4F         FLDCOORD PUSH REGS
SVNUM    DS    F          NUMBERS PUSH RET ADDR
NUMRR    DS    3F         NUMBERS PUSH REGS
SVBUF    DS    F          BUFF327C PUSH RET ADDR
BUFRR    DS    2F         BUFF327C PUSH REGS
         DS    16F'0'     DELIMITER
*******************************************************
SCRSCRN  DC    V(SCREENL) COMMON OUTPUT STRING AREA ADDRESS
SCRPLST  DC    V(PARMLIST) COMMON OUTPUT STRING AREA ADDRESS
SCRPARM  DC    CL1' '       PARAMETER FOR CLEAR SCREEN AND PRINT
SCRCLSC  DC    AL1(0,0,0,0) CLEAR COORDINATES
SCRADDR  DC    AL4(0)       PANEL INSTRUCTIONS ADDRESS
******************************************************
SCRBOXPL DC    V(BOXPARML) BOX PARAMETER LIST ADDRESS
SCRBXPLS DC    AL1(6,3,21,77)  R/C R/C AND PARAMETER LIST
SCRBXCHR DC    CL8'...::...' BOX CHARACTERS
SCRCURSR DC    CL2'01'      INCERT CURSOR IN ROW
SCRCURSC DC    CL2'01'      INCERT CURSOR IN COLUMN
SCRBOXI  EQU   *            ETERNAL REF OR ENTRY POINT BUG
         DC    CL3'BOX'     BOX INDICATOR FOR CALL PROCESS
SCRLINI  DC    CL3'LIN'     LINE INDICATOR FOR CALL PROCESS
SCRPANI  DC    CL3' '       PANEL INDICATOR FOR CALL PROCESS
BUFFERLN DS    F            SCREEN BUFFER SPACE USED
BUFFERST DS    A(OUTSCRN)   FOR SCREEN ACCUMULATION
******************************************************
         DS    0D
         ENTRY CALLSCRN
CALLSCRN EQU   *
         DS    F          CALLING SCREEN BUFFER START
         ENTRY CALLSCRL
CALLSCRL EQU   *
         DS    F          CALLING SCREEN BUFFER END
******************************************************
INRECCN  DS    F              INPUT RECORD COUNT
         DC    C'HERE IS  RETURN CODE'
RETCODE  DS    F              PROGRAM RETURN CODE
FLDTABOF DC    A(USERFLDT)    FIELD TABLE OFFSET
FIELDCOO DC    XL2'0000'      OUTPUT HEX DIGITS
FIELDCOI DC    CL4' '         INPUT CHAR DIGITS
*******************************************************
REPETADD DC    F'0'      REPEAT TO ADDRESS
REPETCOM DC    X'3C'     REPEAT TO ADDRESS COMMAND
REPETSBA DC    X'0000'   REPEAT TO ADDRESS SET BUFFER ADDRESS
REPETCHR DC    X'40'     REPEAT TO ADDRESS CHARACTER
*******************************************************
*              HELP BLOCK DEFINITION
* C                      TR LC BR RC
* O                      OO EO OO IO
* M                      PW FL TW TL              +---------------+
* N                      .. T. T. E.              |COMMANDS ALWAYS|
* D ------ LEVEL ------- .. .. M. .. SUB-PROC     |START ON COL 1 |
* @,01000000000000000000,02,04,05,23,SUB-PROC     +---------------+
* T,TITULO DEL BLOQUE DE AYUDA
* ESTA ES UNA PRUEBA DE UN BLOQUE DE DEFINICION DE AYUDA, EL CUAL
* SIMPRE SERA ALINEADO PARA CABER EN EL CUADRO INDICADO.  TABIEN
* SE ALINEA POR LOS DOS LADOS.  EL FORMATO ES LIBRE DE LA COLUMNA
* 1 A LA 72.  ESTA DELIMITADO POR OTRO COMANDO DE BLOQUE O POR
* EL FINAL DEL MIEMBRO.  (VERSION MEJORADA).
*******************************************************
*        3270 ATTRIBUTE CHARACTER TABLE
*******************************************************
*  (01) UNPROT  ALPH/NUM                              MDT-OFF
*  (02) UNPROT  ALPH/NUM                              MDT-ON
*  (03) UNPROT  ALPH/NUM            PEN-DET           MDT-OFF
*  (04) UNPROT  ALPH/NUM            PEN-DET           MDT-ON
*  (05) UNPROT  ALPH/NUM  HI-INTNS  PEN-DET           MDT-OFF
*  (06) UNPROT  ALPH/NUM  HI-INTNS  PEN-DET           MDT-ON
*  (07) UNPROT  ALPH/NUM  --------  -------  NON-DSP  MDT-OFF
*  (08) UNPROT  ALPH/NUM  --------  -------  NON-DSP  MDT-ON
*
*  (09) UNPROT  NUMERIC                               MDT-OFF
*  (10) UNPROT  NUMERIC                               MDT-ON
*  (11) UNPROT  NUMERIC             PEN-DET           MDT-OFF
*  (12) UNPROT  NUMERIC             PEN-DET           MDT-ON
*  (13) UNPROT  NUMERIC   HI-INTNS  PEN-DET           MDT-OFF
*  (14) UNPROT  NUMERIC   HI-INTNS  PEN-DET           MDT-ON
*  (15) UNPROT  NUMERIC   --------  -------  NON-DSP  MDT-OFF
*  (16) UNPROT  NUMERIC   --------  -------  NON-DSP  MDT-ON
*
*  (17) PROTEC  ALPH/NUM                              MDT-OFF
*  (18) PROTEC  ALPH/NUM                              MDT-ON
*  (19) PROTEC  ALPH/NUM            PEN-DET           MDT-OFF
*  (20) PROTEC  ALPH/NUM            PEN-DET           MDT-ON
*  (21) PROTEC  ALPH/NUM  HI-INTNS  PEN-DET           MDT-OFF
*  (22) PROTEC  ALPH/NUM  HI-INTNS  PEN-DET           MDT-ON
*  (23) PROTEC  ALPH/NUM  --------  -------  NON-DSP  MDT-OFF
*  (24) PROTEC  ALPH/NUM  --------  -------  NON-DSP  MDT-ON
*
*  (25) PROTEC  AUTOSKIP                              MDT-OFF
*  (26) PROTEC  AUTOSKIP                              MDT-ON
*  (27) PROTEC  AUTOSKIP            PEN-DET           MDT-OFF
*  (28) PROTEC  AUTOSKIP            PEN-DET           MDT-ON
*  (29) PROTEC  AUTOSKIP  HI-INTNS  PEN-DET           MDT-OFF
*  (30) PROTEC  AUTOSKIP  HI-INTNS  PEN-DET           MDT-ON
*  (31) PROTEC  AUTOSKIP  --------  -------  NON-DSP  MDT-OFF
*  (32) PROTEC  AUTOSKIP  --------  -------  NON-DSP  MDT-ON
*******************************************************
ATTRTAB  DC    X'40C1C4C5C8C94C4D50D1D4D5D8D95C5D'
         DC    X'6061E4E5E8E96C6DF0F1F4F5F8F97C7D'
ATTRTABL EQU   *
*TTRTAB  DC    X'0102030405060708090A0B0C0D0E0F10'
*        DC    X'1112131415161718191A1B1C1D1E1F20'
*TTRTABL EQU   *
*******************************************************
         DC    C'HERE IS INPUT'
         ENTRY INSCRN
INSCRN   EQU   *
         DS    0CL1920         SCREEN OUTPUT BUFFER
         DC    CL256' '                              LINES 1-4
         DC    CL64' '                               LINES 1-4
         DC    CL80' '                               LINE 5
 DC CL4' ',CL33' ',X'15',C'HELP',X'11',CL33' ',CL4' ' LINE 6
         DC    CL4' ',X'11'        LINE 7
         DC    CL35'THIS IS AN EXAMPLE OF THE HELP. YOU'  LINE 7
         DC    CL35' CAN DEFINE ON A TEXT FILE THE HELP'  LINE 7
         DC    X'11',CL4' '        LINE 7
*
         DC    CL4' ',X'11'        LINE 8
         DC    CL35'INFORMATION FOR EACH OF THE ITEMS O'  LINE 8
         DC    CL35'N EVERY MENU, ALONG WITH THE FRAME '  LINE 8
         DC    X'11',CL4' '        LINE 8
*
         DC    CL4' ',X'11'        LINE 9
         DC    CL35'INFORMATION. THIS INFORMATION MUST '  LINE 9
         DC    CL35'BE LOCATED IN THE HELP.DAT FILE.   '  LINE 9
         DC    X'11',CL4' '        LINE 9
*
         DC    CL4' ',X'11'        LINE 10
         DC    CL35'THERE ARE TWO TYPES OF RECORDS: THE'  LINE 10
         DC    CL35' ITEM IDENTIFICATION RECORD AND THE'  LINE 10
         DC    X'11',CL4' '        LINE 10
*
         DC    CL4' ',X'11'        LINE 11
         DC    CL35'TEXT RECORD. THE ID RECORD CONTAINS'  LINE 11
         DC    CL35' THREE FIELDS: THE LEVEL FIELD (SA-'  LINE 11
         DC    X'11',CL4' '        LINE 11
*
         DC    CL4' ',X'11'        LINE 12
         DC    CL35'ME AS MENU TREE LEVEL), THE PROCESS'  LINE 12
         DC    CL35' FIELD AND THE FRAME COORDINATES.  '  LINE 12
         DC    X'11',CL4' '        LINE 12
*
         DC    CL4' ',X'11'        LINE 13
         DC    CL35'THE HELP TEXT CAN BE WRITTEN IN FRE'  LINE 13
         DC    CL35'E FORMAT. EXAMPLE OF USAGE:        '  LINE 13
         DC    X'11',CL4' '        LINE 13
*
         DC    CL4' ',X'11'        LINE 14
         DC    CL35'                                   '  LINE 14
         DC    CL35'                                   '  LINE 14
         DC    X'11',CL4' '        LINE 14
*
         DC    CL4' ',CL13' ',X'15'   LINE 18
         DC    CL43'C|...PROCESS LEVEL....|SUB-PRO.|TR,LC,BR,BC'
         DC    X'11',CL18' '          LINE 18
*
         DC    CL4' ',CL13' ',X'15'   LINE 19
         DC    CL43'@,01010203000000000000,IDENTIFC,05,03,17,70'
         DC    X'11',CL18' '          LINE 19
*
         DC    CL4' ',X'11'        LINE 20
         DC    CL35'  C=ID, TR=TOP ROW, LC=LEFT COLUMN,'  LINE 20
         DC    CL35' BR=BOTTOM ROW, BC=BOTTOM COLUMN.  '  LINE 20
         DC    X'11',CL4' '        LINE 20
*
         DC    CL4' ',X'11'        LINE 15
         DC    CL35'                                   '  LINE 15
         DC    CL35'                                   '  LINE 15
         DC    X'11',CL4' '        LINE 15
*
         DC    CL4' ',X'11'        LINE 16
         DC    CL35'             ENTER PASSWORD: AAAAAA'  LINE 16
         DC    CL35' (FIX FOR NOW)                     '  LINE 16
         DC    X'11',CL4' '        LINE 16
*
         DC    CL4' ',CL21' ',X'15'   LINE 17
         DC    CL27'                            '  LINE 17
         DC    X'11',CL26' '          LINE 17
*
*        DC    CL4' ',X'15',CL70'TITULO 2 ',X'11',CL4' ' LINE 8
*        DC    CL4' ',X'15',CL70'TITULO 3 ',X'11',CL4' ' LINE 9
*        DC    CL4' ',X'15',CL70'TITULO 4 ',X'11',CL4' ' LINE 10
*        DC    CL4' ',X'15',CL70'TITULO 5 ',X'11',CL4' ' LINE 11
*        DC    CL4' ',X'15',CL70'TITULO 6 ',X'11',CL4' ' LINE 12
*        DC    CL4' ',X'15',CL70'TITULO 7 ',X'11',CL4' ' LINE 13
*        DC    CL4' ',X'15',CL70'TITULO 8 ',X'11',CL4' ' LINE 14
*        DC    CL4' ',X'15',CL70'TITULO 9 ',X'11',CL4' ' LINE 15
*        DC    CL4' ',X'15',CL70'TITULO 10',X'11',CL4' ' LINE 16
*        DC    CL4' ',X'15',CL70'TITULO 11',X'11',CL4' ' LINE 17
*        DC    CL4' ',X'15',CL70'TITULO 12',X'11',CL4' ' LINE 18
*        DC    CL4' ',X'15',CL70'TITULO 13',X'11',CL4' ' LINE 19
*        DC    CL4' ',X'15',CL70'TITULO 14',X'11',CL4' ' LINE 20
         DC    CL256' '                              LINES 21-24
         DC    CL64' '                               LINES 21-24
         ENTRY INSCRNL
INSCRNL  EQU   *               DELIMITER ADDRESS
         DC    X'00'           DELIMITER VALUE
*******************************************************
         DC    C'HERE BUFFPRO OUT'
OUTSCRN  DS    CL4096          SCREEN OUTPUT BUFFER
OUTSCRNL EQU   *               DELIMITER
*******************************************************
         DC    C'HERE BUFFPRO FIELD TAB'
         DS    0F
         ENTRY USERFLDT
USERFLDT EQU   *
         DS    512XL8'0000000000000000' 512 USER FIELDS (4096 BYTES)
*        DC    H'0'            3270 BUFFER ADDRESS
*        DC    H'0'            FIELD LENGTH
*        DC    A(0)            FIELD ADDRESS
         ENTRY USERFLDL
USERFLDL EQU   *               DELIMITER
         DC    8X'00'          DELIMITER
INPSTR   DSECT
CLEARSC  DS    C               CLEAR SCREEN ORDER
INPCONV  DS    0CL20           USED TO CONVERT TO NUMERICS
CLSCORD  DS    CL8             CLEAR COORDINATES
INCROW   DS    CL2             INCERT ROW
INCCOL   DS    CL2             INCERT COLUMN
BOXTR    DS    CL2             BOX TOP ROW
BOXTLC   DS    CL2             BOX TOP LEFT COLUMN
BOXBR    DS    CL2             BOX BOTTOM ROW
BOXBRC   DS    CL2             BOX BOTTOM RITE COLUMN
BOXCHR   DS    CL8             BOX CHARACTERS
RESET    DS    CL1             SCREEN RESET (Y/N)
         COPY  \R42\CPY\EQUREGS.CPY
         END   BUFFPRO
