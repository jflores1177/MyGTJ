*******************************************************
*                   REFERENCES                        *
*******************************************************
*        MODULE - WINDOWS                             *
*                 MENU WINDOWS TREE PROCESSOR         *
*        SYSTEM - PC/370 CROSS ASSEMBLER              *
*        AUTHOR - BERNABE LLOVERA                     *
*        DATE   - SEPTEMBER 1995                      *
*******************************************************
*                                                     *
*     PROGRAM ENTRY POINT: WINDOWS                    *
*     OTHER ENTRIES:                                  *
*        TREELEV  = ACTUAL TREE LEVEL LOCATION        *
*        LEVELCD  = CHARACTER LEVEL CODE              *
*        SYSTEMCD = OUTPUT SYSTEM NAME                *
*        PROCTITL = PROCESS TITLE                     *
*        SUBSNAM  = INPUT SUB-SYSTEM                  *
*        HELP     = HELP PROGRAM                      *
*        APPLCURS = APPLICATION CURSOR LOCATION (COB) *
*                                                     *
*     EXTERNAL REFERENCES:                            *
*        SWDISCO  = COMMON AREA DISCONNECT SWITCH     *
*        SCREENL  = COMMON OUTPUT STRING AREA ADDRESS *
*        PARMLIST = COMMON OUTPUT STRING AREA ADDRESS *
*        BOXPARML = BOX PARAMETER LIST ADDRESS        *
*        TREETAB  = TREE TABLE ADDRESS POINTER        *
*        MENUTAB  = MENU TABLE ADDRESS POINTER        *
*        INSCRN   = COMMON WORK SCREEN ADDR           *
*        INSCRNL  = COMMON WORK SCREEN LIMIT ADDR     *
*        CALLSCRN = CALLING SCREEN BUFFER START       *
*        CALLSCRL = CALLING SCREEN BUFFER END         *
*        PANEL    = SAMPLE USER APPLICATION           *
*                                                     *
*******************************************************
*        ENTRY WINDOWS,TREELEV,LEVELCD,SYSTEMCD,PROCTITL,SUBSNAM
*        ENTRY APPLCURS,PANLENG
*        EXTRN SWDISCO,SCREENL,PARMLIST,BOXPARML,MENUTAB,TREETAB
*        EXTRN INSCRN,INSCRNL,HELP,PANEL,BINOD
*        EXTRN CALLSCRN,CALLSCRL
*
*        PRINT NOGEN
         TITLE '                                * * *   WINDOWS  * * *'
**************************************************************
*        WINDOWS ROUTINE
**************************************************************
WINDOWS  CSECT
         ENTRY
XX4      STM   R14,R12,12(R13) SAVE IN CALLERS SAVE AREA
         LR    R12,R15         USE ENTRY ADDRESS
         USING XX4,R12,R11
         LA    R15,XX4SAV1     GET PROGRAMS SAVE AREA ADDR
         ST    R13,4(R15)      SAVE CALLERS SAVE AREA ADDR
         ST    R15,8(R13)      SAVE PROGRAMS SAVE AREA IN CALLERS AREA
         LR    R13,R15         GET PROGRAMS SAVE AREA ADDR
         L     R11,XX4SAV2     LOAD 2ND BASE REG
         B     AROUND          SKIP SAVE AREA FLD
XX4SAV1  DS    18F             RETURN SAVE REGS AREA
XX4SAV2  DC    A(XX4+4096)     2ND BASE REG ADDR
         DC    CL12'<<WINDOWS>>'  DUMP PROGRAM ID
AROUND   DS    0H
*
         MVI   PROPARM,C'C'         CLEAR SCREEN
         L     R10,0(R1)            GET PARAM LIST ADDRESS
         USING INPSTR,R10           ADDRESSABILITY
         MVC   LASTKEY,KEYCODE      SAVE LAST KEY PRESSED FOR TEST
*********************************************************************
* IF ALREADY IN SOME APPLICATION PROCEDURE GO AHEADE AND DO IT
         CLC   PROCEDUR,=F'0'       ANY PROCEDURE ADDRESS?
         BE    PRONPRO              NO, DO NO PROCEDURE
         CALL  BUFFPROI,PARLEXT     CONVERT FROM VTAM TO APPL
         MVC   INPUTKEY,KEYCODE     INPUT KEY TO PASS TO APPLICATION
         L     R15,PROCEDUR         GET PROCEDURE ADDRESS
         CALL  (15),PROCEDPL        CALL APPL & PASS PARAMETER LIST
         CLC   PROSWIT(3),=C'   '   'END'|'EXT'=NO RETURN '   '=RETURN
         BE    PRORET               YES, PRINT THIS PANEL
         CLC   PROSWIT(3),=C'END'   'END'|'EXT'=NO RETURN '   '=RETURN
         BNE   PROTEXIT             NO, ASSUME IMMEDIATE EXIT
         MVC   PROSWIT(3),=C'   '   'END'|'EXT'=NO RETURN '   '=RETURN
         XC    PROCEDUR,PROCEDUR    RESET PROCEDURE ADDRESS
         B     PRORET               PRINT LAST PANEL
PROTEXIT DS    0H
         MVC   PROSWIT(3),=C'   '   'END'|'EXT'=NO RETURN '   '=RETURN
         XC    PROCEDUR,PROCEDUR    RESET PROCEDURE ADDRESS
         MVI   KEYCODE,X'7D'        SIMULATE AN ENTER
         MVC   CURLOCT(2),=X'0000'  ERASE CURSOR TO AVOID PROCEDURE
PRONPRO  DS    0H                   ELSE, ASSUME IMMEDIATE EXIT
         BAL   R14,RESTPANL         RESTORE PANEL LENGTH
*********************************************************************
         MVI   MOVUPDN,X'00'        DEFAULT SWITCH TO DOWN
*
         CLI   KEYCODE,X'7A'        IS THIS A LEFT MOVE?
         BE    PROPF4               YES, MOVE LEFT
         CLI   KEYCODE,X'7B'        IS THIS A RITE MOVE?
         BNE   PROOTHER             NO, TRY OTHERS
PROPF4   DS    0H
         BAL   R14,SIDEWAY          MOVE SIDE WAYS
         B     PROENTER             ASSUME ENTER
*
PROOTHER DS    0H
         CLI   KEYCODE,X'7D'        IS THIS AN ENTER?
         BE    PROENTER             YES, GO TO ENTER ROUTINE
         CLI   KEYCODE,X'F1'        OR IS THIS AN HELP REQ?
         BE    PROENTER             YES, GO TO ENTER ROUTINE
         CLI   KEYCODE,X'F3'        IS THIS AN ESCAPE?
         BE    PROESCAP             YES, GO TO ESCAPE ROUTINE
         B     PROWRONG             ELSE, RETURN BAD CODE
*
PROESCAP DS    0H
         MVI   MOVUPDN,X'01'        SET MOVE UP SWITCH
         LA    R4,TREELEV           STRING START FOR SCAN
         LA    R7,TREELEVL          STRING LIMIT FOR SCAN
         LA    R3,=X'00'            VALUE ADDREESS
         LA    R2,1                 VALUE LENGTH
         BAL   R14,LFTSCN           SCAN FOR X'00'
         LR    R2,R0                SWITCH REGS
         BCTR  R2,0                 DECREMENT TO POKE X'00'
         MVI   0(R2),X'00'          POKE 00 TO TAKE A MENU OFF
         L     R8,SWDSCN            GET DISCONNECT SWITCH ADDR
         MVI   0(R8),X'00'          TURN DISC SWITCH ON
         MVC   CURLOCT(2),=X'0000'  ERASE CURSOR
*                                   IF THERE'S STILL A BOX,
*                                   IT WILL TURN IT BACK OFF
PROENTER DS    0H
*        CURLOCT  DS    XL2             INPUT CURSOR LOCATION
*        ORDERCD  DS    X               INPUT ORDER CODE
*        FLDLOCT  DS    XL2             INPUT FIELD LOCATION
*        INPDATA  DS    C               INPUT DATA STRING
         CLI   ORDERCD,X'11'        ANY ORDERS BACK?
         BNE   PRONOORD             NO, SKIP ORDER PROCESSING
         CLI   INPDATA,C' '         INPUT DATA STRING = BLANKS?
         BE    PRONOORD             NO, SKIP ORDER PROCESSING
         MVC   CURLOCT,FLDLOCT      USE FLD ADDR INSTEAD OF CURSOR
PRONOORD DS    0H
         LA    R5,1                 LEVEL NUMBER OF CHARS
PROMNLP1 DS    0H
         L     R4,=V(MENUTAB)       GET MENU TABLE ADDR
*        LA    R4,MENUTAB           GET MENU TABLE
PROMNLP2 DS    0H
         CLI   0(R4),X'00'          IS THIS THE END?
         BE    PROWRONG             YES, THIS IS AN ERROR
         LR    R7,R5                GET COMPARE LENGTH
         BCTR  R7,0                 ADJUST FOR EX
         LA    R6,TREELEV(R7)       GET NEXT LEVEL (CODE + 1)
         CLI   0(R6),X'00'          LAST LEVEL TO PRINT?
         BE    PROLAST              YES, SEE CURSOR FOR PROCESS
         EX    R7,PROLEVCM          COMPARE TREE LEVEL
         BE    PROCLEV              GO PRINT THIS LEVEL
         LA    R4,32(R4)            BUMP MENU TABLE ADDR
         B     PROMNLP2             LOOP
*
PROLEVCM CLC   TREELEV(*-*),0(R4)   LEVEL COMPARE
*
PROCLEV  DS    0H
         CLI   19(R4),C'V'          IS THIS ANOTHER MENU?
         BE    PROUPD1              YES, UPDATE ATTR & DISP MENU
         CLI   19(R4),C'H'          IS THIS ANOTHER MENU?
         BNE   PROPROCE             NO, IT'S A PROCESS
*        CLI   19(R4),X'01'         IS THIS ANOTHER MENU?
*        BNE   PROPROCE             NO, IT'S A PROCESS
PROUPD1  DS    0H
         BAL   R14,UPDTMEN          UPDATE ATTR ON MENU TABLE
         BAL   R14,DISPMEN          DISPLAY MENU
         MVI   PROPARM,C'N'         CLEAR NO MORE SCREEN
         LA    R5,1(R5)             SET FOR NEXT LEVEL
         B     PROMNLP1             SEE NEXT LEVEL
*
PROLAST  DS    0H
         CLI   MOVUPDN,X'01'        ARE WE GOING UP?
         BE    PROCURSR             YES, PRINT LAST MENU CURSORS
*
         L     R2,LASTMENU          LOAD MENU POINTER
         CLC   10(4,R2),=F'0'       ANY CURSOR TEST?
         BE    PROERROR             NO, PRINT ERROR AND CURSORS
         SR    R3,R3                FOR IC
         IC    R3,18(R2)            LOAD LOOP COUNT
         LR    R4,R3                SAVE FOR COMPUTE
         ICM   R2,B'1111',10(R2)    AND ADDRESS
PROTRELP DS    0H
         CLC   0(2,R2),CURLOCT      IS THIS THE WINDOW WHANTED?
         BE    PROCON               YES, PRINT IT
         LA    R2,2(R2)             BUMP CURSOR TAB PTR
         BCT   R3,PROTRELP          DO WHILE R3<=4
         CLI   KEYCODE,X'F1'        OR IS THIS AN HELP REQ?
         BNE   PROCURSR             NO, CURSOR AT WRONG LOC
         MVC   PROCEDUR,=V(HELP)    SAVE PROCEDURE ADDRESS
         B     PROHELP1             DO HELP WITH LAST PARMS
*
PROCON   DS    0H
         BCTR  R3,0                 ADJUST FOR DISPL IN TAB
         SR    R4,R3                GET POSITION IN TABLE
         STC   R4,TREELEV(R7)       UPDATE TREE LEVEL
         CLI   KEYCODE,X'F1'        OR IS THIS AN HELP REQ?
         BE    PROHELP              YES, DO HELP ROUTINE
         L     R4,=V(MENUTAB)       GET MENU TABLE ADDR
*        LA    R4,MENUTAB           GET MENU TABLE
PROMNLP3 DS    0H
         CLI   0(R4),X'00'          IS THIS THE END?
         BNE   PROSTIL              NO, STIL SOME MORE
         SR    R3,R3                GET A ZERO
         STC   R3,TREELEV(R7)       ERASE TREE LEVEL
         B     PROERROR             YES, THIS IS AN ERROR
PROSTIL  DS    0H
         CLC   0(10,R4),TREELEV     DID WE FIND THE MENU OR PROCESS?
         BNE   PROANOTH             NO, KEEP LOOPING
         CLI   19(R4),C'V'          IS THIS ANOTHER MENU?
         BE    PRODISP1             YES, DISPLAY MENU
         CLI   19(R4),C'H'          IS THIS ANOTHER MENU?
         BNE   PROPROCE             NO, IT'S A PROCESS
*        CLI   19(R4),X'01'         IS THIS ANOTHER MENU?
*        BNE   PROPROCE             NO, IT'S A PROCESS
*
PRODISP1 DS    0H
         BAL   R14,DISPMEN          DISPLAY MENU
PROCURSR DS    0H
         L     R1,LASTMENU          LOAD MENU POINTER
         LA    R1,24(R1)            CURSOR TABBLE POINTER
         BAL   R14,MOVEORDL         MOVE ORDER LIST TO PANEL BUFF
         B     PRORET               AND EXIT
PROPROCE DS    0H
PROHELP  DS    0H
         CLI   KEYCODE,X'F1'        OR IS THIS AN HELP REQ?
         BNE   PRONHELP             NO, SKIP HELP PROCESS
*
         MVC   PROCEDUR,=V(HELP)    SAVE PROCEDURE ADDRESS
PROCALLP DS    0H
         L     R1,LASTMENU          LOAD MENU POINTER
         LA    R1,24(R1)            CURSOR TABLE POINTER
         BAL   R14,MOVEORDL         MOVE ORDER LIST TO PANEL BUFF
         BAL   R14,EXPAND           SAVE LEVEL CODE
PROHELP1 DS    0H
         BAL   R14,SAVEPANL         SAVE PANEL LENGTH TO WORK WITH
         MVC   PROSWIT(3),=C'   '   'END'=DON'T GO BACK '   '=GO BACK
         MVC   INPUTKEY,KEYCODE     INPUT KEY TO PASS TO APPLICATION
         L     R15,PROCEDUR         GET PROCEDURE ADDRESS
         CALL  (15),PROCEDPL        CALL APPL & PASS PARAMETER LIST
         CLC   PROSWIT(3),=C'END'   IS APPLICATION ENDING?
         BNE   PROMORE              NO, STIL MORE WITH APPLICATION
PRORESET DS    0H
         MVC   PROSWIT(3),=C'   '   'END'=DON'T GO BACK '   '=GO BACK
         XC    PROCEDUR,PROCEDUR    RESET PROCEDURE ADDRESS
PROMORE  DS    0H
         SR    R3,R3                GET A ZERO
         STC   R3,TREELEV(R7)       RESET TREE LEVEL
         B     PRORET               RETURN
*
PRONHELP DS    0H
         CLI   19(R4),C'S'          IS THIS ANOTHER SYSTEM?
         BNE   PRONOSYS             NO, ONLY SUB-SYSTEMS FOR NOW
         CLC   24(8,R4),=CL8' '     ANY SYSTEM NAME?
         BE    PRONOSYS             NO, NO SYSTEM NAME
         MVC   SUBSNAM(8),24(R4)    SAVE FOR CALL PARM
         CALL  VAL,SUBSNAM          TRY TO GET NEXT SUB-SYSTEM
         LTR   R15,R15              ANY LUCK?
         BNZ   PRONOSYS             NO, SEND ERR MESS
*******************************************************
*   RESET SYSTEM FOR NEW MENU TREE ~                  *
*******************************************************
         MVI   PROPARM,C'C'         CLEAR SCREEN
         L     R8,SWDSCN            GET DISCONNECT SWITCH ADDR
         MVI   0(R8),X'00'          TURN DISC SWITCH ON
         MVI   MOVUPDN,X'00'        DEFAULT SWITCH TO DOWN
         MVC   TREELEV(10),FIRSTRE  RESET TREE LEVEL TO 1ST
         XC    LASTMENU,LASTMENU    RESET LAST MENU PRINTED ADDR
         MVC   CURLOCT(2),X'0000'   ERASE CURSOR
         MVC   SYSTEMCD(8),SUBSNAM  SAVE SYSTEM NAME
         B     PRONOORD             START FROM THE BEGGINING
PRONOSYS DS    0H
         CLI   19(R4),C'P'          IS THIS A FUNCTION PROCESS?
         BNE   PRONOFUN             NO, SKIP FUNCTION PROCESSING
         CLC   24(8,R4),=CL8' '     ANY SYSTEM NAME?
         BE    PRONOFUN             NO, NO FUNCTION NAME
         MVC   FUNCNAM(8),24(R4)    SAVE FOR VALIDATION & LINK ROUT
         BAL   R14,FUNCVAL          DO FUNCTION ADDRESS TAB EXTACCION
         LTR   R15,R15              ANY LUCK?
         BZ    PRONOFUN             NO, SEND ERR MESS
         ST    R15,PROCEDUR         SAVE PROCEDURE ADDRESS
         B     PROCALLP             GO TO COMMON PROCEDURE CALL ROUT
*
PRONOFUN DS    0H
         SR    R3,R3                  GET A ZERO
         STC   R3,TREELEV(R7)         RESET TREE LEVEL
PROERROR DS    0H
         LA    R1,=A(ERRMSG)        MOVE NOT ACTIVE MESSAGE
         BAL   R14,MOVEORDL         MOVE ORDER LIST TO PANEL BUFF
         B     PROCURSR               PRINT LAST CURSORS
PROANOTH DS    0H
         LA    R4,32(R4)            BUMP MENU TABLE ADDR
         B     PROMNLP3             LOOP
PRORET   DS    0H
         SR    R15,R15              SET OK RETURN CODE
         B     PROEXIT              EXIT
*
PROWRONG DS    0H
         LA    R15,1                SET (NOT VALID) RETURN CODE
PROEXIT  DS    0H
         ST    R15,RETCODE
         EXIT  SAVE=YES,RC=RETCODE
*
*******************************************************
*                   SUBROUTINES                       *
*******************************************************
*     FUNCVAL  - PROCESS FUNTION VALIDATION & LINK    *
*     ON ENTRY: R14 = RETURN ADDR                     *
*          FUNCNAM  = FUNCTION NAME FROM TREE FILE    *
*     ON EXIT:  R15 = 0 = NOT FOUND | FUNC ADDRESS    *
*******************************************************
FUNCVAL  DS    0H
         ST    R14,SVFUN            PUSH RET ADDR
         STM   R1,R2,FUNRR          PUSH REGS
         SR    R15,R15              DEFAULT R15 TO SHOW ERROR
         LA    R2,PROCETAB          PROCESS TABLE ADDR
FUNCVLP  DS    0H
         CLC   0(8,R2),=CL8' '      ARE WE AT TABLE END?
         BE    FUNCVEX              YES, EXIT
         CLC   0(8,R2),FUNCNAM      IS THIS THE FUNCION?
         BNE   FUNCVMOR             NO, SEACH MORE
         L     R15,8(R2)            GET FUNCTION ADDRESS
         B     FUNCVEX              & EXIT
FUNCVMOR DS    0H
         LA    R2,12(R2)            BUMP TO NEXT ARGUMENT
         B     FUNCVLP              LOOP UNTIL BLANK NAME FOUND
*
FUNCVEX  DS    0H
         LM    R1,R2,FUNRR          POP REGS
         L     R14,SVFUN            POP RET ADDR
         BR    R14
*
*******************************************************
*     MOVEORDL - MOVE ORDER LIST                      *
*                (DATA TO COMMON PANEL BUFFER)        *
*     ON ENTRY: R14 = RETURN ADDR                     *
*               R1 = ORDER LIST ADDRESS               *
*     ON EXIT:  PANEL DATA MOVED                      *
*******************************************************
MOVEORDL DS    0H
         ST    R14,SVMVP            PUSH RET ADDR
******** L     R1,LASTMENU          LOAD MENU POINTER + CURSOR TAB
         L     R8,PROPLST           GET PROCESS PARAMETER LIST
         MVI   0(R8),C'N'           INDICATE NO CLEAR SCREEN
         MVC   5(4,R8),0(R1)        MOVE ORDER LIST ADDRESS
         CALL  OUTSTRN,PROPANI      GENERATE OUT STRING / ADDR LIST
         L     R14,SVMVP            POP RET ADDR
         BR    R14
*
*******************************************************
*     SAVEPANL - SAVE PANEL USED UP SPACE TO BE       *
*                RESTORES AFTER SEND. THIS PREVENTS   *
*                FROM RUNNIG OFF PANEL BUFFER END     *
*                APPLICATION PROGRAM USES SAME PANEL  *
*                BUFFER AREA.                         *
*     ON ENTRY: R14 = RETURN ADDR                     *
*     ON EXIT:  PANEL LENGTH SAVED                    *
*******************************************************
SAVEPANL DS    0H
         CLC   PANLENG(4),=F'0'     IS IT ALREADY SAVED?
         BNE   SAVENOT              YES, DON'T SAVE IT
         L     R15,=V(SCREENL)      POINT TO COMMON OUTPUT AREA LEN
         L     R15,0(R15)           LOAD USED SPACE VALUE
         ST    R15,PANLENG          SAVE PANEL LENGTH
SAVENOT  DS    0H
         BR    R14
*
*******************************************************
*     RESTPANL - RESET PANNEL TO NORMAL PROCESSING    *
*                KEEPING LAST MESSAGE FROM APPLICATION*
*     ON ENTRY: R14 = RETURN ADDR                     *
*     ON EXIT:  PANEL LENGTH SAVED                    *
*******************************************************
RESTPANL DS    0H
         ST    R14,SVRSP            PUSH RET ADDR
         STM   R1,R8,RSPRR          PUSH REGS
*
         CLC   PANLENG(4),=F'0'     ANY PANEL RESTORE LENGTH?
         BE    NOPANLEN             NO, SKIP RETORE
         CLC   PANLENG(4),=F'4096'  ARE WE WITHIN LENGTH?
         BH    NOPANLEN             NO, SKIP RETORE
         L     R8,=V(SCREENL)       POINT TO COMMON OUTPUT AREA LEN
         L     R7,PANLENG           GET FORMER PANEL LENGTH
         ST    R7,0(R8)             RESTORE FORMER PANEL LENGTH
NOPANLEN DS    0H
         XC    PANLENG(4),PANLENG   ERASE PANEL LENGTH BUFF
*
         LM    R1,R8,RSPRR          POP REGS
         L     R14,SVRSP            POP RET ADDR
         BR    R14
*
*******************************************************
*     SIDEWAY - SIDE WAYS MOVEMENT (PF10=<- PF11=->)  *
*     ON ENTRY: R14 = RETURN ADDR                     *
*     ON EXIT:  SWITCHED TREE LEVEL                   *
*******************************************************
SIDEWAY  DS    0H
         ST    R14,SVSID              PUSH RET ADDR
         STM   R1,R8,SIDRR            PUSH REGS
         L     R3,=V(MENUTAB)         GET MENU TABLE ADDR
*        LA    R3,MENUTAB             GET MAIN MENU
         SR    R2,R2                  SET FOR IC
         IC    R2,18(R3)              LOAD LOOP COUNT
         ICM   R5,B'1111',10(R3)      LOAD INPUT CURSOR TAB ADDR
         BCTR  R2,0                   MAKE IT RELATIVE TO 0
*
         SR    R3,R3                  SET FOR IC
         IC    R3,TREELEV+1           GET 2ND TREE LEVEL
         LTR   R3,R3                  PRESENTLY AT 1ST LEVEL?
         BZ    SIDEEX                 YES, EXIT
         BCTR  R3,0                   MAKE IT RELATIVE TO 0
*
         CLI   KEYCODE,X'7A'          IS THIS PF10=LEFT?
         BNE   SIDEF11                NO, IT'S A PF11=RITE
         CH    R3,=H'0'               ARE WE AT THE 1ST OPTION?
         BH    SIDENOT1               NOT 1ST, DON'T WRAP AROUND
         LR    R3,R2                  GET HIEST OPTION
         B     SIDECOMP               COMPUTE DISPLACEMENT
SIDENOT1 DS    0H
         BCTR  R3,0                   DECREMENT TO GO LEFT
         B     SIDECOMP               COMPUTE DISPLACEMENT
*
SIDEF11  DS    0H
         CR    R3,R2                  ARE WE AT THE LAST OPTION?
         BNE   SIDENOTL               NOT LAST, DON'T WRAP AROUND
         LA    R3,0                   GET 1ST OPTION
         B     SIDECOMP               COMPUTE DISPLACEMENT
SIDENOTL DS    0H
         LA    R3,1(R3)               INCREMENT TO GO RITE
         B     SIDECOMP               COMPUTE DISPLACEMENT
*
SIDECOMP DS    0H
         LR    R2,R3                  SAVE FOR CURSOR SETTING
         MH    R2,=H'2'               DISPLACEMENT INTO CURSOR TAB
         AR    R2,R5                  GET INPUT CUR TAB ARG
*        LA    R2,M000000(R2)         GET TAB ARG
         MVC   CURLOCT(2),0(R2)       UPDATE INPUT CURSOR FIELD
         MH    R3,=H'10'              DISPLACEMENT INTO TREE TAB
         A     R3,=V(TREETAB)         GET TAB ARG
*        LA    R3,TREETAB(R3)         GET TAB ARG
         MVC   TREELEV(10),0(R3)      UPDATE TREE LEVEL
SIDEEX   DS    0H
         LM    R1,R8,SIDRR            POP REGS
         L     R14,SVSID              POP RET ADDR
         BR    R14
*
*******************************************************
*     DISPMEN - DISPLAY MENU                          *
*     ON ENTRY: R14 = RETURN ADDR                     *
*     ON EXIT:  OUTPUT MENU & UPDATE TREE LEVEL       *
*******************************************************
DISPMEN  DS    0H
         ST    R14,SVDIS              PUSH RET ADDR
         STM   R1,R8,DISRR            PUSH REGS
         ST    R4,LASTMENU            SAVE FOR CURSOR SEARCH
         MVC   PROADDR,14(R4)         MOVE TEXT ADDRESS
         CLC   20(4,R4),=F'0'         ANY BOX OR LINE?
         BE    DISPNBOX               NO, SKIP BOX OR LINE
         MVC   PROBXPLS(4),20(R4)     MOVE COORDINATES
         L     R8,PROPLST             GET PROCESS PARAMETER LIST
         MVC   0(9,R8),PROPARM        MOVE PARAM LIST
         L     R8,PROBOXPL            GET BOX PARAMETER LIST
         MVC   0(12,R8),PROBXPLS      MOVE BOX PARAM LIST
         CLI   19(R4),C'H'            HORIZONTAL MENU?
         BNE   DISPNHOR               NO, SKIP LINE DO BOX
         CALL  OUTSTRN,PROLINI        GENERATE LINE ON PANEL
         MVI   PROPARM,C'N'           CLEAR NO MORE SCREEN
*        L     R8,=V(PARMLIST)        GET PROCESS PARAMETER LIST
*        MVI   0(R8),C'N'             INDICATE NO CLEAR SCREEN
*        MVC   5(4,R8),=A(NORINTEN)   NORMAL INTENSITY ATTRIBUTE
*        CALL  OUTSTRN,PROPANI        GENERATE OUT STRING / ADDR LIST
         B     DISPNBOX               SKIP DISCONECT SWITCH TAMPER
*        B     DISPDISC               GO SET DISCONECT SW
DISPNHOR DS    0H
         CALL  OUTSTRN,PROBOXI        GENERATE BOX ON PANEL
DISPDISC DS    0H                   *** FOR LAST MENU ***
         L     R8,SWDSCN            GET DISCONNECT SWITCH ADDR
         MVI   0(R8),X'01'          TURN DISC SWITCH OFF
DISPNBOX DS    0H
         L     R8,PROPLST             GET PROCESS PARAMETER LIST
         MVC   0(9,R8),PROPARM        MOVE PARAM LIST
         CALL  OUTSTRN,PROPANI        GENERATE OUT STRING / ADDR LIST
         SR    R3,R3                  SET FOR IC
         IC    R3,TREELEV+1           GET 2ND TREE LEVEL
         LTR   R3,R3                  PRESENTLY AT 1ST LEVEL?
         BZ    DISNOTR                YES, DON'T SAVE TREE LEVEL
         BCTR  R3,0                   MAKE IT RELATIVE TO 0
         MH    R3,=H'10'              DISPLACEMENT INTO TREE TAB
         A     R3,=V(TREETAB)         GET TAB ARG
*        LA    R3,TREETAB(R3)         GET TAB ARG
         MVC   0(10,R3),TREELEV       SAVE TREE LEVEL
DISNOTR  DS    0H
         LM    R1,R8,DISRR            POP REGS
         L     R14,SVDIS              POP RET ADDR
         BR    R14
*
*******************************************************
*     UPDTMEN - UPDATE MENU WITH ATTRIBUTE AND CURSOR *
*     ON ENTRY: R14 = RETURN ADDR                     *
*                R4 = MENU ITEM NUMBER                *
*     ON EXIT:  MENU UPDATED                          *
*******************************************************
UPDTMEN  DS    0H
         ST    R14,SVUPD            PUSH RET ADDR
         STM   R1,R8,UPDRR          PUSH REGS
*
*        LA    R2,TREELEV(R5)       GET NEXT LEVEL CODE
*        CLI   0(R2),X'00'          IS IT A NEW MENU?
*        BNE   UPDTEX               NO, EXIT
*        CLI   MOVUPDN,X'01'        ARE WE GOING UP?
*        BE    UPDTEX               YES, EXIT
*
         LA    R0,1                 SET CURSOR SWITCH TO FOUND
         CLC   10(4,R4),=F'0'       ARE THERE ANY CURSORS?
         BE    UPDTEX               NO, EXIT
         SR    R3,R3                FOR IC
         IC    R3,18(R4)            LOAD LOOP COUNT
         LR    R5,R3                SAVE FOR COMPUTE
         ICM   R2,B'1111',10(R4)    WINDOW TABLE ADDRESS
         ICM   R6,B'1111',24(R4)    CURSOR TABLE ADDRESS
UPDTLP1  DS    0H
         CLC   0(2,R2),CURLOCT      IS THIS THE WINDOW WHANTED?
         BE    UPDTCON              YES, GO GET TABLE LOC
         LA    R2,2(R2)             BUMP WINDOW TAB PTR
         LA    R6,7(R6)             BUMP CURSOR TAB PTR
         BCT   R3,UPDTLP1           DO WHILE R3<=N
         LR    R3,R5                RESTORE TO POINT TO 1ST TAB LOC
         LA    R0,0                 SET CURSOR SWITCH TO NOT FOUND
*        B     UPDTEX               EXIT
*
UPDTCON  DS    0H
         SR    R2,R2                FOR IC
         IC    R2,18(R4)            LOAD LOOP COUNT
         MH    R2,=H'7'             GO TO END OFF CURSOR TAB
         ICM   R1,B'1111',24(R4)    CURSOR TABLE ADDRESS
         LA    R2,0(R2,R1)          GET CURSOR ADDRESS + LEN
         MVC   0(2,R2),0(R6)        MOVE SCREEN ADDR TO CUR COMM
*
         BCTR  R3,0                 ADJUST FOR DISPL IN TAB
         SR    R5,R3                GET POSITION IN TABLE
         ICM   R8,B'1111',14(R4)    TABLE ADDRESS
         XR    R7,R7                FOR IC
         IC    R7,18(R4)            TABLE ARG COUNT
         LA    R6,1                 LOOP COUNTER
UPDTLP2  DS    0H
         CLI   0(R8),X'00'          WAS THIS ALL FOR NOW?
         BE    UPDTEX               YES, EXIT
*
         LTR   R0,R0                ARE WE UPDATING ATTRIBUTES?
         BZ    UPDTGO               NO, SKIP UPTATING
         CR    R6,R5                OPTION WANTED?
         BNE   UPDT60               NO, UPDATE ATTR WITH X'60'
         MVI   2(R8),X'E8'          YES, UPDATE ATTR WITH X'E8'
         B     UPDTGO               AND GO ON
UPDT60   DS    0H
         MVI   2(R8),X'E4'          YES, UPDATE ATTR WITH X'60'
*
UPDTGO   DS    0H
         XR    R3,R3                CLEAR R3
         IC    R3,3(R8)             GET STRING LENGTH
*
         CH    R6,=H'1'             1ST OPTION?
         BE    UPDTMOV              YES, MOVE TITLE JUST IN CASE
         CLI   2(R8),X'E8'          IS THIS THE ONE WE WANT?
         BNE   UPDTSKM              NO, SKIP MOVE
UPDTMOV  DS    0H
         MVI   PROCTITL,C' '        ERASE PROCESS TITLE
         MVC   PROCTITL+1(61),PROCTITL ERASE PROCESS TITLE
         LR    R15,R3               GET A COPY OF THE LENGTH
         CLI   19(R4),C'H'          HORIZONTAL MENU?
         BE    UPDTDEC1             YES, DECREMENT ONLY 1
         SH    R15,=H'2'            GET RID OF EXESS BYTES (HORIZ)
UPDTDEC1 DS    0H
         BCTR  R15,0                ADJUST FOR EX
         EX    R15,MVCPOCTL         SAVE PROCESS TITLE
*
UPDTSKM  DS    0H
         LA    R8,4(R3,R8)          BUMP INPUT ADDRESS
         LA    R6,1(R6)             BUMP LOOP COUNTER
         BCT   R7,UPDTLP2           DO THRU WHOLE TABLE
UPDTEX   DS    0H
         LM    R1,R8,UPDRR          POP REGS
         L     R14,SVUPD            POP RET ADDR
         BR    R14
         DROP  R10
*
MVCPOCTL MVC   PROCTITL(*-*),4(R8)  MOVE PROCESS TITLE
*
*******************************************************
*     LFTSCN -  SCAN VALUE FROM LEFT TO RIGHT         *
*     ON ENTRY: R14 = RETURN ADDR                     *
*               R7  = END OF STRING                   *
*               R4  = START OF STRING                 *
*               R3  = ADDRESS OF VALUE TO SCAN        *
*               R2  = LENGTH OF VALUE TO SCAN         *
*     ON EXIT:  R0  = ADDRESS OF VALUE OR ZERO        *
*******************************************************
LFTSCN   DS    0H
         ST    R14,SVLFT       PUSH RET ADDR
         STM   R1,R7,LFTRR     PUSH REGS
******** LA    R7,STREND       END OF STRING
******** LA    R4,STRSTART     START OF STRING
******** LA    R3,=C'SCAN'     ADDRESS OF VALUE
******** LA    R2,4            LENGTH OF VALUE
         BCTR  R2,0            DEBUMP FOR EX
         LR    R5,R4           ALSO R5 FOR COMPUTE
         LA    R6,1            STEP BUMP
LFTLP    DS    0H
         EX    R2,LFTCLC       IS IT THE VALUE?
         BE    LFTEX           YES, EXIT
         BXLE  R4,R6,LFTLP     DO WHILE (R4 + R6) <= R7
         SR    R0,R0           SHOW VALUE NOT FOUND
         B     LFTEX1          EXIT
LFTEX    DS    0H
         LR    R0,R4           SET OUTPUT PARAMETER
LFTEX1   DS    0H
         LM    R1,R7,LFTRR     POP REGS
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
         LA    R2,TREELEV          INPUT LEVEL FIELD ADDR
         LA    R3,LEVELCD          OUTPUT DOBBLE CHARACTERS
         LA    R4,10               INPUT SIZE
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
*                                                     *
*    CONSTANTS AND VARIABLES                          *
*                                                     *
*******************************************************
         LTORG
TMARK7   DC    C'8A'
TRCSAVA7 DS    16F        TRACE ALL REGS SAVE AREA
SAVEPROC DS    18F             PROCESS SAVE AREA
DOBLE    DS    D           CVD BUFF AREA
MASK     DC    XL16'40202020202020202020202020202020'
******************************************************
SVDIS    DS    F               DISPMEN RET ADDR
DISRR    DS    8F              DISPMEN R1-R8 SAVE AREA
SVUPD    DS    F               UPDTMEN RET ADDR
UPDRR    DS    8F              UPDTMEN R1-R8 SAVE AREA
SVLFT    DS    F               LFTSCN RET ADDR
LFTRR    DS    8F              LFTSCN R1-R8 SAVE AREA
SVSID    DS    F               SIDEWAY RET ADDR
SIDRR    DS    8F              SIDEWAY R1-R8 SAVE AREA
SVEXP    DS    F               EXPAND PUSH RET ADDR
EXPRR    DS    6F              EXPAND R1-R6 PUSH REGS
SVMVP    DS    F               MOVECURT RET ADDR
SVFUN    DS    F               FUNCVAL PUSH RET ADDR
FUNRR    DS    2F              FUNCVAL PUSH REGS
SVRSP    DS    F               RESTPANL PUSH RET ADDR
RSPRR    DS    8F              RESTPANL PUSH REGS
RETCODE  DC    F'0'            EXIT RETURN CODE
******************************************************
NORINTEN EQU   *
         DC    X'0000E4021D6000' NORMAL INTENSITY ATTRIBUTE
******************************************************
PARLEXT  DC    V(CALLSCRN)     PASS EXTERNAL LOCATIONS
         DC    V(CALLSCRL)     AS PARAMETER LIST
******************************************************
SWDSCN   DC    V(SWDISCO) COMMON AREA DISCONNECT SWITCH
******************************************************
PROSCRN  DC    V(SCREENL) COMMON OUTPUT STRING AREA ADDRESS
PROPLST  DC    V(PARMLIST) COMMON OUTPUT STRING AREA ADDRESS
PROPARM  DC    CL1'C'       PARAMETER FOR CLEAR SCREEN AND PRINT
         DC    AL1(0,0,0,0) CLEAR COORDINATES
PROADDR  DC    AL4(0)       PANEL INSTRUCTIONS ADDRESS
******************************************************
PROBOXPL DC    V(BOXPARML) BOX PARAMETER LIST ADDRESS
PROBXPLS DC    AL1(6,4,9,74)  R/C R/C AND PARAMETER LIST
PROBXCHR DC    CL8'+-+||+-+' BOX CHARACTERS
PROBOXI  EQU   *            EXT REF BUG
         DC    CL3'BOX'     BOX INDICATOR FOR CALL PROCESS
PROLINI  EQU   *            EXT REF BUG
         DC    CL3'LIN'     LINE INDICATOR FOR CALL PROCESS
PROPANI  EQU   *            EXT REF BUG
         DC    CL3' '       PANEL INDICATOR FOR CALL PROCESS
******************************************************
*TREETPOI DC    V(TREETAB)  TREE TABLE ADDRESS POINTER
*LEVSTADD DC    V(MENUTAB)  MENU TABLE ADDRESS POINTER
******************************************************
FIRSTRE  DC    AL1(01,0,0,0,0,0,0,0,0,0) 1ST TREE LEVEL FOR A SYS
         ENTRY SUBSNAM
SUBSNAM  EQU   *
         DC    CL8' '       SUB SYSTEM NAME PARM FOR TREE BUILD
         ENTRY FUNCNAM      NOT IMPLEMENTED YET
FUNCNAM  EQU   *
         DC    CL8' '       OPTION PROCESS FUNCTION NAME
SWMAINMN DC    X'00'        MAIN MENU TREE GENERATION SWITCH
REGISTR1 DS    F            SAVE CALLING PROGRAM PARAMENTER LIST
MOVUPDN  DC    X'00'        SWITCH FOR MOVING UP=1, DOWN=0
LASTMENU DS    F            LAST MENU PRINTED ADDR
PUSHAREA DS    16F          TEMPORARELY PUSHING AREA
*
         ENTRY TREELEV
TREELEV  EQU   *
         DC    AL1(01,0,0,0,0,0,0,0,0,0) ACTUAL TREE LEVEL LOC
TREELEVL EQU   *            TREE LEVEL LIMIT
         DC    X'00'        TREE VALUE LIMIT
*
* HELP PROCEDURE PARAMETER LIST (FOR TEST)
*HELPARM DC    V(INSCRN)       COMMON WORK SCREEN ADDR
*        DC    V(INSCRNL)      COMMON WORK SCREEN LIMIT ADDR
*        DC    C' '            CLEAR SCREEN INDICATOR
*        DC    C'01'           INCERT CURSOR ROW
*        DC    C'01'           INCERT CURSOR COLUMN
*        DC    C'06'           BOX TOP ROW
*        DC    C'03'           BOX TOP LEFT COLUMN
*        DC    C'21'           BOX BOTTOM ROW
*        DC    C'77'           BOX BOTTOM RITE COLUMN
*
*******************************************************************
         DC    C'HERE ARE APPL PARMS WINDOWS'
* APPLICATION PARAMETER LIST
PROCEDPL EQU   *            COMMON PROCEDURE PARAMETER LIST
         ENTRY SYSTEMCD
SYSTEMCD EQU   *
         DC    CL8' '       SAVE SYSTEM NAME
         ENTRY LEVELCD
LEVELCD  EQU   *
         DC    CL20'00000000000000000000' CHARACTER LEVEL CODE
         ENTRY SUBPROCD
SUBPROCD EQU   *
         DC    CL8' '       SUB-PROCEDURE CODE (NOT USED YET)
         ENTRY PROCTITL
PROCTITL EQU   *
         DC    CL62' '      PROCESS TITLE
         ENTRY INPUTKEY     NOT IMPLEMENTED YET
INPUTKEY EQU   *
         DC    C' '         INPUT KEY TO PASS TO APPLICATION
         ENTRY PROSWIT      NOT IMPLEMENTED YET
PROSWIT  EQU   *
         DC    CL3' '       'END'=DON'T GO BACK '   '=GO BACK
         ENTRY APPLCURS
APPLCURS EQU   *
         DC    CL4'0000'    APPLICATION CURSOR LOCATION (COBOL)
         DC    XL2'0000'    APPLICATION CURSOR LOCATION (ASM)
*******************************************************************
PROCEDUR DC    F'0'         PROCEDURE ADDRESS
LASTKEY  DC    C' '         LAST KEY PRESSED FOR TEST
*
         DS    0D
         ENTRY PANLENG
PANLENG  EQU   *
         DC    F'0'         PANEL WORK LENGTH
*******************************************************************
*TREETAB  DC    AL1(01,1,0,0,0,0,0,0,0,0) ACTUAL TREE LEVEL LOC
*MENUTAB  EQU   *               MENU TABLE
*   ---LEVEL--- -IN-CUR-TST- --MENU--- -LEN-- MN/PR- --BOX COORDIN--
* DC X'01000000000000000000'
* DC AL4(M000000),AL4(MAIN),AL1(4),C'H',AL1(00,00,00,00)
* DC AL4(MAINC),AL4(0) (OUTPUT CUR TABLE + 4 FOR PROCEDURE NAME)
* DC X'01010000000000000000'
* DC AL4(M010000),AL4(PRO00),AL1(8),C'V',AL1(3,4,12,32)
* DC AL4(PRO00C),AL4(0)
* DC X'01010200000000000000'
* DC AL4(M010200),AL4(PRO02),AL1(3),C'V',AL1(6,6,10,34)
* DC AL4(PRO02C),AL4(0)
*
*M000000  EQU   *
*         DC    X'C15E'       CURSOR LOCATION TO TEST
*         DC    X'C1E8'
*         DC    X'C1F0'
*         DC    X'C1F9'
*
*MAIN     EQU   *
* DC X'02066009',C'PROCESS',X'1D60'
* DC X'02106009',C'REPORTS',X'1D60'
* DC X'021A6007',C'FILES',X'1D60'
* DC X'02226008',C'SYSTEM',X'1D6000'
*
*MAINC    EQU   *
* DC X'020E4003114040'     CURSOR TABLE (ARG LEN 7)
* DC X'02184003114040'     X'114040' AS A DUMMY PAD TO MAKE 7
* DC X'02204003114040'     (& A LEAST OF 3 SPACES BETWIN OPTIONS)
* DC X'02294003114040'
* DC X'020E40011300'
*******************************************************************
*
ERRMSG   EQU   *
 DC X'171EE814',C'PROCESS NOT ACTIVE',X'1D6000'
ERRMSG1  EQU   *
 DC X'171EE814',C'  PROCESS FAILED  ',X'1D6000'
         DS    0F   ALIGNE
******** MENU OPTION | PROGRAM ADDRESS PROCESSES TABLE
*ROCETAB DC    CL8'PANEL',V(PANEL)
PROCETAB DC    CL8'BINOD',V(BINOD)
         DC    CL8' '
*
INPSTR   DSECT
KEYCODE  DS    X               INPUT KEY CODE
CURLOCT  DS    XL2             INPUT CURSOR LOCATION
ORDERCD  DS    X               INPUT ORDER CODE
FLDLOCT  DS    XL2             INPUT FIELD LOCATION
INPDATA  DS    C               INPUT DATA STRING
*
         COPY  \R42\CPY\EQUREGS.CPY
         END   WINDOWS
