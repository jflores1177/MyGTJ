**********************************************************************
*                                                                    *
*                       SAMPLE BINDER PROGRAM                        *
*                                                                    *
*  Example application which includes a module and prints its ESD    *
*  records using the Binder call interface functions INCLUDE, GETN   *
*  and GETE.                                                         *
*                                                                    *
**********************************************************************
**********************************************************************
* PROGRAM INITIALIZATION                                             *
**********************************************************************
**********************************************************************
*               1. Entry point linkage                               *
*****                                                            *****
BAGETE   CSECT
BAGETE   AMODE 31
BAGETE   RMODE 24
         PRINT GEN
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SAVE  (14,12)
         BASR  R12,0              Get 31-bit base even in 24-bit mode
         USING *,R12
         ST    R12,MESSAGE+4      Save program base for message exit
         LA    R15,SAVE
         ST    R13,SAVE+4
         ST    R15,8(,13)
         LR    R13,R15
         SPACE
         MVC   FREEBFR,ZERO       No buffers to FREEBUF yet
         MVC   CLSDCB,ZERO        No DCB to close yet
         MVC   ENDDLG,ZERO        No Dialog to end yet
         MVI   MSG,C'0'
         MVC   MSG+1(80-1),MSG
         L     R2,=A(ESTAEX)           ESTAEX EXIT
         LAE   R3,ESXPARMS             PARAMETER FOR ESTAEX EXIT
***********************************************************************
*        ESTABLISH ESTAEX WITH A RECOVERY ROUTINE                     *
***********************************************************************
         ESTAEX (R2),CT,PARAM=(3)      ESTABLISH ESTAEX IN CASE ABEND
**********************************************************************
*               2. Open output data set                              *
*****                                                            *****
         OPEN  (MYDCB,OUTPUT)     Open output data set
         LTR   R15,R15            Successful?
         BNZ   ERREXIT            Exit if not
         MVC   CLSDCB,FOUR        We must CLOSE our DCB on exit
         SPACE
**********************************************************************
*               3. Obtain and initialize binder buffers              *
***** *****
       IEWBUFF FUNC=GETBUF,TYPE=ESD
       IEWBUFF FUNC=GETBUF,TYPE=NAME
       IEWBUFF FUNC=INITBUF,TYPE=ESD
       IEWBUFF FUNC=INITBUF,TYPE=NAME
         MVC   FREEBFR,FOUR       We must FREEBUF our buffers on exit
         SPACE
**********************************************************************
*               4. Start Dialog, specifying lists                    *
*****                                                            *****
         MVC   DTOKEN,DZERO Clear dialog token
               IEWBIND FUNC=STARTD,                                    +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               DIALOG=DTOKEN,                                          +
               FILES=FILELIST,                                         +
               EXITS=EXITLIST,                                         +
               OPTIONS=OPTLIST,                                        +
               VERSION=4
         CLC   RSNCODE,ZERO       Check the reason code
         BNE   ERREXIT            Exit if not zero
         MVC   ENDDLG,FOUR        We must ENDDIALOG on exit
         EJECT
**********************************************************************
*               5. Create a Workmod with Intent ACCESS               *
*****                                                            *****
         MVC   WKTOKEN,DZERO      Clear workmod token
       IEWBIND FUNC=CREATEW,                                           +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               DIALOG=DTOKEN,                                          +
               INTENT=ACCESS,                                          +
               VERSION=4
         CLC   RSNCODE,ZERO       Check the reason code
         BNE   ERREXIT            Exit if not zero
         EJECT
**********************************************************************
*               6. Set the list option to ALL                        *
*****                                                            *****
       IEWBIND FUNC=SETO,                                              +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               OPTION=LIST,                                            +
               OPTVAL=ALL,                                             +
               VERSION=4
         CLC   RSNCODE,ZERO       Check the reason code
         BNE   ERREXIT            Exit if not zero
         EJECT
**********************************************************************
*                       MAIN PROGRAM                                 *
**********************************************************************
**********************************************************************
*               7. Include a module (IFG0198N)                       *
*****                                                            *****
       IEWBIND FUNC=INCLUDE,                                           +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               INTYPE=NAME,                                            +
               DDNAME=INCLLIB,                                         +
               MEMBER=MODNAME,                                         +
               VERSION=4
         CLC   RSNCODE,ZERO       Check the reason code
         BNE   ERREXIT            Exit if not zero
         EJECT
**********************************************************************
*               8. Get all section names from workmod                *
*****                                                            *****
         MVC   CURSORN,ZERO
       IEWBIND FUNC=GETN,                                              +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               AREA=IEWBBNL,                                           +
               CURSOR=CURSORN,                                         +
               COUNT=COUNTN,                                           +
               TCOUNT=TCOUNT,                                          +
               NTYPE=S,                                                +
               VERSION=4,                                              +
               TCOUNT=TCOUNT,                                          +
               NTYPE=S,                                                +
               VERSION=4
         CH    R15,=H'4'           RC=4 means have all names
         BE    GETNOK
         BH    ERREXIT             Any higher is an error
         PUT   MYDCB,MSG2MANY      RC=0: Too many sections
GETNOK   EQU   *
         EJECT
**********************************************************************
*               9. Get ESD data for each name returned by GETN       *
*****                                                            *****
         L     R5,COUNTN          Number of sections
LOOP1    L     R3,BNL_NAME_PTR    Extract section name
         LH    R2,BNL_NAME_CHARS
         STH   R2,SECTION
         LA    R4,SECTION
         BCTR  R2,0
         EX    R2,MOVESEC
         MVC   CURSORD,ZERO       Reset cursor
       IEWBIND FUNC=GETD,                                              +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               CLASS=B_ESD,                                            +
               SECTION=SECTION,                                        +
               AREA=IEWBESD,                                           +
               CURSOR=CURSORD,                                         +
               COUNT=COUNTD,                                           +
               VERSION=4
         CLC   RSNCODE,ZERO
         BE    GETDOKAY
         CLC   RETCODE,FOUR       Last buffer
         BE    GETDOKAY
         CLC   RETCODE,EIGHT      No data for item
         BNE   ERREXIT
GETDOKAY EQU   *
         L     R4,COUNTD          Number of ESD entries in buffer
         LTR   R4,R4              Skip empty section
         BZ    NEXTSECT
         LA    R7,ESDH_END        First record in ESD buffer
         SH    R7,=H'4'           Leave space for length info
         L     R0,ESDH_ENTRY_LENG
         AH    R0,=H'4'
         SLL   R0,16              Convert to LLBB form
LOOP2    DS    0H
         ST    R0,0(,R7)
         PUT   MYDCB,(R7)         Write ESD to output data set
         L     R0,0(,R7)
         A     R7,ESDH_ENTRY_LENG Move to next ESD in this section
         BCT   R4,LOOP2
NEXTSECT A     R9,BNLH_ENTRY_LENG Move to next section name
         BCT   R5,LOOP1
         SPACE
**********************************************************************
*                       END OF DATA - FINISH UP                      *
**********************************************************************
**********************************************************************
*              10. Done processing - delete workmod                  *
*****                                                            *****
       IEWBIND FUNC=DELETEW,                                           +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               WORKMOD=WKTOKEN,                                        +
               PROTECT=YES,                                            +
               VERSION=4
         CLC   RSNCODE,ZERO
         BNE   ERREXIT
         SPACE
**********************************************************************
*              11. End dialog                                        *
*****                                                            *****
       IEWBIND FUNC=ENDD,                                              +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               DIALOG=DTOKEN,                                          +
               VERSION=4
         CLC   RSNCODE,ZERO
         BNE   ERREXIT
         SPACE
**********************************************************************
*              12. FREEBUF (Release) our buffer storage              *
*****                                                            *****
FREEBUFS IEWBUFF FUNC=FREEBUF,TYPE=ESD
         IEWBUFF FUNC=FREEBUF,TYPE=NAME
**********************************************************************
*              13. Close output dataset                              *
*****                                                            *****
CLOSEDCB CLOSE (MYDCB)
      FREEPOOL MYDCB
         SPACE
**********************************************************************
*              14. Return to operating system                        *
*****                                                            *****
NORMEXIT EQU   *
         LA    R15,0              Set a reason code of zero
         B     EXIT
ERREXIT  EQU   *
         CLC   FREEBFR,FOUR       Do we need to FREEBUF our buffers?
         BNE   CHECKDLG
       IEWBUFF FUNC=FREEBUF,TYPE=ESD
       IEWBUFF FUNC=FREEBUF,TYPE=NAME
CHECKDLG CLC   ENDDLG,FOUR        Do we need to end the Dialog?
         BNE   CHECKDCB
*        Ending the dialog also deletes the workmod
       IEWBIND FUNC=ENDD,                                              +
               RETCODE=RETCODE,                                        +
               RSNCODE=RSNCODE,                                        +
               DIALOG=DTOKEN,                                          +
               PROTECT=NO,                                             +
               VERSION=4
CHECKDCB CLC   CLSDCB,FOUR        Do we need to CLOSE and FREE our DCB?
         BNE   SETRSN
         CLOSE (MYDCB)
      FREEPOOL MYDCB
SETRSN   L     R15,RSNCODE
EXIT     L     R13,SAVE+4
        RETURN (14,12),RC=(15)
**********************************************************************
*                       PROGRAM CONSTANTS                            *
**********************************************************************
DZERO    DC    2F'0'
ZERO     DC    F'0'
FOUR     DC    F'4'
EIGHT    DC    F'8'
MOVESEC  MVC   2(0,R4),0(R3)
MSG2MANY DC    Y(MSG2MZ-*,0),C'TOO MANY SECTIONS TO DISPLAY'
MSG2MZ   EQU   *
**********************************************************************
*              15. Variable length string constants                  *
*****                                                            *****
B_ESD    DC    H'5',C'B_ESD'      Class name
ALL      DC    H'3',C'ALL'        LIST option value
INCLLIB  DC    H'6',C'LPALIB'     Include library
LIST     DC    H'4',C'LIST'       LIST option keyword
MODNAME  DC    H'8',C'IFG0198N'   Member name
TERM     DC    H'4',C'TERM'       TERM option keyword
Y        DC    H'1',C'Y'          TERM option value
**********************************************************************
*              16. STARTD list specifications                        *
*****                                                            *****
FILELIST DS    0F                 ddname specifications
         DC    F'1'               Number of list entries
         DC    CL8'PRINT',F'8',A(PRINTX) Assign print file ddname
PRINTX   DC    CL8'SYSPRINT'      The ddname
         SPACE
OPTLIST  DS    0F                 Global options specifications
         DC    F'1'               Number of list entries
         DC    CL8'TERM',F'1',A(YX) Set TERM option
YX       DC    C'Y'               TERM option value
EXITLIST DS    0F                 User exit specifications
         DC    F'1'               Number of list entries
         DC    CL8'MESSAGE',F'12',A(MESSAGE) Specify MESSAGE exit
MESSAGE  DC    A(MSGEXIT)         Exit routine entry point
         DC    AL4(0)             Base address for exit routine
         DC    A(FOUR)            Take exit for severity >= 4
**********************************************************************
*                       WORKING STORAGE                              *
**********************************************************************
SAVE     DS    18F                Register save area
SAVE2    DS    18F                Another for the exit routine
SAVE13   DS    F                  Register 13 save
COUNTD   DS    F                  Number of ESD records returned
COUNTN   DS    F                  Number of section names
CURSORD  DS    F                  Cursor value for GETD call
CURSORN  DS    F                  Cursor value for GETN call
DCB@     DS    F                  DCB for output file
DTOKEN   DS    CL8                Dialog Token
RETCODE  DS    F                  General return code
RSNCODE  DS    CL4                General reason code
SECTION  DS    H,CL8              Section Name for GETD
TCOUNT   DS    F                  Total number of sections
WKTOKEN  DS    CL8                Workmod Token
MSGLEN   DS    F
MSG      DC    80C'0'             Put message buffer
FREEBFR  DS    F                  Indicator for FREEBUFing our buffers
*                                 on exit, if they were GETBUFfed.
CLSDCB   DS    F                  Indicator for closing our DCB
ENDDLG   DS    F                  Indicator for ENDDing the Dialog
**********************************************************************
*              17. DCB for output file                               *
*****                                                            *****
MYDCB    DCB   DSORG=PS,MACRF=PM,RECFM=VB,LRECL=300,DDNAME=MYDDN
*****                                                            *****
         DS    0D
ESXPARMS DC    A(DSPSTOR,SNAPXP,0,0)       FOUR PARAMETERS FOR ESTAEX
*                                          ... LAST TWO ARE ALETS
*
SNAPXP   SNAPX DSPSTOR=DSPSTOR,MF=L        NEEDED FOR DUMPOPX
*
DSPSTOR  DS    0D                          DATA SPACE STORAGE LIST
START@   DC    A(0)                        BEGIN ADDRESS
END@     DC    X'FFFFFFFF'                 END ADDRESS; HIGH BIT ON
**********************************************************************
*              18. NAMES and ESD Buffer Mappings.                    *
*        Note that the buffer VERSION must match the                 *
*        VERSION of the GETN and the GETD Binder APIs.               *
*****                                                            *****
       IEWBUFF FUNC=MAPBUF,TYPE=ESD,SIZE=50,                           +
               HEADREG=6,ENTRYREG=7,VERSION=4
       IEWBUFF FUNC=MAPBUF,TYPE=NAME,SIZE=50,                          +
               HEADREG=8,ENTRYREG=9,VERSION=4
         LTORG
**********************************************************************
*                       MESSAGE EXIT ROUTINE                         *
*                                                                    *
*        This exit routine merely prints out a message as an example *
*        of how the print exit could be used, not how it should      *
*        be used.                                                    *
**********************************************************************
**********************************************************************
*              19. Message Exit Routine                              *
*                                                                    *
*        Note: This routine will always be entered in AMODE(31).     *
*        If AMODE(24) is required, capping code must be added.       *
*****                                                            *****
MSGEXIT  EQU   *
         SAVE  (14,12)
         L     R12,0(,R1)         Get address of user data
         L     R12,0(,R12)        Get user data(pgm base register)
         L     R4,28(,R1)         Get address of exit return code
         XC    0(4,R4),0(R4)      Set exit return code to zero
         L     R3,4(,R1)          Get address of address of msg buf
         L     R3,0(,R3)          Get address of message buffer
         LH    R1,0(,R3)          Length of the message
         LA    R0,L'MSG
         CR    R1,R0
         BNL   MSGX2
         LR    R1,R0              But limited to buffer length
MSGX2    DS    0H
         LA    R0,4(,R1)          Length+4 for QSAM
         SLL   R0,16              Convert to LLBB form
         ST    R0,MSGLEN
         BCTR  R1,0               Length-1 for Execute
         EX    R1,MOVEMSG         Put all we can in the buffer
         LA    R3,MSGLEN
         ST    R13,SAVE13         Save input save area address
         LA    R13,SAVE2          Save area for PUT
         PUT   MYDCB,(R3)         Write message to data set
         L     R13,SAVE13         Restore save area register
        RETURN (14,12)            Return to binder
*
MOVEMSG  MVC   MSG(0),2(R3) Executed above
ESTAEX   CSECT
ESTAEX   AMODE ANY
ESTAEX   RMODE ANY
         SYSSTATE ASCENV=AR        SET GLOBAL BIT FOR AR MODE
*
         USING ESTAEX,R15          TEMPORARY ADDRESSABILITY
         B     ESTAEX0             BRANCH AROUND ID
         DC    AL1(6)              LENGTH OF PROGRAM NAME
         DC    C'ESTAEX'           PROGRAM NAME
ESTAEX0  DS    0H
         CH    R0,=H'12'           IS THERE AN SDWA?
         BE    ESNOSDWA            NO: JUST EXIT
         DROP  R15                 END TEMPORARY ADDRESSABILITY
         STM   R14,R12,12(R13)     SAVE OS/390 REGISTERS
         LAE   R12,0(R15,R0)       R12 = BASE REGISTER
         USING ESTAEX,R12          ESTABLISH ADDRESSABILITY
         LAE   R3,0(,R1)           R3 --> SDWA
         USING SDWA,R3             OVERLAY SDWA
         L     R4,SDWAPARM         R4 --> EXIT PARAMETER AREA
         L     R5,0(,R4)           R5 --> USER DATA
         LAM   R5,R5,4(R4)         AR5 =  ALET FOR USER DATA
         LM    R10,R11,0(R5)       R10 --> DSPSTOR VALUE
*                                  R11 --> SNAPX MF=L AREA
         LAM   R10,R11,8(R5)       GET ALETS FOR DSPSTOR, SNAPX MF=L
*                                  ... (IN PRIMARY ADDRESS SPACE)
*                                  STOKEN LIST LAYOUT:
*                                  +0 = ADDRESS OF DATA SPACE STORAGE
*                                  +4 = LENGTH OF DATA SPACE STORAGE
*                                  +8 = 8 BYTE STOKEN FOR DATA SPACE
         CLC   =XL8'00',8(R10)     IS THERE A DATA SPACE?
         BE    ESNODS              NO DATA SPACE
***********************************************************************
*        RESUME ABEND; DISPLAY DATA SPACE STORAGE                     *
***********************************************************************
         WTO   'GTDEVJF - Resuming ABEND for DATASPACE'
         SETRP REGS=(14,12),RC=0,DUMP=YES,DUMPOPX=(R11)
ESNODS   DS    0H
         WTO   'GTDEVJF - Resuming ABEND; No DATASPACE'
***********************************************************************
*        RESUME ABEND; NO DATA SPACE STORAGE                          *
***********************************************************************
         SETRP REGS=(14,12),RC=0,DUMP=YES
ESNOSDWA DS    0H
         SR    R15,R15             CONTINUE ABNORMAL TERMINATION
         BR    R14                 RETURN TO OS/390
*
         LTORG
         DROP
         IHASDWA
         END
