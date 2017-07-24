         TITLE 'Rename program name to alias and assign new name'
**
* FUNCTION:
* SETPDSEA renames the specified program and uses the original
* program name as an alias.
*
* INVOCATION:
* // EXEC SETPDSEA,PARM='aaaaaaaa pppppppp[R]'
* aaaaaaaa = existing program name (= subsequent alias)
* pppppppp = new program name (minimum 1 character)
* R = (optional) switch = replace any existing alias
*
* RETURN CODE:
* 0 = OK
* 8 = processing error
* 12 = parameter (length) error
* 16 = OPEN error
* 256+n = STOW error (n = STOW return code)
* eg 260 = new name exists already
* 264 = old name does not exist
*
* DD STATEMENTS:
* SYSLMOD - program library that contains the program for
* which the alias is to be set.
* SYSPRINT - report (log) file
*
* SAMPLE JCL
* // EXEC PGM=SETPDSEA,
* // PARM='C00     C0A'
* //*      |       |
* //*      |       +— new name (1 to 8-characters)
* //*      +—————— old name (8-characters, used as ALIAS)
* //STEPLIB DD DSN=LoadLibrary,DISP=SHR
* //SYSLMOD DD DSN=ProgramLibrary
* //SYSPRINT DD SYSOUT=*
**
         PRINT NOGEN
SETPDSEA CSECT
SETPDSEA AMODE 31
SETPDSEA RMODE 24
         SPACE
         BAKR  R14,0               save registers + return address
         BASR  R12,0               set base register
         USING *,R12
         SPACE
         L     R2,0(R1)            pointer to parameter
         LH    R1,0(R2)            length of parameter data
         LA    R0,2(R2)            address of parameter data
* parameter: mmmmmmmm aaaaaaaa[R]
         MVC   RC,=H'12'           preset RC: invalid parameter length
         CH    R1,=AL2(PMINLEN)    test against minimum length
         JL    EXIT                too short
         CH    R1,=AL2(PMAXLEN)    test against maximum length
         JH    EXIT                too long
* else parameter length OK
         LA    R2,PARM
         LA    R3,PMAXLEN
         ICM   R1,B'1000',=X'40'   padding byte
         MVCL  R2,R0               move parameter to data area
* build variable-name entries
         LA    R1,VONAME           old name (becomes new alias)
         USING VDSECT,R1
         MVC   VLEN,=H'8'
         MVC   VNAME,PONAME
         LA    R1,VNNAME           new name
         USING VDSECT,R1
         MVC   VLEN,=H'8'
         MVC   VNAME,PNNAME
         SPACE
         MVC   RC,=H'16'           RC: open error
         MVC   RNAME,PNNAME
         OPEN  (SYSLMOD,(UPDAT))
         LTR   R15,R15
         JNZ   EXIT
         MVC   RC,=X'0100'         RC: STOW error
         STOW  SYSLMOD,PONAME,C
         STC   R15,RCRSC           append STOW RC
         CLOSE (SYSLMOD)
         CLI   RCRSC,4             test whether error occurred
         JH    EXIT                STOW error
         JL    STOWOK              STOW OK
* replace warning
         CLI   PFLAG,C'R'
         JNE   EXIT                replace not allowed
         SPACE
STOWOK   MVC   RC,=H'8'            RC: processing error
* start dialog
         IEWBIND FUNC=STARTD,                                          X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               DIALOG=DTOKEN,                                          X
               FILES=FILELIST
         BAL   R9,TESTRC
         J     EXIT1 error return
* create workmod (without binding)
         IEWBIND FUNC=CREATEW,                                         X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               DIALOG=DTOKEN,                                          X
               WORKMOD=WKTOKEN,                                        X
               INTENT=ACCESS
         BAL   R9,TESTRC
         J     EXIT2 error return
* include module
         IEWBIND FUNC=INCLUDE,                                         X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               WORKMOD=WKTOKEN,                                        X
               INTYPE=NAME,                                            X
               ATTRIB=YES,                                             X
               DDNAME=LOADLIB,                                         X
               MEMBER=VNNAME
         BAL   R9,TESTRC
         J     EXIT2 error return
* add alias
         IEWBIND FUNC=ADDA,                                            X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               WORKMOD=WKTOKEN,                                        X
               ANAME=VONAME
         BAL   R9,TESTRC
         J     EXIT2 error return
* save workmod
         IEWBIND FUNC=SAVEW,                                           X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               WORKMOD=WKTOKEN,                                        X
               MODLIB=LOADLIB,                                         X
               SNAME=VNNAME,                                           X
               REPLACE=YES
         BAL   R9,TESTRC
         J     EXIT2 error return
         SPACE
         MVC   RC,=H'0'            reset return code
* delete workmod
EXIT2    IEWBIND FUNC=DELETEW,                                         X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               WORKMOD=WKTOKEN,                                        X
               PROTECT=YES
* end dialog
EXIT1    IEWBIND FUNC=ENDD,                                            X
               VERSION=4,                                              X
               RETCODE=RTC,RSNCODE=RSC,                                X
               DIALOG=DTOKEN
         SPACE
EXIT     LH R15,RC                 set program return code
         PR ,                      program return
         SPACE 2
TESTRC   DS 0H                     test return code
* Display low-order 16-bits of RSC (reason code) as hexadecimal
* if non-zero
* Return: 0(R9) error
* 4(R9) OK (no output message issued)
         LTR   R15,R15             return code
         BZ    4(R9)               OK, return
* else display reason code (hexadecimal)
N        EQU   4                   field length
         UNPK  WK,RSC(N+1)         FaFbFcFdFeFgFhxx,abcdefghxx
         TR    WK,TRTAB-240
         WTO   TEXT=MSG,ROUTCDE=(11) Write To Programmer
         BR    R9                  error return
TRTAB    DC    C'0123456789ABCDEF'
         TITLE ‘Constants and work areas'
MSG      DC    H'12'
         DC    C'RSC:'
WK       DS    CL(N*2+1)
         SPACE 1
PARM     DS    0C                  EXEC parameter
PONAME   DS    CL8                 old name
RNAME    DS    0CL8
         DS    C                   separator
PNNAME   DS    CL8                 new name (moved to RNAME)
PFLAG    DS    CL1                 switch (optional, R = replace)
PMAXLEN  EQU   *-PARM              parameter maximum length
         ORG   PNNAME
         SPACE
         DS    C                   minimum alias
PMINLEN  EQU   *-PARM              parameter minimum length
         ORG   ,                   reset origin counter
         SPACE
RC       DS    H                   return code
RCRSC    EQU   *-1                 STOW return code
RTC      DS    F                   IEWBIND return code
RSC      DS    XL4                 IEWBIND reason code
         SPACE
DTOKEN   DS    CL8                 dialog token
WKTOKEN  DS    CL8                 workarea token
         SPACE
FILELIST DC    F'1'                number of list entries
         DC    CL8'PRINT',F'8',A(PRINTX)
PRINTX   DC    CL8'SYSPRINT'
         SPACE
LOADLIB  DC    H'7',CL7'SYSLMOD'
         SPACE
VONAME   DS    H,CL8
VNNAME   DS    H,CL8
         SPACE
SYSLMOD  DCB   DDNAME=SYSLMOD,DSORG=PO,MACRF=(R,W)
         SPACE
VDSECT   DSECT
VLEN     DS    H
VNAME    DS    CL8
         SPACE
* Register equates
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
         END
