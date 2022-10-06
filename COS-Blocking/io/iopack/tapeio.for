      INTEGER*4 FUNCTION T_LOGICAL(DEVNAME,LDRV)
C     ++++++++++
C     RAW TAPE I/O
C     CALL T_LOGICAL(DEVICENAME,LDRV) to associate a
C     particular DEVICENAME with a given logical drive number, LDRV.
C     Normally default associations will be set up for you
C     on a system-wide basis.
C     ----------
      IMPLICIT INTEGER*4 (S)
      INCLUDE 'TAPEIO.INC'
      INTEGER*4 ITMLST(7),SYS$GETDVI
      CHARACTER*(*) DEVNAME
      CHARACTER*64 NAME1,NAME2,TNAME
      EXTERNAL DVI$_DEVCLASS,DC$_TAPE,SS$_NOTRAN,DVI$_DEVNAM
      EXTERNAL SS$_IVDEVNAM
      LOGICAL ERROR
      ERROR(I) = MOD(I,8).NE.1
C     ASSUME FAILURE
D     TYPE *,'CALLED T_LOGICAL WITH ',DEVNAME
      T_LOGICAL = -1
      LDRV = -1
      ITMLST(1) = ISHFT(%LOC(DVI$_DEVCLASS),16).OR.4
      ITMLST(2) = %LOC(ICLASS)
      ITMLST(3) = 0
      ITMLST(4) = ISHFT(%LOC(DVI$_DEVNAM),16).OR.LEN(NAME1)
      ITMLST(5) = %LOC(NAME1)
      ITMLST(6) = %LOC(LN1)
      ITMLST(7) = 0
      CALL IFERR(LIB$GET_EF(IFLG))
      LD = NBLANK(DEVNAME)
      I = SYS$GETDVI(%VAL(IFLG),,DEVNAME(1:LD),ITMLST,,,,)
      IF(.NOT.ERROR(I))CALL IFERR(SYS$WAITFR(%VAL(IFLG)))
      CALL IFERR(LIB$FREE_EF(IFLG))
D      CALL ERRMES(I)
      IF(ERROR(I))RETURN
      IF(ICLASS.NE.%LOC(DC$_TAPE))RETURN
C      CALL IFERR(LOGTRANS(DEVNAME(1:LD),NAME1,LN1))
D      TYPE *,DEVNAME(1:LD),' TRANSLATED TO ',NAME1(1:LN1)
      TNAME = LOG_NAM
      LT = NBLANK(LOG_NAM)
      T_LOGICAL = 1
      DO I = 0 , N_MAX
         WRITE(TNAME(LT+1:LT+L_DIGS),100)I
100      FORMAT(I<L_DIGS>.<L_DIGS>)
         LDRV = I
         ITMLST(4) = ISHFT(%LOC(DVI$_DEVNAM),16).OR.LEN(NAME2)
         ITMLST(5) = %LOC(NAME2)
         ITMLST(6) = %LOC(LN2)
         CALL IFERR(LIB$GET_EF(IFLG))
         J = SYS$GETDVI(%VAL(IFLG),,TNAME(1:LT+L_DIGS),ITMLST,,,,)
         IF(.NOT.ERROR(J))CALL SYS$WAITFR(%VAL(IFLG))
         CALL IFERR(LIB$FREE_EF(IFLG))
C         J = LOGTRANS(TNAME(1:LT+L_DIGS),NAME2,LN2)
D        TYPE *,TNAME(1:LT+L_DIGS),' TRANSLATED TO ',
D    &      NAME2(1:LN2)
C         CALL IFERR(J)
         IF(NAME1(1:LN1).EQ.NAME2(1:LN2))RETURN
C         IF(J.EQ.%LOC(SS$_NOTRAN))THEN
         IF(J.EQ.%LOC(SS$_IVDEVNAM))THEN
            TYPE *,'Couldn''t find a LOGICAL_MAGTAPE_nn to match '//
     &         NAME1(1:LN1)
            IF(I.EQ.0)THEN
               TYPE *,'In order to use this software you must'
               TYPE *,'define logical names like LOGICAL_MAGTAPE_00'
               TYPE *,'to point to the tape drives on your system!'
               TYPE *,'For example:'
               TYPE *,'$ ASSIGN MTA0: LOGICAL_MAGTAPE_00'
               TYPE *,'$ ASSIGN MTA1: LOGICAL_MAGTAPE_01'
               TYPE *,'$ ASSIGN MFA0: LOGICAL_MAGTAPE_02'
            END IF
            DO J=0,I-1
               WRITE(TNAME(LT+1:LT+L_DIGS),100)J
               CALL IFERR(LIB$GET_EF(IFLG))
               IF(.NOT.ERROR(SYS$GETDVI(%VAL(IFLG),,TNAME(1:LT+
     &            L_DIGS),ITMLST,,,,)))CALL IFERR(SYS$WAITFR(IFLG))
               CALL IFERR(LIB$FREE_EF(IFLG))
C               CALL LOGTRANS(TNAME(1:LT+L_DIGS),NAME2,LN2)
               TYPE *,TNAME(1:LT+L_DIGS)//' translates to : '//
     &            NAME2(1:LN2)
            END DO
            STOP 'SEE SYSTEM MANAGER ABOUT ERROR IN T_LOGICAL'
         ELSE
            CALL IFERR(J)
         END IF
      END DO
      TYPE *,'YOU HAVE EXCEEDED THE LIMIT OF ',N_MAX,' TAPE DRIVES!'
      STOP 'SEE SYSTEM MANAGER ABOUT ERROR IN T_LOGICAL'
      END
      SUBROUTINE T_ASSG(LDRV)
      IMPLICIT INTEGER*4 (S)
      INCLUDE 'TAPEIO.INC'
      INTEGER*4 ITMLST(4)
      CHARACTER*20 DRIVENAME
      EXTERNAL DVI$_DEVCLASS,DC$_TAPE
      DATA IOSB/0,N_MAX*0,0,N_MAX*0/
      DATA ISTAT/0,N_MAX*0/
      DATA LFIRST/.TRUE.,N_MAX*.TRUE./
      DATA ICHAN/0,N_MAX*0/
      DATA IFLAG/0,N_MAX*0/
      DATA NBY/0,N_MAX*0/
      DRIVENAME = LOG_NAM
      LT = NBLANK(LOG_NAM)
      WRITE(DRIVENAME(LT+1:LT+L_DIGS),100)LDRV
100   FORMAT(I<L_DIGS>.<L_DIGS>)
      CALL IFERR(SYS$ASSIGN(DRIVENAME(1:LT+L_DIGS),ICHAN(LDRV),,),
     .   'T_ASSG COULDN''T ASSIGN A CHANNEL')
D     TYPE *,LDRV,ICHAN(LDRV)
      CALL IFERR(LIB$GET_EF(IFLAG(LDRV)),
     .   'T_ASSG COULDN''T GET AN EF')
      LFIRST(LDRV)=.FALSE.
      ITMLST(1) = ISHFT(%LOC(DVI$_DEVCLASS),16).OR.4
      ITMLST(2) = %LOC(ICLASS)
      ITMLST(3) = 0
      ITMLST(4) = 0
      CALL IFERR(SYS$GETDVI(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),,ITMLST,
     &   ,,,))
      CALL IFERR(SYS$WAITFR(%VAL(IFLAG(LDRV))))
      IF(ICLASS.NE.%LOC(DC$_TAPE))THEN
         TYPE *,'TAPEIO CALLED WITH A NON-TAPE DEVICE SPECIFIED BY ',
     &      DRIVENAME(1:LT+L_DIGS-1)
         CALL EXIT
      END IF
C     set status to success
      ISTAT(LDRV)=0
      RETURN
      END
      SUBROUTINE T_READ(LDRV,BUFFER,LENGTH)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_READ(LDRV,BUFFER,LENGTH)
C        where
C           LDRV is a logical magtape drive number. ie LOGICAL_MAGTAPE_nn
C           where LOGICAL_MAGTAPE_00 is often MTA0:
C           BUFFER is buffer address,
C           LENGTH is number of bytes
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      INCLUDE 'TAPEIO.INC'
      EXTERNAL IO$_READVBLK
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = LENGTH
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_READVBLK,
     1IOSB(1,LDRV),,,
     1BUFFER,%VAL(LENGTH),,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_READ COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_WRIT(LDRV,BUFFER,LENGTH)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_WRIT(LDRV,BUFFER,LENGTH)
C        where
C           LDRV is a logical magtape drive number.
C           Generally LOGICAL_MAGTAPE_00 points to MTA0:
C           BUFFER is buffer address,
C           LENGTH is number of bytes
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL IO$_WRITEVBLK
      INCLUDE 'TAPEIO.INC'
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = 0
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_WRITEVBLK,
     1IOSB(1,LDRV),,,
     1BUFFER,%VAL(LENGTH),,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_WRIT COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_WAIT(LDRV,NSTATE,NBYTES)
C     ++++++++++
C     CALL T_WAIT(LDRV,NSTATE,NBYTES) forces
C     completion of IO on the unit.
C
C           LDRV is logical magtape drive number.
C
C     RETURNS NSTATE = 0      => SUCCESS
C                      1      => EOF
C                      2      => ERROR
C                      3      => EOT
C             NBYTES = NUMBER OF BYTES IN LAST TRANSFER
C
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL SS$_ENDOFFILE,SS$_ENDOFTAPE
      EXTERNAL SS$_DATAOVERUN
      INCLUDE 'TAPEIO.INC'
      COMMON/IO_ERR/WRTMES
      LOGICAL WRTMES
      INTEGER*2 IST(4,0:N_MAX)
      EQUIVALENCE (IOSB,IST)
      DATA WRTMES/.TRUE./
C
      CALL T_CHECK(LDRV)
      CALL IFERR(SYS$WAITFR(%VAL(IFLAG(LDRV))),
     .   'T_WAIT COULDN''T WAIT FOR EF')
      CALL T_CHECK(LDRV)
      ISTAT(LDRV) = 0
      IRES = IST(1,LDRV)
      IRES = IRES.AND.'0000FFFF'X
      IF(MOD(IRES,8).EQ.1)GO TO 10
      ISTAT(LDRV)=2
      IF(IRES.EQ.%LOC(SS$_ENDOFFILE))ISTAT(LDRV)=1
      IF(IRES.EQ.%LOC(SS$_ENDOFTAPE))ISTAT(LDRV)=3
      IF(ISTAT(LDRV).NE.2)GO TO 10
      CALL ERRMES(IRES)
C
10    CONTINUE
      NSTATE=ISTAT(LDRV)
      NBYTES=IST(2,LDRV)
      NBYTES=NBYTES.AND.'0000FFFF'X
C
C     unfortunately, the VMS 1.6 handler never returns
C     SS$_DATAOVERUN
C
      IF((NBY(LDRV).NE.0).AND.(NBYTES.GT.NBY(LDRV)))THEN
         IRES = %LOC(SS$_DATAOVERUN)
      END IF
      IF(IRES.EQ.%LOC(SS$_DATAOVERUN))THEN
         IF(WRTMES)CALL ERRMES(IRES)
         NBYTES = MIN(NBYTES,NBY(LDRV))
      END IF
      RETURN
      END
      SUBROUTINE T_WEOF(LDRV)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_WEOF(LDRV) writes an EOF
C        where
C           LDRV is a logical magtape drive number.
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL IO$_WRITEOF
      INCLUDE 'TAPEIO.INC'
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = 0
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_WRITEOF,
     1IOSB(1,LDRV),,,
     1,,,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_WEOF COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_RWND(LDRV)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_RWND(LDRV) rewinds the tape
C        where
C           LDRV is a logical magtape drive number.
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL IO$_REWIND
      INCLUDE 'TAPEIO.INC'
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = 0
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_REWIND,
     1IOSB(1,LDRV),,,
     1,,,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_RWND COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_SKPF(LDRV,NUMBER)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_SKPF(LDRV,NUMBER)  skips NUMBER files
C        where
C           LDRV is a logical magtape drive number.
C           NUMBER is the number of files to skip
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL IO$_SKIPFILE
      INCLUDE 'TAPEIO.INC'
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = 0
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_SKIPFILE,
     1IOSB(1,LDRV),,,
     1%VAL(NUMBER),,,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_SKPF COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_SKPR(LDRV,NUMBER)
C     ++++++++++
C     RAW TAPE I/O
C     CALL WITH:
C        CALL T_SKPR(LDRV,NUMBER) skips NUMBER records
C        where
C           LDRV is a logical magtape drive number.
C           NUMBER is the number of files to skip
C
C           Also, execution is asynchronous.
C     ----------
      IMPLICIT INTEGER*4 (S)
      EXTERNAL IO$_SKIPRECORD
      INCLUDE 'TAPEIO.INC'
      CALL T_CHECK(LDRV)
      IF(LFIRST(LDRV))CALL T_ASSG(LDRV)
      NBY(LDRV) = 0
      I = SYS$QIO(%VAL(IFLAG(LDRV)),%VAL(ICHAN(LDRV)),IO$_SKIPRECORD,
     1IOSB(1,LDRV),,,
     1%VAL(NUMBER),,,,,)
      IF(MOD(I,8).EQ.1)RETURN
      CALL ERRMES(I)
      CALL IFERR(SYS$SETEF(%VAL(IFLAG(LDRV))),
     .   'T_SKPR COULDN''T SET EF')
      ISTAT(LDRV)=2
      RETURN
      END
      SUBROUTINE T_CHECK(LDRV)
      INCLUDE 'TAPEIO.INC'
      IF((LDRV.GE.0).AND.(LDRV.LE.N_MAX))RETURN
      TYPE *,'ILLEGAL VALUE OF LDRV IN TAPEIO :',LDRV
      STOP
      END
