      SUBROUTINE CFIRST(IOLUN)
      INCLUDE 'IOPACK.INC'
      CHARACTER*1 CHAR
      CHARACTER*1 ESC
      CHARACTER*255 LOGIC,RESULT
      LOGICAL D_OPEN
      INTEGER*4 SYS$TRNLOG,SYS$CRELOG
      BYTE IESC
      EXTERNAL SS$_NOTRAN,SS$_ABORT
      DATA FIRST/N_IOLUNS*.TRUE./
      DATA WRITE/N_IOLUNS*.FALSE./
      DATA WAIT/N_IOLUNS*.FALSE./
      DATA NDRIVE/N_IOLUNS*-1/
C     if first is clear we're all set
      IF(.NOT.FIRST(IOLUN))RETURN
C     get real name
      WRITE(LOGIC,100)'IOP',IOLUN
100   FORMAT(A3,I3.3)
C     attempt a translation
5     I = LOGTRANS(LOGIC(1:6),RESULT,L)
      CALL IFERR(I)
C     if there weren't any translations then use SY:IOP00n.DAT
40    IF(I.EQ.%LOC(SS$_NOTRAN))THEN
         CALL IFERR(SYS$CRELOG(%VAL(2),LOGIC(1:6),LOGIC(1:6)//'.DAT',))
         GOTO 5
      END IF
C     there was a translation and we've got it now in RESULT(1:L)
C     check for a magtape
      CALL T_LOGICAL(RESULT(1:L),NDRIVE(IOLUN))
      IF(NDRIVE(IOLUN).GE.0)GOTO 80
C     we've got a disk file on our hands
60    CONTINUE
D     TYPE *,'THIS WILL BE A DISK UNIT'
      NDRIVE(IOLUN) = -1
      IF(WRITE(IOLUN))THEN
         IF(.NOT.D_OPEN(IOLUN,'W',RESULT(1:L)))THEN
            TYPE *,'UNIT ',IOLUN,' FILENAME ',RESULT(1:L)
            STOP 'IOPACK UNABLE TO OPEN DISK FILE FOR WRITE'
         END IF
      ELSE
         IF(.NOT.D_OPEN(IOLUN,'R',RESULT(1:L)))THEN
            TYPE *,'UNIT ',IOLUN,' FILENAME ',RESULT(1:L)
            STOP 'IOPACK UNABLE TO OPEN DISK FILE FOR READ'
         END IF
      END IF
80    CONTINUE
      FIRST(IOLUN) = .FALSE.
      RETURN
      END
      SUBROUTINE IOPEN(IOLUN,NAME)
C     ++++++++++
C     CALL IOPEN(IOLUN,NAME)
C     This routine associates a disk or tape filename
C     with a given IOPACK LUN.  It should be called
C     to specify what file to access.  If it is not,
C     then unit n will refer to logical name IOP00n.
C     ----------
      CHARACTER*(*) NAME
      CHARACTER*255 LOGIC
      INCLUDE 'IOPACK.INC'
      INTEGER*4 SYS$CRELOG
      EXTERNAL SS$_ABORT
      IF(.NOT.FIRST(IOLUN))THEN
         TYPE *,'ILLEGAL ATTEMPT TO IOPEN AN OPEN LUN'
         CALL SYS$EXIT(SS$_ABORT)
C        force in CFIRST to initialize COMMON
         CALL CFIRST
      END IF
      WRITE(LOGIC,100)'IOP',IOLUN
100   FORMAT(A3,I3.3)
      CALL IFERR(SYS$CRELOG(%VAL(2),LOGIC(1:6),NAME,))
      RETURN
      END
      SUBROUTINE IOCLOS(IOLUN)
C     ++++++++++
C     CALL IOCLOS(IOLUN) where IOLUN is the
C     number of the IOPACK unit to close.
C     Note that IOCLOS of a tape which was
C     written to causes two extra EOFs to appended.
C     The tape is left positioned between the two EOFs.
C     ----------      
      INCLUDE 'IOPACK.INC'
      IF(NDRIVE(IOLUN).LT.0)THEN
C        it's a disk drive
         IF(FIRST(IOLUN))RETURN
         CALL D_CLOS(IOLUN)
      ELSE
C        it's a tape drive
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         WAIT(IOLUN) = .FALSE.
C        if they wrote on it write an EOF at the end
         IF(WRITE(IOLUN))CALL T_WEOF(NDRIVE(IOLUN))
         IF(WRITE(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         IF(WRITE(IOLUN))CALL T_WEOF(NDRIVE(IOLUN))
         IF(WRITE(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         IF(WRITE(IOLUN))CALL T_SKPF(NDRIVE(IOLUN),-1)
         IF(WRITE(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
      END IF
C     in either case set FIRST again
      FIRST(IOLUN) = .TRUE.
C     force load CFIRST
      IF(.NOT.FIRST(IOLUN))CALL CFIRST
      RETURN
      END
      SUBROUTINE BUFFEROUT(IOLUN,PARITY,IFWA,ILWA)
C     ++++++++++
C     CALL BUFFEROUT(IOLUN,PARITY,IFWA,ILWA)   ,where
C     IOLUN is the IOPACK LUN involved (these are
C     separate and distinct from FORTRAN LUNs and VMS
C     channels as well.
C     PARITY is ignored and exists only for compatibility
C     with systems on the Mesa.
C     IFWA is the first word address to be written out.
C     ILWA is the last word address to be written out.
C     It is assumed that each word is exactly 4 bytes long
C     (which is the case for normal integers or reals).
C     BUFFEROUT is an asynchronous operation which is very
C     similar to WRTAPE.
C     ----------
      INCLUDE 'IOPACK.INC'
      WRITE(IOLUN) = .TRUE.
      CALL CFIRST(IOLUN)
      NBYTES = %LOC(ILWA) - %LOC(IFWA) + 4
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
         CALL D_PUT(IOLUN,IFWA,NBYTES)
      ELSE
C        tape
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         CALL T_WRIT(NDRIVE(IOLUN),IFWA,NBYTES)
         WAIT(IOLUN) = .TRUE.
      END IF
      RETURN
      END
      SUBROUTINE WRTAPE(IOLUN,MODE,NTYPE,NADDR,NWDCNT)
C     ++++++++++
C     CALL WRTAPE(IOLUN,MODE,NTYPE,NADDR,NBYTES)
C     IOLUN is the IOPACK LUN which is completely separate & distinct
C     from FORTRAN LUNs or VMS channels.
C     MODE & NTYPE are ignored and exist only for compatibility with
C     implementations on the Mesa.
C     NADDR is the starting address of the data to write out.
C     NBYTES is the number of 8 bit bytes (not words) to write out.
C     WRTAPE is asynchronous!!!!
C     ----------
C        NB:
C        NWDCNT IN WRTAPE IS THE BYTE COUNT NOT THE WORD COUNT
C
      INCLUDE 'IOPACK.INC'
      WRITE(IOLUN) = .TRUE.
      CALL CFIRST(IOLUN)
      NBYTES = NWDCNT
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
         CALL D_PUT(IOLUN,NADDR,NBYTES)
      ELSE
C        tape
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         CALL T_WRIT(NDRIVE(IOLUN),NADDR,NBYTES)
         WAIT(IOLUN) = .TRUE.
      END IF
      RETURN
      END
      SUBROUTINE BUFFERIN(IOLUN,PARITY,IFWA,ILWA)
C     ++++++++++
C     CALL BUFFERIN(IOLUN,PARITY,IFWA,ILWA)
C     IOLUN is the IOPACK LUN which is separate & distinct from
C     a FORTRAN LUN or a VMS channel.
C     PARITY is ignored and exists only for compatibility
C     with the systems on the Mesa.
C     IFWA is the address of the first word to read in.
C     ILWA is the address of the last word to read in.
C     It is assumed that the words being read in are
C     each 4 bytes long (as are normal integers and reals).
C     This routine is asynchronous & is very similar to RDTAPE.
C     ----------
      INCLUDE 'IOPACK.INC'
      WRITE(IOLUN) = .FALSE.
      CALL CFIRST(IOLUN)
      NBYTES = %LOC(ILWA) - %LOC(IFWA) + 4
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
         CALL D_GET(IOLUN,IFWA,NBYTES)
      ELSE
C        tape
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         CALL T_READ(NDRIVE(IOLUN),IFWA,NBYTES)
         WAIT(IOLUN) = .TRUE.
      END IF
      RETURN
      END
      SUBROUTINE RDTAPE(IOLUN,MODE,NTYPE,NADDR,NWDCNT)
C     ++++++++++
C     CALL RDTAPE(IOLUN,MODE,NTYPE,NADDR,NBYTES)
C     IOLUN is the IOPACK LUN which is separate and distinct
C     from FORTRAN LUNs or VMS channels.
C     MODE & NTYPE are ignored and exist only for compatibility
C     with implementations of this routine on the Mesa.
C     NADDR is the starting address to read into.
C     NBYTES is the maximum number of bytes (not words) to read in.
C     This routine is asynchronous!
C     ----------
C
C        NB:
C        NWDCNT IS THE BYTE COUNT NOT THE WORD COUNT
C
      INCLUDE 'IOPACK.INC'
      WRITE(IOLUN) = .FALSE.
      CALL CFIRST(IOLUN)
      NBYTES = NWDCNT
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
D        TYPE *,'DISK'
         CALL D_GET(IOLUN,NADDR,NBYTES)
      ELSE
C        tape
C        if they didn't do a wait, insert one
D        TYPE *,'TAPE'
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         CALL T_READ(NDRIVE(IOLUN),NADDR,NBYTES)
         WAIT(IOLUN) = .TRUE.
      END IF
      RETURN
      END
      INTEGER*4 FUNCTION LENGTHF(IOLUN)
C     ++++++++++
C     NBYTES = LENGTHF(IOLUN)
C     This routine returns the number of WORDS (not bytes)
C     involved in the last transfer on IOPACK
C     LUN IOLUN.  It also forces completion
C     of all outstanding transfers on the unit.
C     ----------
C
C        NB:
C        LENGTHF RETURNS THE INTEGER*4 WORD COUNT
C        TO GET BYTE COUNT USE IOWAIT
C
      INCLUDE 'IOPACK.INC'
      INTEGER*4 D_LEN
      EXTERNAL SS$_ABORT
      IF(FIRST(IOLUN))THEN
         TYPE *,'CALL TO LENGTHF PRECEDES FIRST TRANSFER'
         CALL SYS$EXIT(SS$_ABORT)
C        force load CFIRST
         CALL CFIRST
      END IF
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
         LENGTHF = (D_LEN(IOLUN)+3)/4
      ELSE
C        tape
         CALL T_WAIT(NDRIVE(IOLUN),NSTATE,LENG)
         LENGTHF = (LENG+3)/4
         WAIT(IOLUN) = .FALSE.
      END IF
      RETURN
      END
      REAL FUNCTION UNIT(IOLUN)
C     ++++++++++
C     IF(UNIT(IOLUN))10,20,30
C     Causes control to be transferred to statement 10
C        on success, statement 20 on EOF, and statement 30 on error.
C     IOLUN is the usual IOPACK LUN.
C     The actual values returned are:
C        SUCCESS -1.0
C        EOF      0.0
C        ERROR    1.0
C     Execution of this function forces completion of any
C     outstanding requests on IOLUN.
C     ----------
C
C        NB:
C        THE 7600 VERSION OF UNIT ALLOWED A 4-WAY
C        BRANCH.  THE FIRST BRANCH WAS NEVER TAKEN
C        (IT REPRESENTED OPERATION NOT FINISHED).
C        SINCE 4-WAY BRANCHES ARE NON-STANDARD WE
C        HAVE SIMPLY OMITTED THE FIRST BRANCH CONDITION.
C        FOR 3-WAY BRANCHING, UNIT < 0.0 MEANS SUCCESS,
C                             UNIT = 0.0 MEANS EOF, AND
C                             UNIT > 0.0 MEANS ERROR.
C
      INCLUDE 'IOPACK.INC'
      EXTERNAL SS$_ABORT
      IF(FIRST(IOLUN))THEN
         TYPE *,'CALL TO UNIT PRECEDES FIRST TRANSFER'
         CALL SYS$EXIT(SS$_ABORT)
C        force load CFIRST
         CALL CFIRST
      END IF
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
         UNIT = D_UNIT(IOLUN)
      ELSE
C        tape
         CALL T_WAIT(NDRIVE(IOLUN),NSTATE,LENG)
         UNIT = NSTATE - 1
         WAIT(IOLUN) = .FALSE.
      END IF
      RETURN
      END
      SUBROUTINE IOWAIT(IOLUN,NSTATE,NWDS)
C     ++++++++++
C     CALL IOWAIT(IOLUN,NSTATE,NBYTES)
C     This routine causes completion of any outstanding IO on
C     IOPACK LUN IOLUN.  Then it returns the number of bytes
C     (not words) transferred in NBYTES and sets NSTATE as follows:
C        SUCCESS  0
C        EOF      1
C        ERROR    2, and potentially for tape transfers (no guarantees!)
C        EOT      3
C     ----------
C
C        NB:
C        NWDS IS ACTUALLY NUMBER OF BYTES
C        TO GET NUMBER OF WORDS USE LENGTHF
C
      INCLUDE 'IOPACK.INC'
      INTEGER*4 D_LEN
      EXTERNAL SS$_ABORT
      IF(FIRST(IOLUN))THEN
         TYPE *,'CALL TO IOWAIT PRECEDES FIRST TRANSFER'
         CALL SYS$EXIT(SS$_ABORT)
C        force load CFIRST
         CALL CFIRST
      END IF
      IF(NDRIVE(IOLUN).LT.0)THEN
C        disk
D        x = d_unit(iolun)
         NSTATE = D_UNIT(IOLUN) + 1.5
D        type *,'d_unit',x,nstate
         NWDS = D_LEN(IOLUN)
      ELSE
C        tape
         CALL T_WAIT(NDRIVE(IOLUN),NSTATE,NWDS)
         WAIT(IOLUN) = .FALSE.
      END IF
      RETURN
      END
      SUBROUTINE ENDFILE(IOLUN)
C     ++++++++++
C     CALL ENDFILE(IOLUN)
C     This fairly useless routine just calls IOCLOS(IOLUN).
C     It is here for compatibility with Mesa implementations.
C     ----------
      CALL IOCLOS(IOLUN)
      RETURN
      END
      SUBROUTINE SKIPFILE(IOLUN)
C     ++++++++++
C     CALL SKIPFILE(IOLUN)
C     This routine works only for magtapes and
C     allows you to skip forward past the next EOF marker.
C     IOLUN is the usual IOPACK LUN.
C     ----------
C
C        NB:
C        ONLY WORKS ON TAPES
C
      INCLUDE 'IOPACK.INC'
      EXTERNAL SS$_ABORT
      N = 1
      GOTO 10
      ENTRY BACKFILE(IOLUN)
C     ++++++++++
C     CALL BACKFILE(IOLUN)
C     This routine which only works for real
C     magtapes, positions you immediately before
C     the last previous EOF on the tape associated
C     with IOPACK LUN IOLUN.
C     ----------
      N = -1
10    WRITE(IOLUN) = .FALSE.
      CALL CFIRST(IOLUN)
      IF(NDRIVE(IOLUN).LT.0)THEN
         TYPE *,'CAN''T SKIP DISK FILES'
         CALL SYS$EXIT(SS$_ABORT)
      END IF
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
      CALL T_SKPF(NDRIVE(IOLUN),N)
         WAIT(IOLUN) = .TRUE.
      RETURN
      END
      SUBROUTINE SKIPSPACE(IOLUN)
C     ++++++++++
C     CALL SKIPSPACE(IOLUN)
C     This routine allows you to skip forward
C     over one physical record.
C     It only works if the device associated with 
C     IOPACK LUN IOLUN is a real tape drive.
C     ----------
C
C        NB:
C        FOR THE MOMENT ONLY WORKS ON TAPES
C
      INCLUDE 'IOPACK.INC'
      EXTERNAL SS$_ABORT
      N = 1
      GOTO 10
      ENTRY BACKSPACE(IOLUN)
C     ++++++++++
C     CALL BACKSPACE(IOLUN)
C     Only works for a real tape drive connected
C     to IOPACK LUN IOLUN.
C     Backs up 1 record.
C     ----------
      ENTRY BSTAPE(IOLUN,MODE)
C     ++++++++++
C     CALL BSTAPE(IOLUN,MODE)
C     Only works for a real tape drive connected
C     to IOPACK LUN IOLUN.
C     MODE is ignored and exists solely for compatibility
C     with Mesa implementations.
C     This routine backs up 1 physical record on tape.
C     ----------
      N = -1
10    WRITE(IOLUN) = .FALSE.
      CALL CFIRST(IOLUN)
      IF(NDRIVE(IOLUN).LT.0)THEN
         TYPE *,'CAN''T SKIP DISK RECORDS FOR NOW'
         CALL SYS$EXIT(SS$_ABORT)
      END IF
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
      CALL T_SKPR(NDRIVE(IOLUN),N)
         WAIT(IOLUN) = .TRUE.
      RETURN
      END
      SUBROUTINE REWIND(IOLUN)
C     ++++++++++
C     CALL REWIND(IOLUN)
C     This routine rewinds the IOPACK LUN IOLUN.
C     Oddly enough, this routine works for tapes
C     or disks!
C     ----------
C
      INCLUDE 'IOPACK.INC'
      INTEGER*2 SPOT(3)
      DATA SPOT/1,0,0/
10    WRITE(IOLUN) = .FALSE.
      CALL CFIRST(IOLUN)
      IF(NDRIVE(IOLUN).LT.0)THEN
         CALL D_JUMP(IOLUN,SPOT)
      ELSE
C        if they didn't do a wait, insert one
         IF(WAIT(IOLUN))CALL T_WAIT(NDRIVE(IOLUN),M,MM)
         CALL T_RWND(NDRIVE(IOLUN))
         WAIT(IOLUN) = .TRUE.
      END IF
      RETURN
      END
