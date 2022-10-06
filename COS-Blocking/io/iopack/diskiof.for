      INTEGER*4 FUNCTION D_READ(LUN,BUFF,SIZE,LEN)
      IMPLICIT NONE
C     ++++++++++
C     CALL IFERR(D_READ(LUN,BUFFER,SIZE,LEN)), where
C     LUN is the usual IOLUN,
C     BUFF is the address to read into,
C     SIZE is the size of the input buffer, and
C     LEN is to be returned with the number of bytes read in.
C     This routine is simply an easy to use combination of
C     calls to D_GET, D_STAT_, & D_LEN.
C     It returns a standard VMS error or success code.
C     ----------
      INTEGER*4 D_GET,D_STAT_,D_LEN,LUN,SIZE,LEN
      BYTE BUFF(SIZE)
      D_READ = D_GET(LUN,BUFF,SIZE)
      LEN = 0
      IF (.NOT.D_READ)RETURN
      D_READ = D_STAT_(LUN)
      IF (.NOT.D_READ)RETURN
      LEN = D_LEN(LUN)
      RETURN
      END
      INTEGER*4 FUNCTION D_READC(LUN,LINE,LNN)
      IMPLICIT NONE
C     ++++++++++
C     CALL IFERR(D_READC(LUN,LINE[,LEN])), where
C     LUN is the usual IOLUN unit,
C     LINE is a character variable to be read into, and
C     LEN is optionally returned with how many characters were
C     read in.  LINE is always blank padded at the end!
C     This routine just calls D_READ.
      CHARACTER*(*) LINE
      INTEGER*4 LUN,LN,LNN,D_READ,NARGS
      D_READC = D_READ(LUN,%REF(LINE),LEN(LINE),LN)
      LINE(LN+1:) = ' '
      IF(NARGS().GE.3)LNN = LN
      RETURN
      END
      INTEGER*4 FUNCTION D_WRITE(LUN,BUFF,SIZE)
      IMPLICIT NONE
C     ++++++++++
C     CALL IFERR(D_WRITE(LUN,BUFF,SIZE)), where
C     LUN is the usual IOLUN,
C     BUFF is the address for the line to write out, and
C     SIZE is the number of bytes to be written.
C     This routine is just a simple to use combination of
C     calls to D_PUT and D_STAT_.
C     It returns a standard VMS error or success code.
C     ----------
      INTEGER*4 D_PUT,D_STAT_,LUN,SIZE
      BYTE BUFF(SIZE)
      D_WRITE = D_PUT(LUN,BUFF,SIZE)
      IF (.NOT.D_WRITE) RETURN
      D_WRITE = D_STAT_(LUN)
      RETURN
      END
      INTEGER*4 FUNCTION D_WRITEC(LUN,LINE)
      IMPLICIT NONE
C     ++++++++++
C     CALL IFERR(D_WRITEC(LUN,LINE)), where
C     LUN is the usual IOLUN, and
C     LINE is the character variable to be written out.
C     This routine just calls D_WRITE.
C     ----------
      INTEGER*4 D_WRITE,LUN,SIZE
      CHARACTER*(*) LINE
      D_WRITEC = D_WRITE(LUN,%REF(LINE),LEN(LINE))
      RETURN
      END
