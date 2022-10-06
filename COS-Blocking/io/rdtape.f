      SUBROUTINE WRTAPE (NUN,MOD,NTY,BUF,NWD)
C***********************************************************************
C
C LATEST REVISION        DECEMBER 1990
C
C PURPOSE                TO READ OR WRITE PHYSICAL RECORDS FROM OR TO
C                        A CRAY LOGICAL UNIT.  THE PACKAGE CANNOT BE 
C                        USED TO READ OR WRITE TAPES DIRECTLY.
C
C                        IT IS PROVIDED FOR CONVERSION EASE OF 7600
C                        PROGRAMS TO CRAY PROGRAMS.
C
C USAGE                  CALL WRTAPE(NUN,MOD,NTY,BUF,NWD)
C                        CALL RDTAPE(NUN,MOD,NTY,BUF,NWD)
C                        CALL IOWAIT(NUN,NS,KWD)
C
C ARGUMENTS
C
C ON INPUT
C FOR WRTAPE             NUN
C                          LOGICAL UNIT ON WHICH DATA IS TO BE WRITTEN
C                        MOD
C                          INTEGER DUMMY ARGUMENT.  THIS ARGUMENT
C                          FORMERLY GOVERNED CONVERSION.  AT PRESENT,
C                          THE DATA IS NOT CONVERTED.
C                        NTY
C                          INTEGER DUMMY ARGUMENT.  THIS ARGUMENT
C                          FORMERLY GOVERNED RECORD READS FROM NON-NCAR
C                          TAPES.
C                        BUF
C                          FIRST WORD CONTAINING DATA TO BE WRITTEN
C                          TO THE RECORD
C                        NWD
C                          NUMBER OF WORDS TO BE WRITTEN TO THE
C                          THE RECORD
C
C ON OUTPUT
C FOR WRTAPE             ALL ARGUMENTS ARE UNCHANGED
C
C
C ON INPUT
C FOR RDTAPE             NUN
C                          LOGICAL UNIT FROM WHICH DATA IS TO BE READ
C                        MOD
C                          INTEGER DUMMY ARGUMENT.  THIS ARGUMENT
C                          FORMERLY GOVERNED CONVERSION.  AT PRESENT,
C                          THE DATA IS NOT CONVERTED.
C                        NTY
C                          INTEGER DUMMY ARGUMENT.  THIS ARGUMENT
C                          FORMERLY GOVERNED RECORD READS FROM NON-NCAR
C                          TAPES.
C                        BUF
C                          FIRST WORD AT WHICH DATA RECORD IS TO
C                          BE STORED
C                        NWD
C                          NUMBER OF WORDS TO BE READ OR WRITTEN FROM
C                          THE RECORD, OR MAXIMUM RECORD SIZE
C
C ON OUTPUT
C FOR RDTAPE             BUF
C                          CONTAINS READ RECORD
C
C ON INPUT
C FOR IOWAIT             NUN
C                          LOGICAL UNIT BEING READ OR WRITTEN
C
C ON OUTPUT
C FOR IOWAIT             NS
C                          STATUS
C                            0  GOOD READ OR WRITE
C                            1  EOF
C                            2  PARITY ERROR ON READ OR UNABLE TO WRITE
C                        KWD
C                          NUMBER OF CRAY 64-BIT WORDS READ OR WRITTEN
C
C I/O                    READ PHYSICAL RECORDS FROM A CRAY
C                        LOGICAL UNIT TO CRAY MEMORY.
C                        WRITE PHYSICAL RECORDS TO CRAY LOGICAL
C                        UNIT FROM CRAY MEMORY.
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       NONE
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                WRITTEN IN 1978 BY MEMBERS OF NCAR'S
C                        SCIENTIFIC COMPUTING DIVISION.
C
C PORTABILITY            NO PORTABILITY EFFORT HAS BEEN MADE
C                        FOR THIS UTILITY.
C
C METHOD                 USES CRAY SOFTWARE BUFFER IN,
C                        AND BUFFER OUT.
C
C***********************************************************************
      DIMENSION       BUF(1)
C     LOGICAL Q8Q4
C     SAVE Q8Q4
C     DATA Q8Q4 /.TRUE./
C     IF (Q8Q4) THEN
C         CALL Q8QST4('CRAYLIB','IOPROC','WRTAPE','VERSION 01')
C         Q8Q4 = .FALSE.
C     ENDIF
      BUFFER OUT (NUN,1) (BUF(1),BUF(NWD))
      RETURN
      END
      SUBROUTINE RDTAPE (NUN,MOD,NTY,BUF,NWD)
      DIMENSION       BUF(1)
C     LOGICAL Q8Q4
C     SAVE Q8Q4
C     DATA Q8Q4 /.TRUE./
C     IF (Q8Q4) THEN
C         CALL Q8QST4('CRAYLIB','IOPROC','RDTAPE','VERSION 01')
C         Q8Q4 = .FALSE.
C     ENDIF
      BUFFER IN (NUN,1) (BUF(1),BUF(NWD))
      RETURN
      END
      SUBROUTINE IOWAIT (NUN,NS,KWD)
      KWD = LENGTH(NUN)
      IF (UNIT(NUN))  10, 20, 60
   10 CONTINUE
      NS = 0
      RETURN
   20 CONTINUE
      NS = 1
      RETURN
   60 CONTINUE
      NS = 2
      RETURN
      END
