      SUBROUTINE RPTOUT(NUNIT,NBUF,LOCRPT,NWDS,JL)
C***********************************************************************
C
C DIMENSION OF
C ARGUMENTS              NBUF(1006)
C
C LATEST REVISION        DECEMBER, 1991
C
C PURPOSE                TO WRITE ONE LOGICAL RECORD.
C                        SEE THE REFERENCE BELOW FOR A COMPLETE
C                        DESCRIPTION OF THE LOGICAL AND PHYSICAL
C                        RECORDS UNDER CONSIDERATION.
C
C USAGE                  CALL RPTOUT(NUNIT,NBUF,LOCRPT,NWDS,JL)
C
C ARGUMENTS
C ON INPUT               NUNIT
C                          LOGICAL UNIT NUMBER  (NOT 5 OR 6)
C
C                        LOCRPT
C                          LOCATION OF THE REPORT FOR OUTPUT.
C                          THE FIRST 12 BITS OF THE REPORT WILL BE
C                          USED BY  RPTIN/RPTOUT. ANY DATA CONTAINED
C                          IN THE FIRST 12 BITS OF THE FIRST WORD
C                          WILL BE DESTROYED.
C
C                        NWDS
C                          NUMBER OF WORDS IN THE REPORT
C
C                        JL
C                          =1  OUTPUT THIS REPORT, ASSUMING
C                              A 1006-WORD NBUF
C                          =2  NO REPORT TO OUTPUT. OUTPUT THE REPORTS
C                              IN NBUF ONTO THE TAPE. THIS IS USED
C                              TO OUTPUT THE LAST BUFFER ONTO THE
C                              TAPE. THIS ALSO SETS NBUF(1) TO ZERO.
C
C
C ARGUMENTS
C ON OUTPUT              NBUF
C                          AN ARRAY IN WHICH THE RECORDS ARE BUILT.
C
C                        NBUF(1)
C                          MUST BE 0 BEFORE THE FIRST WRITE ON A
C                          TAPE. RPTOUT WILL START A RECORD WHEN
C                          IT SEES THE 0.
C
C                        NBUF(2-4)
C                          MAY BE CLEARED TO INITIALIZE THE COUNTERS.
C
C                        NBUF(2)
C                          WILL HAVE A COUNT OF LOGICAL RECORDS
C                          OUTPUT.
C
C                        NBUF(3)
C                          WILL HAVE A COUNT OF PHYSICAL RECORDS
C                          OUTPUT.
C
C                        NBUF(4)
C                          WILL BE A COUNT OF THE WORDS OUTPUT.
C                          BUT WORDS IN REPORTS = NBUF(4)-2*NBUF(3).
C
C                        NBUF(5)
C                          NOT USED BY RPTOUT
C
C                        NBUF(6)
C                          CONTAINS THE STATUS OF THE LAST PHYSICAL
C                          WRITE. MAY BE TESTED TO DETECT AN EOT
C                          CONDITION (NBUF(6)=3).
C
C I/O                    THE OUTPUT UNIT IS LOGICAL UNIT NUNIT.
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       NONE, BUT SEE COMPANION UTILITY RPTIN
C FILES                  ON CRAYLIB.
C
C LANGUAGE               FORTRAN
C
C HISTORY                ORIGINALLY WRITTEN BY ROY JENNE AT NCAR
C                        IN 1966. ADAPTED FOR THE CRAY-1 BY MEMBERS
C                        OF NCAR'S SCIENTIFIC COMPUTING DIVISION
C                        IN JANUARY, 1983.
C
C PORTABILITY            NO PORTABILITY EFFORT HAS BEEN MADE.
C
C REFERENCES             PP. 7-14 THROUGH 7-18 OF THE NCAR
C                        "CONVERSION HANDBOOK", PUBLISHED
C                         JANUARY, 1983.
C
C***********************************************************************
      DIMENSION NBUF(1),LOCRPT(1)
      DATA KLMAX/998/
      DATA KMAX/1006/
      IF (NBUF(1) .NE. 0)  GO TO 4
      NBUF(6)=0
      NBUF(1) = 8
    4 CONTINUE
      IF(NBUF(6) .EQ. 3) GO TO 80
      IF (JL .EQ. 2)  GO TO 59
      IF (NWDS .GT. KLMAX)  GO TO 70
      IF (NWDS .LE. 0) GO TO 70
   10 NN = NBUF(1) + NWDS
      IF(NN .GT. KMAX)  GO TO 60
C  MOVE THE REPORT INTO THE BUFFER
      K = NBUF(1)
C SHIFT AND STORE LOG REC LENGTH
C     NKWDS=NWDS*200000000000000000B
C     LOCRPT(1)=LOCRPT(1) .AND. 7777777777777777B
C     LOCRPT(1)=LOCRPT(1) .OR. NKWDS
      CALL SBYTE(LOCRPT,NWDS,0,12)
      DO 6160 I=1,NWDS
 6160 NBUF(K+I-1)=LOCRPT(I)
      NBUF(2) = NBUF(2) + 1
      NBUF(1) = NN
      RETURN
C
C  OUTPUT A RECORD HERE
   60 IF(NBUF(1) .GT. KMAX)  GO TO 61
   59 IF (NBUF(1) .EQ. 0)  RETURN
      KKK = NBUF(1)
      KKWDS = KKK-6
      KKDAT = KKK-1
      NBUF(7)=100000000000000000000B.OR.KKWDS
      NBUF(KKK)=0
      DO 100 I=7,KKDAT
      NBUF(KKK)=NBUF(KKK)+NBUF(I)
  100 CONTINUE
      IF (KKWDS .LT. 3)  GO TO 61
      CALL WRTAPE(NUNIT,1,0,NBUF(7),KKWDS)
      NBUF(3) = NBUF(3) + 1
      NBUF(4) = NBUF(4) + KKWDS
      NBUF(1) = 8
      CALL IOWAIT(NUNIT,NSTATE,NWDWRT)
      NBUF(6)=NSTATE
      IF(NSTATE .EQ. 3) GO TO 63
      IF (NSTATE .NE. 0)  GO TO 61
      IF (NWDWRT .EQ. KKWDS)  GO TO 63
   61 PRINT 962,NUNIT,NBUF(3),NWDWRT,NSTATE
  962 FORMAT (* RPTOUT--BAD PHYSICAL WRITE. UNIT, PHYSICAL RECORD, LENGT
     *H, STATUS*,4I8)
   63 IF (JL .NE. 2)  GO TO 10
   65 IF(NBUF(6) .EQ. 3) RETURN
      NBUF(1)=0
      RETURN
C  HERE FOR RECORD LENGTH MYSTERY
   70 PRINT 960,NUNIT,NBUF(2),NWDS
  960 FORMAT (* RPTOUT--BAD LOGICAL LENGTH. UNIT, LOGICAL RECORD, LENGTH
     **,3I8)
      RETURN
   80 CONTINUE
      PRINT 964
  964 FORMAT(* RPTOUT--ATTEMPT TO WRITE AFTER EOT, DATA MAY HAVE BEEN LO
     *ST*)
      RETURN
C REVISION HISTORY:
C
C DECEMBER, 1991   Change output variable LOCRPT. Previous description:
C                  LOCRPT
C                     LOCATION OF THE REPORT FOR OUTPUT.
C                     THE FIRST 16 BITS OF THE REPORT WILL BE
C                     USED BY  RPTIN/RPTOUT. ANY DATA CONTAINED
C                     IN THE FIRST 16 BITS OF THE FIRST WORD
C                     WILL BE DESTROYED.
C                  Current description: change 16 TO 12.
C                  Reason for change: Dennis Joseph and Will Spangler
C                  requested it.  RPTOUT does not zero bits 13-16 of
C                  the first word in the report array LOCRPT, as it 
C                  formerly did.  This change will allow data to be 
C                  stored in these bits by members of the (NOAA/NCAR) 
C                  COADS project.  Because these bits have not been
C                  used in the past, this change will not affect past 
C                  users of this software.
      END
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
