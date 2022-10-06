      SUBROUTINE RPTIN (NUNIT,NBUF,LOCRPT,NWDS,JJ,KLMAX,JEOF)
C***********************************************************************
C
C DIMENSION OF
C ARGUMENTS              NBUF(1006)
C
C LATEST REVISION        NOVEMBER, 1984
C
C PURPOSE                RPTIN IS USED TO READ THE NEXT LOGICAL
C                        RECORD FROM A FILE WRITTEN USING SUBROUTINE
C                        RPTOUT. SEE THE REFERENCE MENTIONED BELOW
C                        FOR A COMPLETE DESCRIPTION OF THE LOGICAL
C                        AND PHYSICAL RECORDS UNDER CONSIDERATION.
C
C
C USAGE                  CALL RPTIN (NUNIT,NBUF,LOCRPT,NWDS,JJ,KLMAX,
C                                    JEOF)
C ARGUMENTS
C ON INPUT               NUNIT
C                          LOGICAL UNIT NUMBER (NOT 5 OR 6)
C
C                        NBUF
C                          NBUF(1) MUST BE SET TO 0 BEFORE THE
C                          FIRST READ. RPTIN WILL READ A NEW
C                          PHYSICAL RECORD WHEN NBUF(1)=0.
C
C                        JJ
C                          NO LONGER USED, BUT INCLUDED TO MAINTAIN
C                          COMPATIBILITY WITH EXISTING PROGRAMS.
C                          USE JJ = 1.
C
C                        KLMAX
C                          MAXIMUM NUMBER OF WORDS IN A REPORT TO
C                          MOVE TO LOCRPT.
C
C ARGUMENTS
C ON OUTPUT              NBUF
C                          AN ARRAY TO USE FOR UNPACKING THE RECORDS.
C                          DIMENSION NBUF (1006).
C
C                        NBUF(2-6)
C                          MAY BE CLEARED WHEN THE USER WISHES TO
C                          REINITIALIZE THESE COUNTERS.
C
C                        NBUF(2)
C                          WILL HAVE A COUNT OF LOGICAL RECORDS READ.
C
C                        NBUF(3)
C                          WILL HAVE A COUNT OF PHYSICAL RECORDS READ.
C
C                        NBUF(4)
C                          WILL HAVE A COUNT OF WORDS READ FROM THE
C                          THE TAPE.
C
C                        NBUF(5)
C                          WILL HAVE THE COUNT OF BITS IN THE PHYSICAL
C                          RECORD, EXCLUDING THE CHECKSUM.
C
C                        NBUF(6)
C                          WILL BE THE SAME AS JEOF
C
C                        LOCRPT
C                          ARRAY TO PUT REPORT IN
C
C                        NWDS
C                          THE TOTAL NUMBER OF WORDS IN THIS REPORT
C
C                        JEOF
C                          =0   GOOD REPORT RETURNED
C                          =1   EOF
C                          =2   REPORT RETURNED FROM A RECORD WITH
C                               A BAD CHECKSUM
C                          =3   EOT. THE LOGICAL RECORD RETURNED
C                               WITH THIS JEOF IS THE LAST ON THE
C                               CURRENT TAPE.
C
C I/O                    INPUT IS ACCEPTED ON LOGICAL UNIT NUNIT.
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       NONE, BUT THERE IS A COMPANION UTILITY
C FILES                  RPTOUT ON CRAYLIB.
C
C LANGUAGE               FORTRAN
C
C HISTORY                ORIGINALLY WRITTEN BY ROY JENNE AT NCAR
C                        IN 1966. ADAPTED FOR THE CRAY-1 BY MEMBERS
C                        OF NCAR'S SCIENTIFIC COMPUTING DIVISION
C                        IN JANUARY 1983.
C
C PORTABILITY            NO PORTABILITY EFFORT HAS BEEN MADE.
C
C REFERENCES             PP. 7-14 THROUGH 7-18 OF THE NCAR
C                        "CONVERSION HANDBOOK" PUBLISHED JANUARY 1983.
C
C***********************************************************************
      DIMENSION NBUF(1),LOCRPT(1)
      DIMENSION LIN(2),LOT(2),XF(2),YF(2),LGET(2),LOFF(2)
      DATA KMAX/1000/
C
C FOR THE FOLLOWING INITIALIZATIONS:
C     INDEX 1 IS FOR 7600, INDEX 2 IS FOR CRAY1
C
      DATA LIN/60,64/
      DATA LOT/64,64/
      DATA XF/1.0667,1./
      DATA YF/.9375,1./
      DATA LOFF/0,4/
      DATA LGET/60,60/
C
C IF NBUF(1) .EQ. 0, READ FIRST BUFFER (PHYSICAL RECORD)
C IF NBUF(1) .NE. 7, CURRENT BUFFER NOT EXHAUSTED, GET NEXT LOGICAL RECO
C IF NBUF(1) .EQ. 7, READING NEXT BUFFER IN PROGRESS, GET STATUS
C
      IF (NBUF(1) .EQ. 0) GO TO 3
      IF (NBUF(1) .NE. 7) GO TO 40
      GO TO 6
    3 CALL IOWAIT(NUNIT,NBUF(6),NWRED)
    4 CONTINUE
    5 CALL RDTAPE(NUNIT,1,2,NBUF(7),KMAX)
    6 CALL IOWAIT(NUNIT,NBUF(6),NWRED)
C
C DETERMINE PHYSICAL RECORD CHARACTERISTICS
C
      CALL GBYTE(NBUF(7),KM,0,4)
      KM=KM+1
      NBUF(1)=8
      JEOF=NBUF(6)
      IF (JEOF .EQ. 3) GO TO 41
      IF (JEOF .EQ. 1) GO TO 42
      NTST=XF(KM)*NWRED+.01
      CALL GBYTE(NBUF(7),IWDS,LOFF(KM),LGET(KM))
      IF (NTST .GT. 2) GO TO 10
      IEREC=NBUF(3)+1
      PRINT 960,NUNIT,IEREC,NWRED,IWDS,NBUF(6)
  960 FORMAT(* RPTIN--SHORT PHYSICAL RECORD.  UNIT,PHYSICAL RECORD, LENG
     *TH, EXPECTED LENGTH, STATUS.*,5I8)
   10 CONTINUE
      IF(KM.GE.1.AND.KM.LE.2) GO TO 9
      IEREC=NBUF(3)+1
      PRINT 959,NUNIT,IEREC,NWRED
  959 FORMAT(* THIS IS NOT A RPTIN RECORD. UNIT, PHYSICAL  RECORD, LENGT
     *H*,3I8)
    9 CONTINUE
      IF(KM.EQ.1) CALL N76SUM(NBUF(7),IWDS,NERR)
      IF(KM.EQ.2) CALL NCRYSM(NBUF(7),IWDS,NERR)
      IF(NERR .EQ. 0) GO TO 15
      IEREC=NBUF(3)+1
      PRINT 1001,NUNIT,IEREC,NWRED,IWDS,NBUF(6)
 1001 FORMAT(* RPTIN--BAD CHECKSUM. UNIT, PHYSICAL RECORD, LENGTH, EXPEC
     *TED LENGTH, STATUS*,5I8)
   15 CONTINUE
      NWU=(XF(KM)*NWRED)+0.01
      NWL=NWU-XF(KM)+.99
      IF (IWDS .GE. NWL .AND. IWDS .LE. NWU+3) GO TO 18
      IEREC=NBUF(3)+1
      PRINT 962,NUNIT,IEREC,NWRED,IWDS,NBUF(6)
  962 FORMAT(* RPTIN--BAD PHYSICAL LENGTH. UNIT, PHYSICAL RECORD, LENGTH
     *, EXPECTED LENGTH, STATUS*,5I8)
      GO TO 5
   18 CONTINUE
      NBUF(6)=0
      NBUF(3)=NBUF(3)+1
      NBUF(4)=NBUF(4)+NWRED
      NBUF(5)=LIN(KM)*(IWDS-1)
C
C GET LOGICAL RECORD
C
   40 JEOF=NBUF(6)
      CALL GBYTE(NBUF(7),KM,0,4)
      KM=KM+1
      IBITS=LIN(KM)*(NBUF(1)-7)
      CALL GBYTES(NBUF(7),NWDS,IBITS,12,0,1)
      IF(NWDS.GT.0.AND.(NWDS*LIN(KM)+IBITS).LE.NBUF(5)) GO TO 46
      IEREC=NBUF(2)+1
      PRINT 964,NUNIT,IEREC,NWDS
  964 FORMAT(* RPTIN--BAD LOGICAL LENGTH. UNIT, LOGICAL RECORD ,LENGTH*,
     *5I8)
      GO TO 3
   46 CONTINUE
      NCWDS=YF(KM)*NWDS+.99
      IF(NCWDS.GT.KLMAX) NCWDS=KLMAX
      IF(KM.NE.2) GO TO 50
      K=NBUF(1)
      DO 49 I=1,NCWDS
   49 LOCRPT(I)=NBUF(K+I-1)
      GO TO 51
   50 CONTINUE
      CALL GBYTES(NBUF(7),LOCRPT,IBITS,LOT(KM),0,NCWDS)
   51 CONTINUE
      NBUF(2)=NBUF(2)+1
      NBUF(1)=NBUF(1)+NWDS
      IBITS=IBITS+LIN(KM)*NWDS
      NWDS=NCWDS
      IF (IBITS .GE. NBUF(5)) GO TO 52
      RETURN
C
C PHYSICAL RECORD EXHAUSTED, INITIATE NEXT READ
C
   52 NBUF(1)=7
      CALL RDTAPE(NUNIT,1,2,NBUF(7),KMAX)
      RETURN
   42 CONTINUE
      NBUF(1)=0
      RETURN
   41 CONTINUE
      PRINT 965
  965 FORMAT(* RPTIN--ATTEMPT TO READ AFTER EOT, DATA MAY HAVE BEEN LOST
     **)
      RETURN
      END
        SUBROUTINE N76SUM(IBUF,N,NERR)
C
C COMPUTE 7600 CHECKSUM ON CRAY1
C
        DIMENSION IBUF(1),NBUF(512)
        NSUM=0
        NN=N-1
        CALL GBYTES(IBUF,NBUF,0,60,0,N)
        DO 10 I=1,NN
        NSUM=NSUM+NBUF(I)
    5   NOVER=SHIFTR(NSUM,60)
        NSIX=NSUM.AND.77777777777777777777B
        NSUM=NSIX+NOVER
        IF(NOVER.NE.0) GO TO 5
   10   CONTINUE
        NERR=NBUF(N)-NSUM
        RETURN
        END
        SUBROUTINE NCRYSM(IBUF,N,NERR)
C
C COMPUTE CRAY1 CHECKSUM ON CRAY1
C
        DIMENSION IBUF(1)
        NSUM=0
        NN=N-1
        DO 10 I=1,NN
        NSUM=NSUM+IBUF(I)
   10   CONTINUE
        NERR=IBUF(N)-NSUM
        RETURN
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
