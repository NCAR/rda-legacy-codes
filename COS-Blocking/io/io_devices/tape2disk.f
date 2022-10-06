      program t2d
      parameter (ndim=32768)
      character nbf*(ndim),dev*16,flnm*64,flnmx*64
      integer topen,tclose,tread,trewin,tskipf,tstate,twrite
      logical label
      label=.FALSE.
c
      open(7,file='tape2disk.out')
      iun=0
      nce=0
      nr=0
      nrt=0
      nbt=0
      write(*,*)' Enter 0 for binary copy  or 1 for ASCII (linefeed) '
      read(*,*)lf
c
      write(*,*)' Enter number of files to copy '
      read(*,*)nrlim
      if(nrlim.le.0) nrlim=99999
c     write(*,*)' New UNIX file for each input file? (0=no,1=yes) '
c     read(*,*)multi
      multi=1
      nf=0
c
      write(*,*)' Enter device to read (/dev/nrst0) '
      read(*,'(a)')dev
      iy=index(dev,' ')-1
      ist=tclose(iun)
      jst=topen(iun,dev(1:iy),label)
      if(jst.ne.0 .or. kst.ne.0) then
	print 1002,ist,jst,kst,dev(1:iy)
 1002   format(' Tape device not opened, status = ',3z9,1x,a)
	stop
      endif
      print 1003,ist,jst,kst,dev(1:ix)
 1003 format(' Tape device opened, status = ',3z9,1x,a)
      write(*,*)' Enter output file name '
      write(*,*)'  (a suffix will be added to create unique names) '
      read(*,'(a)')flnm
      ix=index(flnm,' ')
      nfx=nf+10000
      write(flnmx(ix:ix+4),'(i5)')nfx
      flnmx(1:ix-1)=flnm(1:ix-1)
      flnmx(ix:ix)='.'
      call sopen(21,flnmx)
      nerr=0
      ncerr=0
    9 nr=0
      lmax=0
      lmin=ndim
      write(*,*)' Displays info from first recs in each file'
      write(*,*)'  and copies to '//flnmx
   10 continue
      ist=tread(iun,nbf)
      if(ist.gt.0) ncerr=0
      if(ist.eq.0) go to 80
c special termination
c     if(ist.lt. 40) go to 94
c
      if(ist.lt.0) then
	write(*,1004)nr,ist
	write(7,1004)nr,ist
 1004   format(' read error- nr,ist ',i8,i4)
	nerr=nerr+1
	ncerr=ncerr+1
	if(ncerr.gt.2) then
	  istat=tskipf(iun,0,1)
	  write(*,1005)nr,nerr
	  write(7,1005)nr,nerr
 1005     format(' record skip nr,nerr,ncerr ',3i6)
	endif
	if(nerr.gt.30) go to 95
	ist=-ist
      endif
      nce=0
      if(ist.gt.lmax) lmax=ist
      if(ist.lt.lmin) lmin=ist
      nr=nr+1
      nrt=nrt+1
      nbt=nbt+ist
      ltho=(ist+3.1)/4.
      nub=32*ltho - 8*ist
c     nub=0
      if(lf.eq.1) then
	ist=ist+1
	nbf(ist:ist)=char(10)
      endif
      call swrite(21,nbf,ist,ilth,istat)
      if(mod(nr,1000).lt.10) then
	write(*,1001)nf,nr,ist,istat
	write(7,1001)nf,nr,ist,istat
 1001   format(' Copying-file,rec,lth,stat ',i6,3i12)
      endif
      go to 10
   80 continue
      nf=nf+1
      if(multi.eq.1) then
	call sclose(21)
	nfx=nf+10000
	write(flnmx(ix:ix+4),'(i5)')nfx
	flnmx(1:ix-1)=flnm(1:ix-1)
	flnmx(ix:ix)='.'
	call sopen(21,flnmx)
      endif
   78 continue
      ist=tclose(iun)
      jst=topen(iun,dev(1:iy),label)
      if(jst.ne.0 .or. kst.ne.0) then
	print 1002,ist,jst,kst,dev(1:ix)
	stop
      endif
      nce=nce+1
      write(*,1000)nf,nr,ist,lmin,lmax,nrt,nbt
      write(7,1000)nf,nr,ist,lmin,lmax,nrt,nbt
 1000 format(' file,rec,status,min,max',i5,i6,i4,2i8,i8,i15)
c test for number of consec empty files
      if(nce.le.25 .and. nf.lt.nrlim)  go to 9
      call sclose(21)
      go to 99
   94 continue
      write(*,1007)ist
      write(7,1007)ist
 1007 format(' terminated on short record, lth = ',i5)
   95 continue
      write(*,1006)nr,ist,flnmx(1:ix+5)
      write(7,1006)nr,ist,flnmx(1:ix+5)
 1006 format(' error exit  recs = ',i12,' end status = ',i8,
     2 ' file',1x,a)
      call sclose(21)
   99 continue
      close(7)
      end
      SUBROUTINE GBYTE (IN,IOUT,ISKIP,NBYTE)
      CALL GBYTES (IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END

      SUBROUTINE SBYTE (IOUT,IN,ISKIP,NBYTE)
      CALL SBYTES (IOUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END

      SUBROUTINE GBYTES (IN,IOUT,ISKIP,NBYTE,NSKIP,N)
C          Get bytes - unpack bits:  Extract arbitrary size values from a
C          packed bit string, right justifying each value in the unpacked
C          array.
      DIMENSION IN(*), IOUT(*)
C            IN    = packed array input
C            IO    = unpacked array output
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to take
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C************************************** MACHINE SPECIFIC CHANGES START HERE
C          Machine dependent information required:
C            LMWD   = Number of bits in a word on this machine
C            MASKS  = Set of word masks where the first element has only the
C                     right most bit set to 1, the second has the two, ...
C            LEFTSH = Shift left bits in word M to the by N bits
C            RGHTSH = Shift right
C            OR     = Logical OR (add) on this machine.
C            AND    = Logical AND (multiply) on this machine
C          This is for Sun UNIX Fortran, DEC Alpha, and RS6000
      PARAMETER (LMWD=32)
      DIMENSION MASKS(LMWD)
      SAVE      MASKS
      DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
C    +'1FFFFFFFF'X,   '3FFFFFFFF'X,   '7FFFFFFFF'X,   'FFFFFFFFF'X,
C    +'1FFFFFFFFF'X,  '3FFFFFFFFF'X,  '7FFFFFFFFF'X,  'FFFFFFFFFF'X,
C    +'1FFFFFFFFFF'X, '3FFFFFFFFFF'X, '7FFFFFFFFFF'X, 'FFFFFFFFFFF'X,
C    +'1FFFFFFFFFFF'X,'3FFFFFFFFFFF'X,'7FFFFFFFFFFF'X,'FFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFF'X,   '3FFFFFFFFFFFF'X,   '7FFFFFFFFFFFF'X,
C    +                                        'FFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFF'X,  '3FFFFFFFFFFFFF'X,  '7FFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFFF'X, '3FFFFFFFFFFFFFF'X, '7FFFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFFF'X,
C    +'1FFFFFFFFFFFFFFF'X,'3FFFFFFFFFFFFFFF'X,'7FFFFFFFFFFFFFFF'X,
C                                             'FFFFFFFFFFFFFFFF'X/
C          IBM PC using Microsoft Fortran uses different syntax:
C     DATA MASKS/16#1,16#3,16#7,16#F,16#1F,16#3F,16#7F,16#FF,
C    + 16#1FF,16#3FF,16#7FF,16#FFF,16#1FFF,16#3FFF,16#7FFF,16#FFFF,
C    + 16#1FFFF,16#3FFFF,16#7FFFF,16#FFFFF,16#1FFFFF,16#3FFFFF,
C    + 16#7FFFFF,16#FFFFFF,16#1FFFFFF,16#3FFFFFF,16#7FFFFFF,16#FFFFFFF,
C    + 16#1FFFFFFF,16#3FFFFFFF,16#7FFFFFFF,16#FFFFFFFF/
      INTEGER RGHTSH, OR, AND
      LEFTSH(M,N) = ISHFT(M,N)
      RGHTSH(M,N) = ISHFT(M,-N)
C     OR(M,N)  = M.OR.N
C     AND(M,N) = M.AND.N
C     OR(M,N)  = IOR(M,N)
C     AND(M,N) = IAND(M,N)
C************************************** MACHINE SPECIFIC CHANGES END HERE
C          History:  written by Robert C. Gammill, jul 1972.


C          NBYTE must be less than or equal to LMWD
      ICON = LMWD-NBYTE
      IF (ICON.LT.0) RETURN
      MASK = MASKS (NBYTE)
C          INDEX  = number of words into IN before the next "byte" appears
C          II     = number of bits the "byte" is from the left side of the word
C          ISTEP  = number of bits from the start of one "byte" to the next
C          IWORDS = number of words to skip from one "byte" to the next
C          IBITS  = number of bits to skip after skipping IWORDS
C          MOVER  = number of bits to the right, a byte must be moved to be
C                   right adjusted
      INDEX = ISKIP/LMWD
      II    = MOD (ISKIP,LMWD)
      ISTEP = NBYTE+NSKIP
      IWORDS= ISTEP/LMWD
      IBITS = MOD (ISTEP,LMWD)

      DO 6 I=1,N                                                                
      MOVER = ICON-II
      IF (MOVER) 2,3,4

C          The "byte" is split across a word break.
    2 MOVEL = -MOVER
      MOVER = LMWD-MOVEL
      NP1 = LEFTSH (IN(INDEX+1),MOVEL)
      NP2 = RGHTSH (IN(INDEX+2),MOVER)
      IOUT(I) = AND (OR (NP1,NP2) , MASK)
      GO TO 5                                                                   

C          The "byte" is already right adjusted.
    3 IOUT(I) = AND (IN (INDEX+1) , MASK)
      GO TO 5                                                                   

C          Right adjust the "byte".
    4 IOUT(I) = AND (RGHTSH (IN (INDEX+1),MOVER) , MASK)

    5 II = II+IBITS
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
    6 CONTINUE                                                                  

      RETURN                                                                    
      END                                                                       

      SUBROUTINE SBYTES (IOUT,IN,ISKIP,NBYTE,NSKIP,N)
C          Store bytes - pack bits:  Put arbitrary size values into a
C          packed bit string, taking the low order bits from each value
C          in the unpacked array.
      DIMENSION IN(*), IOUT(*)
C            IOUT  = packed array output
C            IN    = unpacked array input
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to pack
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C************************************** MACHINE SPECIFIC CHANGES START HERE
C          Machine dependent information required:
C            LMWD   = Number of bits in a word on this machine
C            MASKS  = Set of word masks where the first element has only the
C                     right most bit set to 1, the second has the two, ...
C            LEFTSH = Shift left bits in word M to the by N bits
C            RGHTSH = Shift right
C            OR     = Logical OR (add) on this machine
C            AND    = Logical AND (multiply) on this machine
C            NOT    = Logical NOT (negation) on this machine
C          This is for Sun UNIX Fortran
      PARAMETER (LMWD=32)
      DIMENSION MASKS(LMWD)
      SAVE      MASKS
      DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
      INTEGER RGHTSH, OR, AND
      LEFTSH(M,N) = ISHFT(M,N)
      RGHTSH(M,N) = ISHFT(M,-N)
C     OR(M,N)  = M.OR.N
C     AND(M,N) = M.AND.N
C     OR(M,N)  = IOR(M,N)
C     AND(M,N) = IAND(M,N)
C     NOT(M)   = .NOT.M
C***********************************************************************        

C          NBYTE must be less than or equal to LMWD
      ICON = LMWD-NBYTE
      IF (ICON .LT. 0) RETURN
      MASK = MASKS(NBYTE)
C          INDEX  = number of words into IOUT the next "byte" is to be stored
C          II     = number of bits in from the left side of the word to store it
C          ISTEP  = number of bits from the start of one "byte" to the next
C          IWORDS = number of words to skip from one "byte" to the next
C          IBITS  = number of bits to skip after skipping IWORDS
C          MOVER  = number of bits to the right, a byte must be moved to be
C                   right adjusted
      INDEX = ISKIP/LMWD
      II    = MOD(ISKIP,LMWD)
      ISTEP = NBYTE+NSKIP
      IWORDS = ISTEP/LMWD
      IBITS = MOD(ISTEP,LMWD)

      DO 6 I=1,N                                                                
      J = AND (MASK,IN(I))
      MOVEL = ICON-II
      IF (MOVEL) 2,3,4

C          The "byte" is to be split across a word break
    2 MSK = MASKS (NBYTE+MOVEL)
      IOUT(INDEX+1) = OR (AND(NOT(MSK),IOUT(INDEX+1)),RGHTSH(J,-MOVEL))
      ITEMP = AND (MASKS(LMWD+MOVEL),IOUT(INDEX+2))
      IOUT(INDEX+2) = OR(ITEMP,LEFTSH(J,LMWD+MOVEL))
      GO TO 5                                                                   

C          The "byte" is to be stored right-adjusted
    3 IOUT(INDEX+1) = OR ( AND (NOT(MASK),IOUT(INDEX+1)) , J)
      GO TO 5                                                                   

C          The "byte" is to be stored in middle of word, so shift left.
    4 MSK = LEFTSH(MASK,MOVEL)
      IOUT(INDEX+1) = OR(AND(NOT(MSK),IOUT(INDEX+1)),LEFTSH(J,MOVEL))

    5 II = II+IBITS
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
    6 CONTINUE

      RETURN                                                                    
      END                                                                       
      subroutine sread(iun,nb,lth,ilth,ist)
c
c simple stream i/o based on fortran extensions fgetc
c  and fputc.
c
c read "lth" bytes in "nb" from unit "iun"
c "ilth" is actual bytes read which may be less
c  than "lth" if you attempt to read post the end
c  of file.
c "ist" should be zero, -1 = eof, other = error.
      character nb*(*)
      integer fgetc
      ilth=0
      do 20 l=1,lth
      ist=fgetc(iun,nb(l:l))
      if(ist.ne.0) go to 30
      ilth=l
   20 continue
   30 return
      end
      subroutine swrite(iun,nb,lth,ilth,ist)
c write "lth" bytes from "nb" to unit "iun"
c "ilth" is actual bytes written and should =
c  "lth".
c "ist" should be zero, other = error.
      character nb*(*)
      integer fputc
      ilth=0
      do 20 l=1,lth
      ist=fputc(iun,nb(l:l))
      if(ist.ne.0) go to 30
      ilth=l
   20 continue
   30 return
      end
      subroutine sopen(iun,name)
c open logical unit "iun" for file "name"
      character name*(*)
      open(iun,file=name,access='sequential')
      return
      end
      subroutine sclose(iun)
      close(iun)
      return
      end
