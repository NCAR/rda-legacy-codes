      program t2t
      parameter (mxl=32700)
      character nbf*(mxl),dev*16,devo*16
      dimension nhdr(12)
      integer topen,tclose,tread,trewin,tskipf,tstate,twrite
      logical label
      label=.FALSE.
c
      iun=0
      jun=1
      nce=0
      nr=0
      xby=0
      nro=0
      open(7,file='tape2tape.out')
      write(*,*)' If you want to limit number of records copied,'
      write(*,*)'  enter number to copy, otherwise enter "0" '
      read(*,*)nrlim
      write(*,*)' Enter device to read (/dev/nrst8)'
      read(*,'(a)')dev
      ixi=index(dev,' ')-1
      ist=tclose(iun)
      jst=topen(iun,dev(1:ixi),label)
      kst=trewin(iun)
      if(jst.ne.0 .or. kst.ne.0) then
	print 1002,ist,jst,kst,dev(1:ixi)
 1002   format(' Tape device not opened, status = ',3z9,1x,a)
	stop
      endif
      print 1003,ist,jst,kst,dev(1:ixi)
 1003 format(' Tape device opened, status = ',3z9,1x,a)
      write(*,*)' Enter device to write'
      read(*,'(a)')devo
      ixo=index(devo,' ')-1
      ist=tclose(jun)
      nf=0
    8 jst=topen(jun,devo(1:ixo),label)
      if(jst.ne.0 .or. kst.ne.0) then
	print 1002,ist,jst,kst,devo(1:ixo)
	stop
      endif
      nrf=0
      nf=nf+1
c
      imx=0
      imn=mxl
   10 ist=tread(iun,nbf)
      if(ist.eq.0) go to 80
      if(ist.lt.0 .or. ist.ge.mxl) go to 75
      if(nrlim.gt.0 .and. nr.ge.nrlim) go to 80
      nce=0
      xby=xby+ist
      nr=nr+1
      nrf=nrf+1
      if(ist.gt.imx) imx=ist
      if(ist.lt.imn) imn=ist
      if(mod(nrf,1000).le.3) then
	write(7,1001)nr,ist
	write(*,1001)nr,ist
 1001   format(' Copying-rec,lth ',2i12)
      endif
      jkst=twrite(jun,nbf(1:ist))
      nro=nro+1
      if(jkst.gt.0) go to 10
   75 continue
      write(*,1006)ist,jkst
      write(7,1006)ist,jkst
 1006 format(' ERROR read status, write status ',2i8)
   80 continue
c when eof is encountered, tape is positioned before eof
c close and open input for byte limit escape
      kst=tskipf(iun,1,0)
      ist=tclose(iun)
      jst=topen(iun,dev(1:ixi),label)
c endfile to output unit
      lst=tclose(jun)
      xxby=xby/1000000.
      nce=nce+1
      write(7,1000)nf,nrf,nr,nro,xxby,imn,imx,ist,jst,kst,lst
      write(*,1000)nf,nrf,nr,nro,xxby,imn,imx,ist,jst,kst,lst
 1000 format(' file,recs,tot,out = ',i6,3i12,
     2 ' total MB=',f8.1,' mn,mx,ist,jst,kst,lst = ',6i6)
      if(jkst.lt.0) go to 90
      if(nrlim .gt. 0 .and. nr.gt.nrlim) go to 90
      if(nce.lt.5) go to 8
   90 close(7)
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
	subroutine swap4(in,io,nn)
c swaps bytes in groups of 4 to compensate for byte swapping within
c    words which occurs on DEC (VAX) and PC machines.
c
c in - input array to be swapped
c io - ouput array with bytes swapped
c nn - number of bytes to be swapped
	logical*1 in(1),io(1),ih
	do 10 i=1,nn,4
	ih=in(i)
	io(i)=in(i+3)
	io(i+3)=ih
	ih=in(i+1)
	io(i+1)=in(i+2)
	io(i+2)=ih
   10	continue
	return
	end
	subroutine swap2(in,io,nn)
c swaps bytes in groups of 2 to compensate for byte swapping within
c    words which occurs on DEC (VAX) and PC machines.
c
c in - input array to be swapped
c io - ouput array with bytes swapped
c nn - number of bytes to be swapped
	logical*1 in(1),io(1),ih
	do 10 i=1,nn,2
	ih=in(i)
	io(i)=in(i+1)
	io(i+1)=ih
   10	continue
	return
	end
	subroutine filt(m,n,in)
	logical*1 m(1),n(1),l,u
	data l/32/,u/127/
	do 10 i=1,in
	n(i)=m(i)
	if(n(i).lt.  l) n(i)=l  
	if(n(i).gt.  u) n(i)=u   
   10	continue
	return
	end
