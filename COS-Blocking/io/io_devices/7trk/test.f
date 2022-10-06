      program read
      parameter (maxl=16384,maxb=(maxl+4)/4)
      character nbf*(maxl),acom*32
      integer tskipb
      dimension nbfb(maxb),icom(32)
      equivalence (nbf,nbfb)
      llth=0
      numt=0
      mnumt=1
      ipass=0
      nce=0
    5 iun=0
      nrt=0
      ipass=ipass+1
      write(*,1006)ipass
      write(26,1006)ipass
 1006 format(' start pass number ',i5)
      iatt=0
    7 call open7(iun,acom,icom,'m','o',nbf,maxl,6,istat)
      if(istat.gt.1) go to 90
      write(*,*)' opened '
      write(*,*)' testing skip file '
      nrt=0
       nrt=nrt+1
       call skipf7(iun,acom,icom,istat)
       write(*,*)nrt,istat
      call status7(iun,acom,icom,1,ifile,irec,listat)
c     write(*,*)' skipped '
c     write(*,*)istat
      stop 8
    8 nr=0
      ncer=0
      iatt=iatt+1
      write(*,1004)ipass,iatt
 1004 format(' starting pass ',i5,'  attempt ',i3)
      if(iatt.gt.2) stop 8
      xby=0
      nf=1
      lmax=0
      lmin=maxl


   10 continue
      do i=1,maxb
       nbfb(i)=0
      enddo
      call read7(iun,acom,icom,nbf,maxl,lth,istat)
      if(istat.eq.1) go to 90
      nr=nr+1
c
c      if(lth.ne.2749) go to 2002
c      nbf(lth+1:)=char(0)
      icsm=0
      izer=0
      do i=1,lth
	inch=ichar(nbf(i:i))
	icsm=xor(icsm,inch)
	if(inch.eq.0) izer=izer+1
c       if(icsm.eq.0) then
c       write(*,'(2i6,1x,2z9)')nr,i,icsm,ichar(nbf(i:i))
c       endif
      enddo
c     nwds=(lth+3.1)/4.
c     if (lth.eq.2749) write(*,'(10z9)')(nbfb(i),i=1,nwds)
c     write(*,2001)nr,lth,nwds,(nbfb(i),i=nwds-1,nwds+2),icsm
c2001 format(1x,3i5,2x,5z9)
c2002 continue
      xby=xby+lth
c test backspace
c     if(nr.eq.40) then
c       call status7(iun,acom,icom,1,ifile,irec,kstat)
c       call bspace7(iun,acom,icom,kstat)
c       call status7(iun,acom,icom,1,ifile,irec,kstat)
c     write(*,*)' pause '
c     iii = system('sleep 5')
c     write(*,*)' start '
c     endif
c
      if(istat.gt.1) then
	write(26,1001)nf,nr,lth,istat
	write(*,1001)nf,nr,lth,istat
 1001   format(' read error - file, rec, lth, stat = ',4i6)
	write(26,'(5x,8z9)')(nbfb(i),i=1,8)
	write(*,'(5x,8z9)')(nbfb(i),i=1,8)
	if(istat.eq.3) stop 10
	ncer=ncer+1
	if(ncer.gt.20) go to 7
c treat error after eof as eof
	if(nr.eq.1) go to 90
      else
	ncer=0
      endif
c
      nce=0
      if(lth.gt.lmax) lmax=lth
      if(lth.lt.lmin) lmin=lth
c
c     if(mod(nr,10) .lt.2) then
      if(lth .ne. llth .or. icsm.ne.0) then
	nl=(lth+3)/4
	write(26,1002)nf,nr,lth,istat,izer,icsm,
     2 (nbfb(i),i=1,2),(nbfb(i),i=nl-1,nl)
	write(*,1002)nf,nr,lth,istat,izer,icsm,
     2 (nbfb(i),i=1,2),(nbfb(i),i=nl-1,nl)
 1002   format(1x,i4,2i6,i3,i5,1x,z4,2x,2z9,' - ',2z9)
      endif
      llth=lth
      go to 10
   90 continue
      nce=nce+1
      nrt=nrt+nr
      write(26,1003)nf,nrt,nr,xby,lmin,lmax
      write(*,1003)nf,nrt,nr,xby,lmin,lmax
 1003 format(' EOF - file,rect,recs, bytes, minlth, maxlth ',
     2  i5,i6,i8,f12.0,2i8)
      nf=nf+1
      nr=0
      nr=0
      xby=0.
      lmax=0
      lmin=maxl
      if(nce.lt.2) go to 10
      call status7(iun,acom,icom,1,ifile,irec,listat)
      call rewind7(iun,acom,icom,istat)
      numt=numt+1
      if(numt.lt.mnumt) go to 5
c     call offline7(iun,acom,icom,istat)
      end

~ iosubs.f
