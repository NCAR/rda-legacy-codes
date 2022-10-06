      program t2d
      parameter (lmax=32700)
      character nbf*(lmax),dev*64,flnm*64
      integer topen,tclose,tread,trewin,tskipf,tstate,twrite
      integer time
      logical label
      label=.FALSE.
c
      open(21,file='tapescan.out')
      iun=0
      nce=0
      nrt=0
      tnbyt=0
      nrlim=99999999
      write(*,*)' Enter device to read (/dev/rmt/0n)'
      read(*,'(a)')dev
      ix=index(dev,' ')-1
      ist=tclose(iun)
      jst=topen(iun,dev(1:ix),label)
c     kst=trewin(iun)
      if(jst.ne.0 .or. kst.ne.0) then
	print 1002,ist,jst,kst,dev(1:ix)
 1002   format(' Tape device not opened, status = ',3z9,1x,a)
	stop
      endif
      print 1003,ist,jst,kst,dev(1:ix)
 1003 format(' Tape device opened, status = ',3z9,1x,a)
c
      nf=0
    8 imx=0
      ist=tclose(iun)
      jst=topen(iun,dev(1:ix),label)
      imn=lmax
      nr=0
      tnby=0
      tby=0.
      nf=nf+1
      lst=time()
   10 ist=tread(iun,nbf)
      if(ist.le.0) go to 80
      if(nr.gt.nrlim) go to 80
      nce=0
      ncerr=0
      nr=nr+1
      tnby=tnby+ist
      tby=tby+ist
      if(ist.gt.imx) imx=ist
      if(ist.lt.imn) imn=ist
      if(mod(nr,1000).eq.1) then
	lct=time()-lst
	if(lct.gt.0) then
	  rate=tby/lct
	else
	  rate=0.
	endif
	write(*,1001)nf,nr,ist,tnby,rate
	write(21,1001)nf,nr,ist,tnby,rate
 1001   format(' Reading-file,rec,lth ',i5,2i12,e15.7,
     2    '   rate= ',f8.0)
	lst=time()
	tby=0.
      endif
      go to 10
   80 continue
      if(ist.eq.0 .or. nr.gt.nrlim) then
	kst=tskipf(iun,1,0)
	nce=nce+1
	write(*,1000)nf,nr,ist,imn,imx,tnby
	write(21,1000)nf,nr,ist,imn,imx,tnby
 1000   format(' file,recs = ',i4,i10,' status = ',i4,
     2   ' min,max,bytes ',2i6,e15.7)
	nrt=nrt+nr
	tnbyt=tnbyt+nby
      else
	write(*,1005)nf,nr,ist
	write(21,1005)nf,nr,ist
 1005   format('Read Error- file, record, status ',3i8)
	ncerr=ncerr+1
	if(ncerr.gt.50) then
	  write(*,*)' more than 50 consecutive errors, abort '
	  write(21,*)' more than 50 consecutive errors, abort '
	  stop
	endif
	go to 10
      endif
      if(nce.lt.2) go to 8
      write(*,1004)nf,nrt,tnbyt
      write(21,1004)nf,nrt,tnbyt
 1004 format(' exit, total files,recs, bytes ',i5,i10,e15.7)
      close(21)
      end
