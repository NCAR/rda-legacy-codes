      subroutine open7(iun,acom,icom,aden,apar,nbf,maxl,ie,istat)
      save
      dimension icom(*)
      integer topen,system,tread
      character aden*1,apar*1,acom*(*),nbf*(*)
      character dev*32,den*1,par*1
      logical label/.false./
      acom(32:32)=char(ie)
c set density flag
      if(aden.ne.'h' .and. aden.ne.'l') then
	den='m'
      else
	den=aden
      endif
      par=apar
      istat=0
c build device name
      dev='/dev/rmt/4'//den//'bn '
      ix=index(dev,' ')-1
c     ist=topen(iun,dev(1:ix),label)
c     jst=tread(iun,nbf(1:maxl))
c     call rewind7(iun,acom,icom,jstat)
c     call status7(iun,acom,icom,0,ifile,irec,kst)
      do i=1,2
	ist=topen(iun,dev(1:ix),label)
	jst=tread(iun,nbf(1:maxl))
	call rewind7(iun,acom,icom,jstat)
	call status7(iun,acom,icom,0,ifile,irec,kst)
	icom(1)=0
	icom(2)=0
	if(ist.eq.0 .and. kst.eq.0) go to 5
c       if(ist.eq.0 .and.jst.ge.0 .and. kst.eq.0) go to 5
	isst=system('sleep 1 ')
	write(ie,1000)ist,jst,kst
 1000   format(' 7trk waiting for drive ready-ist,jst,kst ',3i8)
      enddo
      if(ist.ne.0 .or. kst.ne.0) then
	write(ie,1001)ist,kst,dev(1:ix)
 1001   format(' 7trk open failed, status = ',2z9,1x,a)
	acom=' '
	istat=2
	return
      endif
      istat=1
    5 continue
      acom(1:16)=dev(1:16)
      return
      end

      subroutine read7(iun,acom,icom,nbf,maxl,lth,istat)
      save
      dimension icom(*)
      integer tskipf,system,tclose,topen,tread
      character nbf*(*),acom*(*)
      logical label/.false./
      istat=0
      ie=ichar(acom(32:32))
      if(acom.eq.' ') then
	write(ie,1001)
 1001   format(' 7trk read failed, device not open ')
	istat=9
	return
      endif
      if(maxl.lt.1) then
	write(ie,1003)maxl
 1003   format(' 7trk read failed, bad max length ',i5)
	istat=9
	return
      endif
      nretry=0
   10 continue
      do i = 1,maxl
	nbf(i:i)=char(0)
      enddo
      ist=tread(iun,nbf(1:maxl))
c test longitudinal parity
      icsm=0
      do i=1,ist
	icsm=xor(icsm,ichar(nbf(i:i)))
      enddo
      if(ist.eq.0) then
c     EOF       eof
	icom(1)=icom(1)+1
	icom(3)=icom(2)
	icom(2)=0
	istat=1
	lth=0
	if(icom(1).eq.1 .and. icom(3).eq.0) then
	  write(ie,1013)icom(1),icom(2),icom(3)
 1013     format(' 7trk - eof at bot ; icom ',3i8)
	  call status7(iun,acom,icom,1,ifile,irec,jstat)
	  call rewind7(iun,acom,icom,ist)
	  call skipf7(iun,acom,icom,kst)
	  write(ie,1012)kst
 1012     format(' 7trk - eof at bot; kst ',i5)
	  call status7(iun,acom,icom,1,ifile,irec,jstat)
	else
	  ist = tskipf(iun,1,0)
	endif
	if(kst.ne.0) then
	  write(ie,1004)kst
 1004     format(' 7trk skip eof failed ',i5)
	  call status7(iun,acom,icom,1,ifile,irec,jstat)
	  istat=1
	  return
	endif
      elseif(ist.lt.0 .or.icsm.ne.0) then
	icom(2)=icom(2)+1
c               error
	istat=2
	write(ie,1002)ist,icsm,nretry
 1002   format(' 7trk read error, ist, cksum, retries = ',1x,i6,z9,i6)
	call status7(iun,acom,icom,1,ifile,irec,jsstat)
	if(nretry.gt.0) then
	  call bspace7(iun,acom,icom,kstat)
	  call status7(iun,acom,icom,0,ifile,irec,jstat)
	  nretry=nretry-1
	  go to 10
	endif
c if unrecovered error with bad status try to find end of record
	if(ist.lt.0) then
	  do i=maxl,1,-1
	    ii=i
	    if(ichar(nbf(i:i)).ne.0) go to 20
	  enddo
   20     continue
c drive encounter 0 lth record
c  to recover without drive hangup, a rewind is necessary
c    then attempt to reposition past zero length record
	  if(ii.le.2) then
	    ifile=icom(1)
	    irec=icom(2)
	    write(ie,1011)ifile,irec
 1011       format(' 7trk - start skip recover;file,rec ',2i8)
	    call rewind7(iun,acom,icom,istat)
	    call status7(iun,acom,icom,1,kfile,krec,kstat)
	    iskipf=0
  24        continue
	    if(iskipf.ge.ifile) go to 26
	      call skipf7(iun,acom,icom,istat)
	      write(ie,1007)iskipf,istat
 1007         format(' 7trk - recovery file skip;skip,stat ',2i8)
	      if(istat.eq.0) iskipf=iskipf+1
	    go to 24
   26       continue
	    write(ie,1008)irec
 1008       format(' 7trk - recovery record skip;irec ',i8)
c
	    iskipr=0
	    iskipe=0
   30       continue
	      call skipr7(iun,acom,icom,jstat)
	      if(jstat.ne.0) then
		isstat=system('sleep 1')
		write(ie,1009)jstat
 1009           format(' 7trk - error in skip; status ',i6)
		call status7(iun,acom,icom,1,kfile,krec,kstat)
		kst=tclose(iun)
		ix=index(acom,' ')-1
		ist=topen(iun,acom(1:ix),label)
		write(ie,*)' 7trk - skip reopen stat  '
		call status7(iun,acom,icom,1,kfile,krec,kstat)
		isstat=system('sleep 1')
		iskipe=iskipe+1
		if(iskipe.gt.9) then
		   call skipf7(iun,acom,icom,kst)
		   istat=1
		   ii=0
		   return
		endif
		iskipr=iskipr+1
	      else
		iskipe=0
		iskipr=iskipr+1
	      endif
	    if(iskipr.lt.irec) go to 30
c
	    write(ie,1006)
 1006       format(' 7trk - status after skip ',i6)
	    call status7(iun,acom,icom,1,ifile,irec,jstat)
	    istat=3
	  else
	    istat=4
	  endif
c end of section to recover from 0 length record
	  lth=ii
	  icsm=0
	  do i=1,ii
	    icsm=xor(icsm,ichar(nbf(i:i)))
	  enddo
	  write(ie,1005)ii,icsm
 1005     format(' 7trk read error recover, lth, cksum = ',1x,i6,z9,i6)
	endif
      else
c good read
	icom(2)=icom(2)+1
	istat=0
	lth=ist
      endif
      return
      end

      subroutine rewind7(iun,acom,icom,istat)
      save
      dimension icom(*)
      integer trewin,system
      character acom*(*)
      ie=ichar(acom(32:32))
      if(acom.eq.' ') then
	write(ie,*)' 7trk rewind failed, device not open '
	istat=9
	return
      endif
      ist=trewin(iun)
      ikst=system('sleep  1 ')
      call status7(iun,acom,icom,0,ifile,irec,jstat)
      ist=trewin(iun)
      ikst=system('sleep  1 ')
      call status7(iun,acom,icom,0,ifile,irec,jstat)
      if(ist.ne.0) then
	write(ie,1001)ist
 1001   format(' 7trk rewind failed, status - ',z9)
	istat=2
	call status7(iun,acom,icom,1,ifile,irec,jstat)
      else
	icom(1)=0
	icom(2)=0
	istat=0
      endif
      return
      end

      subroutine status7(iun,acom,icom,iprt,ifile,irecno,istat)
      save
      dimension icom(*)
      integer tstate
      character acom*(*)
      logical lerrf,leoff,leotf
      ie=ichar(acom(32:32))
      ist=tstate(iun,ifile,irecno,lerrf,leoff,leotf,itcsr)
      if(iprt.ne.0) then
       write(ie,*)'7trk status return ------------------'
       write(ie,'(" 7trk status, ist   = ",z10)')ist
       write(ie,'(" 7trk status, file  = ",i10)')ifile
       write(ie,'(" 7trk status, recno = ",i10)')irecno
       write(ie,'(" 7trk status, err   = ",z10)')lerrf
       write(ie,'(" 7trk status, eof   = ",z10)')leoff
       write(ie,'(" 7trk status, eot   = ",z10)')leotf
       write(ie,'(" 7trk status, status= ",z10)')itcsr
      endif
c     istat=ist
      istat=itcsr
      return
      end

      subroutine bspace7(iun,acom,icom,istat)
      save
      dimension icom(*)
      integer system,tclose,topen
      character acom*(*)
      logical label/.false./
      ie=ichar(acom(32:32))
      call status7(iun,acom,icom,0,ifile,irec,istat)
      if(irec.gt.0) then
	ix=index(acom,' ')-1
	kst=tclose(iun)
	ijst=system('mt -f '//acom(1:ix)//' bsr 1 ')
	ist=topen(iun,acom(1:ix),label)
	istat=0
	icom(2)=icom(2)-1
      else
	istat=2
      endif
      return
      end

      subroutine skipf7(iun,acom,icom,istat)
      save
      dimension icom(*)
      integer tskipf,system,tclose,topen
      character acom*(*)
      logical label/.false./
      ie=ichar(acom(32:32))
      ix=index(acom,' ')-1
      write(ie,'(" 7trk - start file skip  ")')
      call status7(iun,acom,icom,1,ifile,irec,jstat)
      nfail=0
      kst=tclose(iun)
c
c file skips more reliable using mt rather than fortran.
c loop until good file skip status
   10 continue
      nstat=system('mt -f '//acom(1:ix)//' fsf ')
      if(nstat.eq.0) then
	icom(1)=icom(1)+1
	icom(2)=0
	icom(3)=-1
	istat=0
	ist=topen(iun,acom(1:ix),label)
	return
      endif
      write(ie,'(" 7trk - file skip; nstat",2i8)')nstat
      nfail=nfail+1
c on certain types of tape noise, record skip works better
      mstat=system('mt -f '//acom(1:ix)//' fsr ')
      write(ie,'(" 7trk - file/rec skip; mstat",2i8)')mstat
      if(mstat.eq.0 .or. nfail.lt.19) go to 10
c
      nstat=system('mt -f '//acom(1:ix)//' fsf ')
      if(nstat.eq.0) then
	icom(1)=icom(1)+1
	icom(2)=0
	icom(3)=-1
	istat=0
      else
	write(ie,'(" 7trk - file skip failed;nstat,nfail",2i8)')
     2   nstat,nfail
	istat=9
	icom(3)=-1
      endif
      ist=topen(iun,acom(1:ix),label)
      return
      end

      subroutine skipr7(iun,acom,icom,istat)
      save
      dimension icom(*)
      integer tskipf
      istat=tskipf(iun,0,1)
      icom(2)=icom(2)+1
      return
      end

