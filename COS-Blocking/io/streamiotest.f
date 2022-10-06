      program stream
      character nbf*2048
      do 2 i=1,2048
      nbf(i:i)=char(mod(i,256))
    2 continue
      iun=11
      lth=100
      call sopen(iun,'test_stream')
      do 10 i=1,100
      nbf(1:1)=char(i)
      call swrite(iun,nbf,lth,ilth,ist)
   10 continue
      call sclose(iun)
      call sopen(iun,'test_stream')
      nr=0
      lth=200
   12 continue
      nr=nr+1
      call sread(iun,nbf,lth,ilth,ist)
      write(*,1001)nr,ichar(nbf(1:1)),ichar(nbf(lth:lth)),ilth,ist
 1001 format(5i6)
      if(ist.eq.0) go to 12
      end
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
