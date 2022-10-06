	subroutine swap4(in,io,nn)
c swaps bytes in groups of 4 to compensate for byte swapping within
c    words which occurs on DEC (VAX) and PC machines.
c
c in - input array to be swapped
c io - ouput array with bytes swapped
c nn - number of bytes to be swapped
	character*1 in(1),io(1),ih
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
	character*1 in(1),io(1),ih
	do 10 i=1,nn,2
	ih=in(i)
	io(i)=in(i+1)
	io(i+1)=ih
   10	continue
	return
	end
	subroutine filt(m,n,in)
	character*1 m(1),n(1),l,u
	data l/32/,u/127/
	do 10 i=1,in
	n(i)=m(i)
	if(n(i).lt.  l) n(i)=l  
	if(n(i).gt.  u) n(i)=u   
   10	continue
	return
	end
