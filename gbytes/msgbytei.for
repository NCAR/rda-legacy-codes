      interface to subroutine ubytes[c]
     +(p[reference],u[reference],q[reference],b[reference],
     + s[reference],n[reference])
      integer*4 p,u,q,b,s,n
      end
      subroutine gbytes(p,u,q,b,s,n)
      integer*4 p,u,q,b,s,n
      call ubytes(p,u,q,b,s,n)
      return
      end
      subroutine gbyte(p,u,q,b)
      integer*4 p,u,q,b
      call gbytes(p,u,q,b,0,1)
      return
      end
      interface to subroutine pbytes[c]
     +(p[reference],u[reference],q[reference],b[reference],
     + s[reference],n[reference])
      integer*4 p,u,q,b,s,n
      end
      subroutine sbytes(p,u,q,b,s,n)
      integer*4 p,u,q,b,s,n
      call pbytes(p,u,q,b,s,n)
      return
      end
      subroutine sbyte(p,u,q,b)
      integer*4 p,u,q,b
      call sbytes(p,u,q,b,0,1)
      return
      end
      subroutine swap4(in,out,n)
      logical*1 in(n),out(n),ih
      do 10 i=1,n,4
      ih=in(i)
      out(i)=in(i+3)
      out(i+3)=ih
      ih=in(i+1)
      out(i+1)=in(i+2)
      out(i+2)=ih
   10 continue
      return
      end
      subroutine swap2(in,out,n)
      logical*1 in(n),out(n),ih
      do 10 i=1,n,2
      ih=in(i)
      out(i)=in(i+1)
      out(i+1)=ih
   10 continue
      return
      end



