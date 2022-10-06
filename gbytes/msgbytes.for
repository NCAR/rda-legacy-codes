      SUBROUTINE GBYTE(IN,IOUT,ISKIP,NBYTE)
      CALL GBYTES(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END
      SUBROUTINE SBYTE(IOUT,IN,ISKIP,NBYTE)
      CALL SBYTES(IOUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END
      SUBROUTINE GBYTES(IN,IOUT,ISKIP,NBYTE,NSKIP,N)                            
C THIS PROGRAM WRITTEN BY.....                                                  
C             DR. ROBERT C. GAMMILL, CONSULTANT                                 
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH                          
C             MAY 1972                                                          
C THIS IS THE FORTRAN VERSION OF GBYTES.                                        
      COMMON/MACHIN/NBITSW,NBITSC,MASK0,MASKS(32)                               
      DIMENSION IN(1),IOUT(1)                                                   
C  THE STATEMENTS BETWEEN ASTERISK LINES GIVE ALL NECESSARY MACHINE             
C  DEPENDENT INFORMATION                                                        
C***********************************************************************        
C THIS SPECIFICATION IS FOR IBM PC USING MICROSOFT FORTRAN
      INTEGER RGHTSH,OR,AND                                                     
      LEFTSH(M,N)=ISHFT(M,N)                                                      
      RGHTSH(M,N)=ISHFT(M,-N)                                                      
      OR(M,N)=IOR(M,N)                                                            
      AND(M,N)=IAND(M,N)
      DATA NBITSW/32/
      DATA MASK0/16#0/
      DATA MASKS/16#1,16#3,16#7,16#F,16#1F,16#3F,16#7F,16#FF,
     2 16#1FF,16#3FF,16#7FF,16#FFF,16#1FFF,16#3FFF,16#7FFF,16#FFFF,
     3 16#1FFFF,16#3FFFF,16#7FFFF,16#FFFFF,16#1FFFFF,16#3FFFFF,
     4 16#7FFFFF,16#FFFFFF,16#1FFFFFF,16#3FFFFFF,16#7FFFFFF,16#FFFFFFF,
     5 16#1FFFFFFF,16#3FFFFFFF,16#7FFFFFFF,16#FFFFFFFF/
C***********************************************************************        
C NBYTE MUST BE LESS THAN OR EQUAL TO NBITSW                                    
      ICON=NBITSW-NBYTE                                                         
      IF(ICON.LT.0) RETURN                                                      
      MASK=MASKS(NBYTE)                                                         
C INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IN' THE NEXT BYTE APPEARS.         
      INDEX=ISKIP/NBITSW                                                        
C II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.            
      II=MOD(ISKIP,NBITSW)                                                      
C ISTEP IS THE DISTANCE IN BITS FROM THE START OF ONE BYTE TO THE NEXT.         
      ISTEP=NBYTE+NSKIP                                                         
C IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.                
      IWORDS=ISTEP/NBITSW                                                       
C IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.                      
      IBITS=MOD(ISTEP,NBITSW)                                                   
      DO 6 I=1,N                                                                
C MOVER SPECIFIES HOW FAR TO THE RIGHT A BYTE MUST BE MOVED IN ORDER            
C    TO BE RIGHT ADJUSTED.                                                      
      MOVER=ICON-II                                                             
      IF(MOVER) 2,3,4                                                           
C                                                                               
C THE BYTE IS SPLIT ACROSS A WORD BREAK.                                        
    2 MOVEL=-MOVER                                                              
      MOVER=NBITSW-MOVEL
C EXPAND STATEMENT SINCE MSFORTRAN SEEMS TO FAIL OTHERWISE
      NP1=LEFTSH(IN(INDEX+1),MOVEL)
      NP2=RGHTSH(IN(INDEX+2),MOVER)
C      NP3=OR(LEFTSH(IN(INDEX+1),MOVEL),RGHTSH(IN(INDEX+2),MOVER))
      IOUT(I)=AND(OR(NP1,NP2),MASK)
C      IOUT(I)=AND(OR(LEFTSH(IN(INDEX+1),MOVEL),RGHTSH(IN(INDEX+2),MOVER)        
C     1 ),MASK)                                                                  
      GO TO 5                                                                   
C                                                                               
C THE BYTE IS ALREADY RIGHT ADJUSTED.                                           
    3 IOUT(I)=AND(IN(INDEX+1),MASK)                                             
      GO TO 5                                                                   
C                                                                               
C RIGHT ADJUST THE BYTE.                                                        
    4 IOUT(I)=AND(RGHTSH(IN(INDEX+1),MOVER),MASK)                               
C                                                                               
C INCREMENT II AND INDEX.                                                       
    5 II=II+IBITS                                                               
      INDEX=INDEX+IWORDS                                                        
      IF(II.LT.NBITSW) GO TO 6                                                  
      II=II-NBITSW                                                              
      INDEX=INDEX+1                                                             
    6 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SBYTES(IOUT,IN,ISKIP,NBYTE,NSKIP,N)                            
C THIS PROGRAM WRITTEN BY.....                                                  
C             DR. ROBERT C. GAMMILL, CONSULTANT                                 
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH                          
C             JULY 1972                                                         
C THIS IS THE FORTRAN VERSIONS OF SBYTES.                                       
      COMMON/MACHIN/NBITSW,NBITSC,MASK0,MASKS(32)                               
      DIMENSION IN(1),IOUT(1)                                                   
C                                                                               
C  THE STATEMENTS BETWEEN ASTERISK LINES GIVE ALL NECESSARY MACHINE             
C  DEPENDENT INFORMATION                                                        
C***********************************************************************        
C THIS SPECIFICATION IS SUITABLE FOR CDC 6000 SERIES COMPUTERS.                 
      INTEGER RGHTSH,OR,AND                                                     
      OR(M,N)=IOR(M,N)                                                            
      AND(M,N)=IAND(M,N)                                                         
C                            
      LEFTSH(M,N)=ISHFT(M,N)                                                      
      RGHTSH(M,N)=ISHFT(M,-N)                                                      
C***********************************************************************        
C                                                                               
C NBYTE MUST BE LESS THAN OR EQUAL TO NBITSW                                    
      ICON=NBITSW-NBYTE                                                         
      IF(ICON.LT.0) RETURN                                                      
      MASK=MASKS(NBYTE)                                                         
C INDEX TELLS HOW MANY WORDS INTO IOUT THE NEXT BYTE IS TO BE STORED.           
      INDEX=ISKIP/NBITSW                                                        
C II TELLS HOW MANY BITS IN FROM THE LEFT SIDE OF THE WORD TO STORE IT.         
      II=MOD(ISKIP,NBITSW)                                                      
C ISTEP IS THE DISTANCE IN BITS FROM ONE BYTE POSITION TO THE NEXT.             
      ISTEP=NBYTE+NSKIP                                                         
C IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.                
      IWORDS=ISTEP/NBITSW                                                       
C IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.                      
      IBITS=MOD(ISTEP,NBITSW)                                                   
      DO 6 I=1,N                                                                
      J=AND(MASK,IN(I))                                                         
      MOVEL=ICON-II                                                             
      IF(MOVEL) 2,3,4                                                           
C                                                                               
C THE BYTE IS TO BE SPLIT ACROSS A WORD BREAK.                                  
2     MSK=MASKS(NBYTE+MOVEL)                                                    
      IOUT(INDEX+1)=OR(AND(NOT(MSK),IOUT(INDEX+1)),RGHTSH(J,-MOVEL))            
      ITEMP=AND(MASKS(NBITSW+MOVEL),IOUT(INDEX+2))                              
      IOUT(INDEX+2)=OR(ITEMP,LEFTSH(J,NBITSW+MOVEL))                            
      GO TO 5                                                                   
C                                                                               
C BYTE IS TO BE STORED RIGHT-ADJUSTED.                                          
3     IOUT(INDEX+1)=OR(AND(NOT(MASK),IOUT(INDEX+1)),J)                          
      GO TO 5                                                                   
C                                                                               
C BYTE IS TO BE STORED IN MIDDLE OF WORD.  SHIFT LEFT.                          
4     MSK=LEFTSH(MASK,MOVEL)                                                    
      IOUT(INDEX+1)=OR(AND(NOT(MSK),IOUT(INDEX+1)),LEFTSH(J,MOVEL))             
5     II=II+IBITS                                                               
      INDEX=INDEX+IWORDS                                                        
      IF(II.LT.NBITSW) GO TO 6                                                  
      II=II-NBITSW                                                              
      INDEX=INDEX+1                                                             
6     CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SWAP4(IN,IO,NN)
      LOGICAL*1 IN(1),IO(1),IH
      DO 10 I=1,NN,4
      IH=IN(I)
      IO(I)=IN(I+3)
      IO(I+3)=IH
      IH=IN(I+1)
      IO(I+1)=IN(I+2)
      IO(I+2)=IH
  10  CONTINUE
      RETURN
      END
