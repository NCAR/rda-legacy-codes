      SUBROUTINE VOPEN(IUN,NB,NBDIM,LBSIZE,LRSIZE,MODE,NRW,IST)                 
      DIMENSION NB(*),IB(1)
      DIMENSION ICSMP(16)                                                       
C                                                                               
C ROUTINES FOR READING/WRITING  IBM VBS FORMAT TAPE RECORDS                     
C ENTRY POINTS                                                                  
C     VOPEN     SETS UP ACCESS TO A LOGICAL UNIT                                
C     VREAD     READS THE NEXT SEQUENTIAL LOGICAL RECORD                        
C     VWRITE    WRITES A LOGICAL RECORD                                         
C     VCLOSE    REMOVES A LOGICAL UNIT CONNECTION                               
C                                                                               
C VOPEN                                                                         
C                                                                               
C     INPUT ARGUMENTS                                                           
C       IUN     THE FORTRAN UNIT TO ACCESS.                                     
C       NB      WORKING BUFFER WHICH MUST BE DIMENSIONED LARGE                  
C               ENOUGH TO CONTAIN 'LBSIZE' BITS PLUS 12 WORDS.                  
C       NBDIM   DIMENSION OF NB.                                                
C       LBSIZE  DESIRED(OR EXPECTED) BLOCK SIZE IN BITS.                        
C       LRSIZE  MAXIMUM LOGICAL RECORD LENGTH IN BITS.                          
C       MODE    SPECIFIES CHECKSUMMING AND STATUS STORE RETRIEVE.               
C               0= DO NEITHER                                                   
C               1= TEST/WRITE 16 BIT XOR CHECKSUM                               
C               10= STORE/RETRIEVE STATUS STORED IN LOGICAL RECORD              
C               11= DO BOTH                                                     
C       NRW     READ/WRITE FLAG WHERE 1=READ, 2=WRITE, 3=WRITE -NO SPANNING.
C     OUTPUT ARGUMENTS                                                          
C       IST     STATUS OF OPEN WHERE 0 IS OK AND 2 IS AN ERROR.                 
C                                                                               
C VREAD  / VWRITE                                                               
C                                                                               
C     INPUT ARGUMENTS                                                           
C       NB      BUFFER CORRESPONDING TO VOPEN CALL
C       IB      LOGICAL RECORD BUFFER (WRITE)                                   
C                                                                               
C     OUTPUT ARGUMENTS                                                          
C       IB      LOGICAL RECORD BUFFER (READ)                                    
C       ICNT    NUMBER OF BYTES TO WRITE OR NUMBER ACTUALLY READ
C       IST     STATUS RETURN 0 =OK , 1=EOF, 2=ERROR, 3=EOD.                    
C                                                                               
C VCLOSE                                                                        
C                                                                               
C     INPUT ARGUMENTS                                                           
C       NB      BUFFER CORRESPONDING TO VOPEN CALL.
C                                                                               
C     OUTPUT ARGUEMENTS                                                         
C       IST     STATUS AS IN VOPEN.
C                                                                               
C  NB BUFFER USAGE                                                              
C     THE FIRST 10 WORDS OF NB ARE USED FOR BOOKKEEPING INFORMATION             
C     WHICH IS DERIVED FROM INFO IN VOPEN CALL OR COUNTER INFO
C     NECESSARY TO KEEP TRACK OF WHAT HAS BEEN USED IN CURRENT BLOCK.           
C     WORDS 11 THRU NBDIM ARE USED FOR UNBLOCKING (BLOCKING) THE                
C     LOGICAL RECORDS.                                                          
C                                                                               
C       WORD    USAGE                                                           
C         1     FORTRAN UNIT NUMBER                                             
C         2     DIMENSION OF NB                                                 
C         3     SIZE OF BLOCK (BITS)                                            
C         4     MAXIMUM SIZE OF LOGICAL RECORD (BITS)                           
C         5     READ/WRITE MODE                                                 
C         6     NUMBER OF BITS AVAILABLE IN BLOCK                               
C         7     NUMBER OF BITS USED IN CURRENT BLOCK                            
C         8     BLOCK COUNT SINCE LAST VOPEN CALL
C         9     READ/WRITE FLAG WHERE 1=OPEN FOR READ, 2=OPEN FOR WRITE         
C        10     WHOLE WORDS NECESSARY TO CONTAIN ONE LOGICAL RECORD             
C                                                                               
C REVISIONS
C       15 APRIL 1992 - FIX TO PREVENT BAD LOGICAL LENGTH FROM
C                       CREATING INFINITE LOOP.
C                       REPLACE DATA STATEMENT BY PARAMETER STATEMENT
C                       FOR DEFINING MACHINE DEPENDENT CONSTANTS.
C                                                                               
C MACHINE DEPENDENT CONSTANTS                                                   
C DEFINE UNIT SIZE FOR WRITE(IWU) AND READ(IRU) IN BITS AND MACHINE             
C     WORD SIZE IN BITS.                                                        
C                                                                               
      PARAMETER (IWU=64,IRU=64,IWZ=64,NZER=0)
      IST=0                                                                     
      LIM=12+(LBSIZE+IWZ-1)/IWZ                                                 
      IF(LIM.LE.NBDIM) GO TO 5                                                  
      PRINT 1001,LIM,NBDIM                                                      
 1001 FORMAT(' VOPEN - BUFFER DIMENSION TOO SMALL , LIMIT, DIMENSION ',
     2 2I6)                                                                     
      IST=2                                                                     
      RETURN                                                                    
    5 NB(1)=IUN                                                                 
      NB(2)=NBDIM-12                                                            
      NB(3)=LBSIZE                                                              
      NB(4)=LRSIZE                                                              
      NB(5)=MODE                                                                
      NB(6)=0                                                                   
      IF(NRW.EQ.2) NB(6)=NB(3)                                                  
      NB(7)=32                                                                  
      NB(8)=0                                                                   
      NB(9)=1                                                                   
      IF(NRW.eq.2 .or. nrw.eq.3) NB(9)=nrw
      NB(10)=(NB(4)+IWZ-1)/IWZ                                                  
      RETURN                                                                    
C                                                                               
      ENTRY VCLOSE(NB,IST)                                                      
      IST=0                                                                     
      IF(NB(9).EQ.1) GO TO 12                                                   
      IF(NB(9).NE.2 .and. nb(9).ne.3) GO TO 90
      IF(NB(7).LE.64) GO TO 12                                                  
      NWU=(NB(7)+IWU-1)/IWU                                                     
      NBYT=(NB(7)+7)/8                                                          
      CALL SBYTE(NB(11),NBYT,0,16)                                              
      ICSM=0                                                                    
      IF(MOD(NB(5),10).NE.1) GO TO 11                                           
      NWCS=(NB(7)+IWZ-1)/IWZ                                                    
      CALL SBYTE(NB(11),NZER,16,16)                                             
      CALL SBYTE(NB(11),NZER,NB(7),IWZ)                                         
      DO 9 I=1,NWCS                                                             
      ICSM= XOR(ICSM,NB(10+I))
    9 CONTINUE                                                                  
      NWCSP=(IWZ+1)/16                                                          
      IF(NWCSP.EQ.1) GO TO 11                                                   
      CALL GBYTES(ICSM,ICSMP,0,16,0,NWCSP)                                      
      ICSM=0                                                                    
      DO 10 I=1,NWCSP                                                           
      ICSM= XOR(ICSM,ICSMP(I))
   10 CONTINUE                                                                  
   11 CALL SBYTE(NB(11),ICSM,16,16)                                             
      CALL WRTAPE(NB(1),1,0,NB(11),NWU)                                         
      CALL IOWAIT(NB(1),KST,KWDS)                                               
      IST=KST                                                                   
      NB(8)=NB(8)+1                                                             
   12 NB(9)=0                                                                   
      RETURN                                                                    
C                                                                               
      ENTRY  VREAD(NB,IB,ICNT,IST)                                              
      ITRF=0                                                                    
      IST=0                                                                     
      ICNT=0                                                                    
      IF(NB(9).NE.1) GO TO 90                                                   
   14 IF(NB(7)+32 .LT. NB(6)) GO TO 18                                          
   13 LLIM=NB(2)+1
      CALL RDTAPE(NB(1),1,0,NB(11),LLIM)                                        
      CALL IOWAIT(NB(1),JST,IWDS)                                               
      IF(IRU*IWDS .GT. IWZ*NB(2)) GO TO 88                                      
      IF(JST.EQ.0) GO TO 15                                                     
      IST=JST                                                                   
      IF(JST.NE.2) RETURN                                                       
   15 CONTINUE                                                                  
      CALL GBYTE(NB(11),NLTH,0,16)                                              
      NB(6)=8*NLTH                                                              
      NB(7)=32                                                                  
      NB(8)=NB(8)+1                                                             
      IF(MOD(NB(5),10).NE.1) GO TO 18                                           
      NWCS=(NB(6)+IWZ-1)/IWZ                                                    
      CALL SBYTE(NB(11),NZER,NB(6),IWZ)                                         
      ICSM=0                                                                    
      DO 16 I=1,NWCS                                                            
      ICSM= XOR(ICSM,NB(10+I))
   16 CONTINUE                                                                  
      NWCSP=(IWZ+1)/16                                                          
      CALL GBYTES(ICSM,ICSMP,0,16,0,NWCSP)                                      
      ICSM=0                                                                    
      DO 17 I=1,NWCSP                                                           
      ICSM= XOR(ICSM,ICSMP(I))
   17 CONTINUE                                                                  
      IF(ICSM.EQ.0 .AND. NB(11).NE.0) GO TO 18                                  
      PRINT 1004,(NB(I),I=1,11),ICSM                                            
 1004 FORMAT(' VBSIO, CHECKSUM ERR- NB(1-11),ICSM ',12I8)                       
   18 CONTINUE                                                                  
      ILP=1+(8*ICNT+IWZ-1)/IWZ                                                  
      ILOF=IWZ*(ILP-1)-8*ICNT                                                   
      CALL GBYTE(NB(11),ILL,NB(7),16)                                           
      ILRL=ILL-4                                                                
      IF(ILRL.LE.0) THEN
	PRINT 1005,ILL
 1005   FORMAT(' BAD LOGICAL LENGTH, SKIP THIS BLOCK, LTH =',I8)
	GO TO 13
      ENDIF
      JLRL=ILRL                                                                 
      ICNT=ICNT+ILRL                                                            
      NB(7)=NB(7)+16                                                            
      CALL GBYTE(NB(11),IFG,NB(7),8)                                            
      NB(7)=NB(7)+8                                                             
      IF(NB(5).GE.10) CALL GBYTE(NB(11),IST,NB(7),8)                            
      NB(7)=NB(7)+8                                                             
      IF(ITRF.NE.0) GO TO 22                                                    
      IF(ICNT.LT.(NB(4)/8)) GO TO 20                                            
      ITRF=1                                                                    
      ILRL=ILRL-ICNT+NB(4)/8                                                    
   20 NLWDS=(8*ILRL+IWZ-1)/IWZ                                                  
      CALL GBYTES(NB(11),IB(ILP),NB(7),IWZ,0,NLWDS)                             
      IF(ILOF.EQ.0) GO TO 22                                                    
      ILOFF=IWZ-ILOF                                                            
      CALL SBYTES(IB(ILP-1),IB(ILP),ILOFF,IWZ,0,NLWDS)                          
   22 NB(7)=NB(7)+8*JLRL                                                        
      IF(IFG.NE.0 .AND. IFG.NE.2) GO TO 14                                      
      RETURN                                                                    
C                                                                               
      ENTRY VWRITE(NB,IB,ICNT,IST)                                              
      IFG=0                                                                     
      ILP=0                                                                     
      IBTW=8*ICNT + 32                                                          
      incful=64
      if(nb(9).eq.3) incful=ibtw
      IF(NB(9).NE.2 .and. nb(9).ne.3) GO TO 90
   25 IF((NB(7)+ incful) .LT. NB(3)) GO TO 30
      NWU=(NB(7)+IWU-1)/IWU
      NBYT=(NB(7)+7)/8                                                          
      CALL SBYTE(NB(11),NBYT,0,16)                                              
      ICSM=0                                                                    
      IF(MOD(NB(5),10).NE.1) GO TO 28                                           
      NWCS=(NB(7)+IWZ-1)/IWZ                                                    
      CALL SBYTE(NB(11),NZER,16,16)                                             
      CALL SBYTE(NB(11),NZER,NB(7),IWZ)                                         
      DO 26 I=1,NWCS                                                            
      ICSM= XOR(ICSM,NB(10+I))
   26 CONTINUE                                                                  
      NWCSP=(IWZ+1)/16                                                          
      IF(NWCSP.EQ.1) GO TO 28                                                   
      CALL GBYTES(ICSM,ICSMP,0,16,0,NWCSP)                                      
      ICSM=0                                                                    
      DO 27 I=1,NWCSP                                                           
      ICSM= XOR(ICSM,ICSMP(I))
   27 CONTINUE                                                                  
   28 CALL SBYTE(NB(11),ICSM,16,16)                                             
      CALL WRTAPE(NB(1),1,0,NB(11),NWU)                                         
      CALL IOWAIT(NB(1),KST,IWDS)                                               
      NB(7)=32                                                                  
      NB(8)=NB(8)+1                                                             
      IF(KST .EQ. 0) GO TO 30                                                   
      PRINT 1002,NB(1),NB(8),KST,IWDS                                           
 1002 FORMAT ('0VBSIO, WRITE ERROR- UNIT,BLOCK,STATUS,LTH -',                   
     2 I2,I8,I3,I6)                                                             
      GO TO 90                                                                  
   30 CONTINUE                                                                  
      IBSG=IBTW                                                                 
      IBAV=NB(3)-NB(7)                                                          
      IF(IBTW.LE.IBAV) GO TO 40                                                 
      IF(IFG.EQ.2) IFG=3                                                        
      IF(IFG.EQ.0) IFG=1                                                        
      IBSG=IBAV                                                                 
   40 ILRL=(IBSG+7)/8
      IBTW=IBTW-IBSG                                                            
      CALL SBYTE(NB(11),ILRL,NB(7),16)                                          
      NB(7)=NB(7)+16                                                            
      CALL SBYTE(NB(11),IFG,NB(7),8)                                            
      NB(7)=NB(7)+8                                                             
c     CALL SBYTE(NB(11),IFG,NZER,8) error in ealier versions- ok if stat store
      CALL SBYTE(NB(11),nzer,nb(7),8)
      IF(NB(5).GE.10) CALL SBYTE(NB(11),IST,NB(7),8)                            
      NB(7)=NB(7)+8                                                             
      NLWDS=(8*ILRL+IWZ-1)/IWZ                                                  
      IF(MOD(ILP,IWZ).EQ.0) GO TO 50                                            
      IPAV=(NB(7)+IWZ-1)/IWZ                                                    
      CALL GBYTES(IB,NB(11+IPAV),ILP,IWZ,0,NLWDS)                               
      IF ((IPAV*IWZ) .EQ. NB(7)) GO TO 55                                       
      CALL SBYTES(NB(11),NB(11+IPAV),NB(7),IWZ,0,NLWDS)                         
      GO TO 55                                                                  
   50 CONTINUE                                                                  
      ILX=ILP/IWZ + 1.001                                                       
      CALL SBYTES(NB(11),IB(ILX),NB(7),IWZ,0,NLWDS)                             
   55 IBADD=IBSG-32                                                             
      NB(7)=NB(7)+IBADD                                                         
      ILP=ILP+IBADD                                                             
      IBTW=IBTW+32                                                              
      IFG=2                                                                     
      IF(IBTW .GT. 32) GO TO 25                                                 
      IST=0                                                                     
      RETURN                                                                    
   88 PRINT 1003,NB(2)                                                          
 1003 FORMAT(' VBSIO, READ TAPE BLOCK TRUNCATED- NBDIM= ',I6)                   
   90 IST=2                                                                     
      RETURN                                                                    
      END                                                                       
