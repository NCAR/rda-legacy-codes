      INTEGER*4 N_MAX,L_DIGS
      PARAMETER (N_MAX = 7)
      PARAMETER (L_DIGS = 2)
      CHARACTER*20 LOG_NAM
      PARAMETER (LOG_NAM = 'LOGICAL_MAGTAPE_')
C     L_DIGS is how many digits are required to express
C     all the logical tape numbers on this system.
C     By default it is set to 2 which should suffice
C     for virtually any VAX/VMS system.
      COMMON/T_COM/IOSB(2,0:N_MAX),ISTAT(0:N_MAX),LFIRST(0:N_MAX)
      COMMON/T_COM/ICHAN(0:N_MAX),IFLAG(0:N_MAX),NBY(0:N_MAX)
      LOGICAL LFIRST
