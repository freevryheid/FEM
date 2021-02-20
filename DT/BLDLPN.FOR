      SUBROUTINE BLDLPN
C=======================================================================
C     pg 387
C=======================================================================
C     TO CALL BLOCK 'DLPN'
C     TO READ NUMBER OF D.O.F. PER NODE
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ES/M,MR,MP,M1
      COMMON/LOC/LCORG,LDLNC
      COMMON VA(1000000)
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      WRITE(MP,2000) M
2000  FORMAT(//' INPUT OF D.O.F. (M=',I2,')'/' ',17('='))
      CALL EXDLPN(VA(LDLNC))
      RETURN
      END