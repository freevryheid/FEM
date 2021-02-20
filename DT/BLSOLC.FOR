C
      SUBROUTINE BLSOLC
C=======================================================================
C     pg 400
C=======================================================================
C TO CALL BLOCK "SOLC'
C TO READ CONCENTRATED LOADS
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/RESO/NEQ
      COMMON/ES/M,MR,MP,M1
      COMMON/LOC/LCORG,LDLNC,LNEQ,LXX(15),LFG
      COMMON VA(1000000)
      DATA TBL/4HFG  /
      IF(M1.EQ.0) M1=MR
      WRITE(MP,2000) M
2000  FORMAT(//' INPUT OF CONCENTRADED LOADS (M=',I2,')'/' ',
     1  39('='))
      IF(LFG.EQ.1) CALL ESPACE(NEQ,1,TBL,LFG)
      CALL EXSOLC(VA(LFG),VA(LDLNC),VA(LNEQ))
      RETURN
      END
      
      