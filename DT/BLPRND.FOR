CLX
      SUBROUTINE BLPRND
C=======================================================================
C     pg 392
C=======================================================================
C     TO CALL BLOCK 'PRND'
C     TO READ NODAL PROPERTIES
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/COOR/NDIM,NNT
      COMMON/PRND/NPRN
      COMMON/ES/M,MR,MP,M1
      COMMON/LOC/LXX(4),LPRNG
      COMMON VA(1000000)
      DATA TBL/4HPRNG/
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      READ(M1,1000) NPRN
1000  FORMAT(I5)
      WRITE(MP,2000) M,NPRN
2000  FORMAT(//' INPUT OF NODAL PROPERTIES (M=',I2,')'/' ',30('=')/
     1  15X,'NUMBER OF PROPERTIES PER NODE    (NPRN)=',I5)
      IF(LPRNG.EQ.1) CALL ESPACE(NNT*NPRN,1,TBL,LPRNG)
      CALL EXPRND(VA(LPRNG))
      RETURN
      END
 