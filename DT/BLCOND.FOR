CLX
      SUBROUTINE BLCOND
C=======================================================================
C     pg 389
C=======================================================================
C     TO CALL BLOCK 'COND'
C     TO READ BOUNDARY CONDITIONS AND GENERATE TABLE (NEQ)
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/COOR/NDIM,NNT,NDLN,NDLT
      COMMON/COND/NCLT,NCLZ,NCLNZ
      COMMON/ALLOC/NVA,IVA
      COMMON/ES/M,MR,MP,M1
      COMMON/LOC/LCORG,LDLNC,LNEQ,LDIMP
      COMMON VA(1000000)
      DIMENSION TBL(2)
      DATA TBL/4HNEQ ,4HDIMP/
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      WRITE(MP,2000) M
2000  FORMAT(//' INPUT OF BOUNDARY CONDITIONS (M=',I2,')'/' ',
     1  33('=')/)
      IF(LNEQ.EQ.1) CALL ESPACE(NDLT,0,TBL(1),LNEQ)
      IF(LDIMP.EQ.1) CALL ESPACE(NDLT,1,TBL(2),LDIMP)
      CALL EXCOND(VA(LCORG),VA(LDLNC),VA(LNEQ),VA(LDIMP))
      CALL VIDE(LDIMP+NCLT,1,TBL(2))
      RETURN
      END


 