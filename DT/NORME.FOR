      SUBROUTINE NORME(N,VDEL,V,XNORM)
C=======================================================================
C     pg 429
C=======================================================================
C     COMPUTE THE LENGTHS RATIO OF VECTORS VDEL AND V
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VDEL(*),V(*)
      DATA ZERO/0.D0/,UN/1.D0/,FAC/1.D-3/
      SQRT(X)=DSQRT(X)
C-----------------------------------------------------------------------
      C1=ZERO
      C2=ZERO
      DO 10 I=1,N
      C1=C1+VDEL(I)*VDEL(I)
10    C2=C2+V(I)*V(I)
      C=C1*FAC
      IF(C2.LE.C) C2=UN
      XNORM=SQRT(C1/C2)
      RETURN
      END