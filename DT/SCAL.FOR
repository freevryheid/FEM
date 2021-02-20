      FUNCTION SCAL(X,Y,N)
C===================================================================
C     pg 286
C===================================================================
C
C     INNER PRODUCT OF VECTORS X AND Y OF LENGTH N
C       (FUNCTION TO BE WRITTEN EVENTUALLY IN ASSEMBLER)
C=================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*)
      DATA ZERO/0.0D0/
c------------------------------------------------------------------
      SCAL=ZERO
      DO 10 I=1,N
10    SCAL=SCAL+X(I)*Y(I)
      RETURN
      END