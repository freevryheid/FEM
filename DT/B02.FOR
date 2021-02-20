      SUBROUTINE B02(VNIX,INEL,VBE)
C=====================================================================
C     pg 206
C=====================================================================
C   TO FORM MATRIX B (2 DIMENSIONAL ELASTICITY)
C      INPUT
C       VNIX    DERIVATIVES OF INTERPOLATION FUNCTIONS W.R.T. X,Y,Z
C       INEL    NUMBER OF INTERPOLATION FUNCTIONS
C      OUTPUT
C       VBE     MATRIX B
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VNIX(INEL,*),VBE(3,*)
      DATA ZERO/0.D0/
      J=1
      DO 10 I=1,INEL
      C1=VNIX(I,1)
      C2=VNIX(I,2)
      VBE(1,J)=C1
      VBE(1,J+1)=ZERO
      VBE(2,J)=ZERO
      VBE(2,J+1)=C2
      VBE(3,J)=C2
      VBE(3,J+1)=C1
10    J=J+2
      RETURN
      END