      SUBROUTINE BTDB(VKE,VBE,VDE,IDLE,IMATD,NSYM)
C=====================================================================
C     pg 207
C=====================================================================
C TO ADD THE PRODUCT B(T).D.B TO VKE
C     INPUT
C       VKE     ELEMENT MATRIX NON SYMMETRICAL (NSYM.EQ.1)
C                                  SYMMETRICAL (NSYM.EQ.O)
C       VBE     MATRIX B
C       VDE     MATRIX D (FULL)
C       IDLE    TOTAL NUMBER OF D.O.F. PER ELEMENT
C          IMATD DIMENSION OF MATRIX D (MAX. 6)
C        OUTPUT
C          VKE
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VKE(*),VBE(IMATD,*),VDE(IMATD,*),T(6)
      DATA ZERO/0.D0/
C---------------------------------------------------------------------
      IJ=1
      IMAX=IDLE
      DO 40 J=1,IDLE
      DO 20 I1=1,IMATD
      C=ZERO
      DO 10 J1=1,IMATD
10    C=C+VDE(I1,J1)*VBE(J1,J)
20    T(I1)=C
      IF(NSYM.EQ.0) IMAX=J
      DO 40 I=1,IMAX
      C=ZERO
      DO 30 J1=1,IMATD
30    C=C+VBE(J1,I)*T(J1)
      VKE(IJ)=VKE(IJ)+C
40    IJ=IJ+1
      RETURN
      END