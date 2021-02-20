      SUBROUTINE DNIDX(VNI,VJ1,NDIM,INEL,VNIX)
C======================================================================
C     pg 64
C======================================================================
C     COMPUTE THE DERIVATIVES OF INTERPOLATION FUNCTIONS WITH
C     RESPECT TO X,Y,Z
c     (1,2 OR 3 DIMENSIONS)
C     INPUT
C       VNI  DERIVATIVES OF INTERPOLATION FUNCTIONS WITH RESPECT
C            TO KSI,ETA,DZETA
C       VJ1  INVERSE OF THE JACOBIAN
C       NDIM NUMBER OF DIMENSIONS (1,2 OR 3)
C       INEL NUMBER OF INTERPOLATION FUNCTIONS (OR NODES)
C   OUTPUT
C       VNIX X,Y,Z DERIVATIVES OF INTERPOLATION FUNCTIONS
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VNI(INEL,*),VJ1(NDIM,*),VNIX(INEL,*)
      DATA ZERO/0.D0/
C-----------------------------------------------------------------------
      DO 20 I=1,NDIM
      DO 20 J=1,INEL
      C=ZERO
      DO 10 IJ=1,NDIM
10    C=C+VJ1(I,IJ)*VNI(J,IJ)
20    VNIX(J,I)=C
      RETURN
      END