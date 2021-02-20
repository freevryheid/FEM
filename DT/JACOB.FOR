      SUBROUTINE JACOB(VNI,VCORE,NDIM,INEL,VJ,VJ1,DETJ)
C=======================================================================
C    pg 63
C=======================================================================
C    TO EVALUATE THE JACOBIAN MATRIX, ITS DETERMINANT AND
C    ITS INVERSE (1,2,3 DIMENSIONS)
C      INPUT
C         VNI DERIVATIVES OF INTERPOLATION FUNCTION W.R.T.
C                 KSI,ETA,DZETA
C          VCORE ELEMENT NODAL COORDINATES
C          NDIM    NUMBER OF DIMENSIONS
C          INEL    NUMBER OF NODES PER ELEMENT
C      OUTPUT
C          VJ      JACOBIAN MATRIX
C          VJ1     INVERSE OF JACOBIAN MATRIX
C          DETJ    DETERMINANT OF JACOBIAN MATRIX
C========================--=============================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VNI(INEL,*),VCORE(NDIM,*),VJ(*),VJ1(*)
      DATA ZERO/0.D0/,UN/1.D0/
C-----------------------------------------------------------------------
C------- FORM THE JACOBIAN MATRIX
      J=1
      DO 20 JJ=1,NDIM
      DO 20 II=1,NDIM
      C=ZERO
      DO 10 IJ=1,INEL
10    C=C+VNI(IJ,II)*VCORE(JJ,IJ)
      VJ(J)=C
20    J=J+1
C------- 1, 2, OR 3 DIMENSIONAL INVERSION
      GO TO (40,50,60),NDIM
40    DETJ=VJ(1)
      IF(DETJ.EQ.ZERO) RETURN
      VJ1(1)=UN/DETJ
      RETURN
50    DETJ=VJ(1)*VJ(4)-VJ(2)*VJ(3)
      IF(DETJ.EQ.ZERO) RETURN
      VJ1(1)=VJ(4)/DETJ
      VJ1(2)=-VJ(2)/DETJ
      VJ1(3)=-VJ(3)/DETJ
      VJ1(4)=VJ(1)/DETJ
      RETURN
60    DETJ=VJ(1)*(VJ(5)*VJ(9)-VJ(8)*VJ(6))
     1    +VJ(4)*(VJ(8)*VJ(3)-VJ(2)*VJ(9))
     2    +VJ(7)*(VJ(2)*VJ(6)-VJ(5)*VJ(3))
      IF(DETJ.EQ.ZERO) RETURN
      VJ1(1)=(VJ(5)*VJ(9)-VJ(6)*VJ(8))/DETJ
      VJ1(2)=(VJ(3)*VJ(8)-VJ(2)*VJ(9))/DETJ
      VJ1(3)=(VJ(2)*VJ(6)-VJ(3)*VJ(5))/DETJ
      VJ1(4)=(VJ(7)*VJ(6)-VJ(4)*VJ(9))/DETJ
      VJ1(5)=(VJ(1)*VJ(9)-VJ(7)*VJ(3))/DETJ
      VJ1(6)=(VJ(4)*VJ(3)-VJ(6)*VJ(1))/DETJ
      VJ1(7)=(VJ(4)*VJ(8)-VJ(7)*VJ(5))/DETJ
      VJ1(8)=(VJ(2)*VJ(7)-VJ(8)*VJ(1))/DETJ
      VJ1(9)=(VJ(1)*VJ(5)-VJ(4)*VJ(2))/DETJ
      RETURN
      END

