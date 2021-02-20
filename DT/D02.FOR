      SUBROUTINE D02(VPREE,VDE)
C====================================================================
C     pg 206
C====================================================================
C     TO FORM MATRIX D (2 DIMENSIONAL ELASTICITY)
C        INPUT
C          VPREE ELEMENT PROPERTIES
C                       VPREE(1) YOUNG'S MODULUS
C                       VPREE(2) POISSON'S COEFFICIENT
C                       VPREE(3) .EQ.0 PLANE STRESSES
C                                .EQ.1 PLANE STRAINS
C        OUTPUT
C          VDE     MATRIX D (FULL)
C====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VPREE(*),VDE(9)
      DATA ZERO/0.D0/,UN/1.D0/,DEUX/2.D0/
      E=VPREE(1)
      X=VPREE(2)
      A=VPREE(3)
      C1=E*(UN-A*X)/((UN+X)*(UN-X-A*X))
      C2=C1*X/(UN-A*X)
      C3=E/(DEUX*(UN+X))
      VDE(1)=C1
      VDE(2)=C2
      VDE(3)=ZERO
      VDE(4)=C2
      VDE(5)=C1
      VDE(6)=ZERO
      VDE(7)=ZERO
      VDE(8)=ZERO
      VDE(9)=C3
      RETURN
      END
