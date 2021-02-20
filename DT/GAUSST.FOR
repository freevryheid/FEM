      SUBROUTINE GAUSST(IPGKED,NDIM,VKPG,VCPG,IPG)
C======================================================================
C     pg 267 
C     NOTE: THIS CAN BE EXTENDED TO 12 IPG
C======================================================================
C     TO FORM ARRAYS OF COORDINATES AND WEIGHTS AT INTEGRATION
C     POINTS FOR TRIANGULAR ELEMENTS
C       INPUT
C        IPGKED(1)  NUMBER OF INTEGRATION POINTS
C        IPGKED(2)  .EQ.1 IF WE HAVE 3 POINTS AT MID SIDES
C        NDIM    NUMBER OF DIMENSIONS (1,2 OR 3)
C     OUTPUT
C        VKPG    COORDINATES OF GAUSS POINTS
C        VCPG    WEIGHTS AT GAUSS POINTS
C        IPG     TOTAL NUMBER OF GAUSS POINTS
C====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ES/MR,MP
      DIMENSION IPGKED(*),VKPG(*),VCPG(*)
      DATA ZERO/0.D0/,UN/1.D0/,DEUX/2.D0/,TROIS/3.D0/,CINQ/5.D0/
      DATA SIX/6.D0/
      SQRT(X)=DSQRT(X)
      IPG=IPGKED(1)
C-------  1 POINT AT THE GRAVITY CENTER
      IF(IPG.NE.1) GO TO 10
      VCPG(1)=UN/DEUX
      VKPG(1)=UN/TROIS
      VKPG(2)=UN/TROIS
      GO TO 100
C-------  3 POINTS
10    IF(IPG.NE.3) GO TO 20
      C1=UN/SIX
      VCPG(1)=C1
      VCPG(2)=C1
      VCPG(3)=C1
C-------  3 POINTS AT MID SIDES
      IF (IPGKED(2).NE.1) GO TO 11
      VKPG(1)=UN/DEUX
      VKPG(2)=UN/DEUX
      VKPG(3)=ZERO
      VKPG(4)=UN/DEUX
      VKPG(5)=UN/DEUX
      VKPG(1)=ZERO
      GO TO 100
C-------  3 POINTS AT ONE THIRD OF THE MEDIANS
11    VKPG(1)=C1
      VKPG(2)=C1
      VKPG(3)=DEUX/TROIS
      VKPG(4)=C1
      VKPG(5)=C1
      VKPG(6)=DEUX/TROIS
      GO TO 100
C-------  4 POINTS AT ONE THIRD OF THE MEDIANS AND INTERSECTION
20    IF (IPG.NE.4) GO TO 30
      C2=-0.28125D0
      C3=0.2604166666666667D0
      VCPG(1)=C2
      VCPG(2)=C3
      VCPG(3)=C3
      VCPG(4)=C3
      VKPG(1)=UN/TROIS
      VKPG(2)=UN/TROIS
      VKPG(3)=UN/CINQ
      VKPG(4)=UN/CINQ
      VKPG(5)=TROIS/CINQ
      VKPG(6)=UN/CINQ
      VKPG(7)=UN/CINQ
      VKPG(8)=TROIS/CINQ
      GO TO 100
C-------  6 POINTS
30    IF (IPG.NE.6) GO TO 40
      C1=0.111690794839005D0
      C2=0.054975871827661D0
      C3=0.445948490915965D0
      C4=0.091576213509771D0
      VCPG(1)=C1
      VCPG(2)=C1
      VCPG(3)=C1
      VCPG(4)=C2
      VCPG(5)=C2
      VCPG(6)=C2
      VKPG(1)=C3
      VKPG(2)=C3
      VKPG(3)=UN-DEUX*C3
      VKPG(4)=C3
      VKPG(5)=C3
      VKPG(6)=UN-DEUX*C3
      VKPG(7)=C4
      VKPG(8)=C4
      VKPG(9)=UN-DEUX*C4
      VKPG(10)=C4
      VKPG(11)=C4
      VKPG(12)=UN-DEUX*C4
      GO TO 100
C-------  7 POINTS
40    IF (IPG.NE.7) GO TO 99
      C1=9.D0/80.D0
      C2=0.066197076394253D0
      C3=0.062969590272414D0
      C4=0.470142064105115D0
      C5=0.101286507323456D0
      VCPG(1)=C1
      VCPG(2)=C2
      VCPG(3)=C2
      VCPG(4)=C2
      VCPG(5)=C3
      VCPG(6)=C3
      VCPG(7)=C3
      VKPG(1)=UN/TROIS
      VKPG(2)=UN/TROIS
      VKPG(3)=C4
      VKPG(4)=C4
      VKPG(5)=UN-DEUX*C4
      VKPG(6)=C4
      VKPG(7)=C4
      VKPG(8)=UN-DEUX*C4
      VKPG(9)=C5
      VKPG(10)=C5
      VKPG(11)=UN-DEUX*C5
      VKPG(12)=C5
      VKPG(13)=C5
      VKPG(14)=UN-DEUX*C5
100   RETURN
99    WRITE(MP,2000) IPG
2000  FORMAT('*** ERROR: NUMBER OF INTEGRATION POINTS (IPG) =',I5)                  
      RETURN
      END
      