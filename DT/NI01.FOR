      SUBROUTINE NI01(VKPG,VNI)
C======================================================================
C     pg 197
C======================================================================
C     TO EVALUATE THE INTERPOLATION FUNCTIONS AND THEIR DERIVATIVES
C     D(N)/D(KSI) D(N)/D(ETA) BY THE GENERAL PN-INVERSE METHOD
C     FOR 1,2 OR 3 DIMENSIONAL QUADRATIC ELEMENTS
C     INPUT
C        VKPG    COORDINATES AT WHICH N IS TO BE EVALUATED
C        IPG     NUMBER OF POINTS
C        INEL    NUMBER OF FUNCTIONS N (OR OF NODES)        INEL.LE.20
C        NDIM    NUMBER OF DIMENSIONS                       NDIM.LE.3
C     OUTPUT
C        VNI     FUNCTIONS N AND DERIVATIVES
C======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/COOR/NDIM
      COMMON/RGDT/IEL,ITPE,ITPE1,IGRE,IDLE,ICE,IPRNE,IPREE,INEL,IDEG,IPG
      COMMON/TRVL/VKSI,VPN,VP,KEXP,KDER,K1
      DIMENSION VKPG(*),VNI(*)
      DIMENSION VKSI1(3),KEXP1(3),VKSI2(16),KEXP2(16),VKSI3(60),
     1 KEXP3(60)
C
C.........INFORMATION TO DEFINE THE 3 REFERENCE ELEMENTS
C          (INEL.LE.20 NDIM.LE.3)
C DIMENSION VKSI(NDIM*INEL),KEXP(NDIM*INEL),KDER(NDIM)
      DIMENSION VKSI(60),KEXP(60),KDER(3)
C     DIMENSION VPN(INEL*INEL),VP(INEL)
      DIMENSION VPN(400),VP(20)
C     DIMENSION K1(INEL)
      DIMENSION K1(20)
C     CHARACTERISTICS FOR 1,2 AND 3 DIMENSIONAL REFERENCE ELEMENTS
      DATA VKSI1/-1.D0,0.D0,1.D0/,KEXP1/0,1,2/
      DATA VKSI2/-1.D0,-1.D0, +0.D0,-1.D0, +1.D0,-1.D0, +1.D0,+0.D0,
     1           +1.D0,+1.D0, +0.D0,+1.D0, -1.D0,+1.D0, -1.D0,+0.D0/
      DATA KEXP2/0,0, 1,0, 0,1, 2,0, 1,1, 0,2, 2,1, 1,2/,IDEGR/2/
      DATA VKSI3/-1.D0,-1.D0,-1.D0, +0.D0,-1.D0,-1.D0,
     1           +1.D0,-1.D0,-1.D0, +1.D0,+0.D0,-1.D0,
     2           +1.D0,+1.D0,-1.D0, +0.D0,+1.D0,-1.D0,
     3           -1.D0,+1.D0,-1.D0, -1.D0,+0.D0,-1.D0,
     4           -1.D0,-1.D0,+0.D0, +1.D0,-1.D0,+0.D0,
     5           +1.D0,+1.D0,+0.D0, -1.D0,+1.D0,+0.D0,
     6           -1 D0,-1 D0,+1.D0, +0 D0,-1 D0,+1.D0,
     7           +1 D0,-1 D0,+1.D0, +1 D0,+0 D0,+1.D0,
     8           +1.D0,+1.D0,+1.D0, +0.D0,+1.D0,+1.D0,
     9           -1.D0,+1.D0,+1.D0, -1.D0,+0.D0,+1.D0/
      DATA KEXP3/0,0,0, 1,0,0, 0,1,0, 0,0,1, 1,1,1,
     1  1,1,0, 0,1,1, 1,0,1, 2,0,0, 0,2,0, 0,0,2,
     2  2,1,0, 2,0,1, 2,1,1, 1,2,0, 0,2,1, 1,2,1,
     3  1,0,2, 0,1,2, 1,1,2/
C
C.......
      IDEG=IDEGR
C-------  SELECT TABLES VKSI AND KEXP ACCORDING TO NDIM
      I1=NDIM*INEL
      DO 5 I=1,I1
      GO TO (1,2,3),NDIM
1     VKSI(I)=VKSI1(I)
      KEXP(I)=KEXP1(I)
      GO TO 5
2     VKSI(I)=VKSI2(I)
      KEXP(I)=KEXP2(I)
      GO TO 5
3     VKSI(I)=VKSI3(I)
      KEXP(I)=KEXP3(I)
5     CONTINUE
C-------  EVALUATE THE PN-INVERSE MATRIX
      CALL PNINV(VKSI,KEXP,VP,K1,VPN)
C-------  EVALUATE N,D(N)/D(KSI),D(N)/D(ETA) AT G.P.
      I1=1
      I2=1
      DO 10 IG=1,IPG
      KDER(1)=0
      KDER(2)=0
      KDER(3)=0
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
      KDER(1)=1
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
      IF(NDIM.EQ.1) GO TO 10
      KDER(1)=0
      KDER(2)=1
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
      IF(NDIM.EQ.2) GO TO 10
      KDER(2)=0
      KDER(3)=1
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
10    I1=I1+NDIM
      RETURN
      END