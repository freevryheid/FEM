      SUBROUTINE NI02(VKPG,VNI)
C======================================================================
C     pg 205
C======================================================================
C     TO EVALUATE THE INTERPOLATION FUNCTIONS N AND THEIR DERIVATIVES
C     D(N)/D(KSI) AND D(N)/D(ETA) BY GENERAL PN-INVERSE METHOD
C       INPUT
C          VKPG     COORDINATES AT WHICH N IS TO BE EVALUATED
C          IPG      NUMBER OF POINTS
C          INEL     NUMBER OF FUNCTIONS N (OR OF NODES)     INEL.EQ.8
C          NDIM     NUMBER OF DIMENSIONS                    NDIM.EQ.2
C       OUTPUT
C          VNI      FUNCTIONS N AND DERIVATIVES
C======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/COOR/NDIM
      COMMON/RGDT/IEL,ITPE,ITPE1,IGRE,IDLE,ICE,IPRNE,IPREE,INEL,IDEG,IPG
      DIMENSION VKPG(*),VNI(*)
C
C.........INFORMATIONS RELATED TO THE 8 NODES REFERENCE SQUARE ELEMENT
C           (INEL.EQ.8 NDIM.EQ.2)
C     DIMENSION VKSI(NDIM*INEL),KEXP(NDIM*INEL),KDER(NDIM)
      DIMENSION VKSI(16),KEXP(16),KDER(2)
C     DIMENSION VPN (INEL*INEL),VP(INEL),K1(INEL)
      DIMENSION VPN(64),VP(8),K1(8)
C         NODAL COORDINATES OF THE REFERENCE ELEMENT
      DATA VKSI/-1.D0,-1.D0, +0.D0,-1.D0, +1.D0,-1.D0, +1.D0,+0.D0,
     1 +1.D0,+1.D0, +0.D0,+1.D0, -1.D0,+1.D0, -1.D0,+0.D0/
C          MONOMIAL EXPONENTS OF THE POLYNOMIAL BASIS, MAX-DEGREE
       DATA KEXP/0,0, 1,0, 0,1, 2,0, 1,1, 0,2, 2,1, 1,2/,IDEGR/2/
C
C.......
      IDEG=IDEGR
C-------  EVALUATE THE PN-INVERSE MATRIX
      CALL PNINV(VKSI,KEXP,VP,K1,VPN)
C-------  EVALUATE N,D(N)/D(KSI),D(N)/D(ETA) AT G.P.
      I1=1
      I2=1
      DO 10 IG=1,IPG
      KDER(1)=0
      KDER(2)=0
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
      KDER(1)=1
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
      KDER(1)=0
      KDER(2)=1
      CALL NI(VKPG(I1),KEXP,KDER,VP,VPN,VNI(I2))
      I2=I2+INEL
10    I1=I1+NDIM
      RETURN
      END