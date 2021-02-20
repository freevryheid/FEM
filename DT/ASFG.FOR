C
      SUBROUTINE ASFG(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,
     1  VKGS,VKGD,VKGI,VFG,VDLE,VRES)
C======================================================================
C     pg 405
C======================================================================
C
C     ASSEMBLING DISTRIBUTED LOADS IN FG
C======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ELEM/NELT,NNEL,NTPE,NGRE,ME,NIDENT
      COMMON/ASSE/NSYM
      COMMON/RESO/NEQ
      COMMON/RGDT/IEL,ITPE,ITPE1,IGRE,IDLE,ICE,IPRNE,IPREE,INEL,IDEG,IPG
     1  ,ICOD
      COMMON/ES/M,MR,MP,M1,M2
      DIMENSION KLD(*),VDIMP(*),KLOCE(*),VCORE(*),VPRNE(*),VPREE(*),
     1  KNE(*),VKE(*),VFE(*),VKGS(*),VKGD(*),VKGI(*),VFG(*),VDLE(*),
     2  VRES(*)
C-----------------------------------------------------------------------
C-----   REWIND ELEMENT FILE M2
      REWIND M2
C-----   LOOP OVER THE ELEMENTS
      DO 20 IE=1,NELT
C-----   READ AN ELEMENT FROM FILE M2
      CALL RDELEM(M2,KLOCE,VCORE,VPRNE,VPREE,KNE)
C-----   EVALUATE INTERPOLATION FUNCTIONS IF REQUIRED
      IF(ITPE.EQ.ITPE1) GO TO 10
      ICOD=2
      CALL ELEMLB(VCORE,VPRNE,VPREE,VDLE,VKE,VFE)
C-----   EVALUATE ELEMENT VECTOR
10    ICOD=7
      CALL ELEMLB(VCORE,VPRNE,VPREE,VDLE,VKE,VFE)
C-----   PRINT ELEMENT VECTOR VFE
      IF(M.GE.2) WRITE(MP,2000) IEL,(VFE(I),I=1,IDLE)
2000  FORMAT(/' VECTOR (FE) , ELEMENT:',I5/(10X,10E12.5))
C-----   ASSEMBLE
      CALL ASSEL(0,1,IDLE,NSYM,KLOCE,KLD,VKE,VFE,VKGS,VKGD,VKGI,VFG)
20    ITPE1=ITPE
      RETURN
      END
      
      