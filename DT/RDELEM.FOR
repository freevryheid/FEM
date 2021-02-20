CLX
      SUBROUTINE RDELEM(ME,KLOCE,VCORE,VPRNE,VPREE,KNE)
C=======================================================================
C     pg 399
C=======================================================================
C     READ ELEMENT PROPERTIES FROM FILE ME
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/RGDT/IEL,ITPE,ITPE1,IGRE,IDLE,ICE,IPRNE,IPREE,INEL
      DIMENSION KLOCE(*),VCORE(*),VPRNE(*),VPREE(*),KNE(*)
C-----------------------------------------------------------------------
      READ(ME) IEL,ITPE,IGRE,IDLE,ICE,IPRNE,IPREE,INEL,
     1            (KLOCE(I),I=1,IDLE),(VCORE(I),I=1,ICE),
     2            (VPRNE(I),I=1,IPRNE),(VPREE(I),I=1,IPREE),
     3            (KNE(I),I=1,INEL)
      RETURN
      END
      
      