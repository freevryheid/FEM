C
      SUBROUTINE WRELEM(ME,KLOCE,VCORE,VPRNE,VPREE,KNE)
C=======================================================================
C     pg 399
C=======================================================================
C     WRITE ELEMENT PROPERTIES ON FILE ME
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/RGDT/IEL,ITPE,ITPE1,IGRE,IDLE,ICE,IPRNE,IPREE,INEL
      DIMENSION KLOCE(*),VCORE(*),VPRNE(*),VPREE(*),KNE(*)
C-----------------------------------------------------------------------
      IPRNE1=IPRNE
      IF(IPRNE1.EQ.0) IPRNE1=1
      IPREE1=IPREE
      IF(IPREE1.EQ.0) IPREE1=1
      WRITE(ME)IEL,ITPE,IGRE,IDLE,ICE,IPRNE1,IPREE1,INEL,
     1            (KLOCE(I),I=1,IDLE),(VCORE(I),I=1,ICE),
     2            (VPRNE(I),I=1,IPRNE1),(VPREE(I),I=1,IPREE1),
     3            (KNE(I),I=1,INEL)
      RETURN
      END
      