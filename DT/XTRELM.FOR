C
      SUBROUTINE XTRELM(IGPE,VCORG,VPRNG,VPREG,KNE,VCORE,VPRNE,VPREE)
C=======================================================================
C     pg 398
C=======================================================================
C     TO GENERATE ELEMENT COORDINATES AND PROPERTIES FROM
C     GLOBAL ARRAYS
C     (IGPE: GROUP NUMBER FOR ELEMENT PROPERTIES)
C====================-==================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/COOR/NDIM
      COMMON/PRND/NPRN
      COMMON/PREL/NGPE,NPRE
      COMMON/RGDT/NUL(5),ICE,IPRNE,IPREE,INEL
      DIMENSION VCORG(*),VPRNG(*),VPREG(*),KNE(*),VCORE(*),
     1 VPRNE(*),VPREE(*)
C-----------------------------------------------------------------------
C------     GENERATE ELEMENT COORDINATES
      IPRNE=0
      ICE=0
      DO 30 IN=1,INEL
      IC=(KNE(IN)-1)*NDIM
      DO 10 I=1,NDIM
      ICE=ICE+1
      IC=IC+1
10    VCORE(ICE)=VCORG(IC)
C-----    GENERATE ELEMENT NODAL PROPERTIES
      IF(NPRN.EQ.0) GO TO 30
      IC=(KNE(IN)-1)*NPRN
      DO 20 I=1,NPRN
      IPRNE=IPRNE+1
      IC=IC+1
20    VPRNE(IPRNE)=VPRNG(IC)
30    CONTINUE
C-----    GENERATE ELEMENT PROPERTIES
      IPREE=0
      IF(NPRE.EQ.0) GO TO 50
      IC=(IGPE-1)*NPRE
      DO 40 I=1,NPRE
      IPREE=IPREE+1
      IC=IC+1
40    VPREE(IPREE)=VPREG(IC)
50    RETURN
      END
      