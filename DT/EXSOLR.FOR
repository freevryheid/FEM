C
      SUBROUTINE EXSOLR(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,
     1       VKGS,VKGD,VKGI,VFG,VCORG,KDLNC,KNEQ,VRES,VDLE)
C=======================================================================
C     pg 405
C=======================================================================
C     TO EXECUTE BLOCK 'SOLR'
C     ASSEMBLE DISTRIBUTED LOADS (ELEMENT FUNCTION 7)
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ASSE/NSYM,NKG,NKE,NDLE
      COMMON/RESO/NEQ,NRES
      COMMON/ES/M,MR,MP,M1,M2
      DIMENSION KLD(*),VDIMP(*),KLOCE(*),VCORE(*),VPRNE(*),VPREE(*),
     1  KNE(*),VKE(*),VFE(*),VKGS(*),VKGD(*),VKGI(*),VFG(*),VCORG(*),
     2  KDLNC(*),KNEQ(*),VRES(*),VDLE(*)
C------------------------------------------------------------------------
C-----  ASSEMBLE FG
      CALL ASFG(KLD,VDIMP,KLOCE,VCORE,VPRNE,VPREE,KNE,VKE,VFE,VKGS,
     1  VKGD,VKGI,VFG,VDLE,VRES)
C-----    OUTPUT
      IF(M.GE.1) WRITE(MP,2000) (VFG(I),I=1,NEQ)
2000  FORMAT(/' GLOBAL LOAD VECTOR (FG)'/(1X,10E12.5))
      RETURN
      END
      
      