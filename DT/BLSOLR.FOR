C
      SUBROUTINE BLSOLR
C======================================================================
C     pg 404
C======================================================================
C     TO CALL BLOCK 'SOLR'
C     TO ASSEMBLE DISTRIBUTED LOADS (ELEMENT FUNCTION 7)
C======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/COOR/NDIM,NNT,NDLN,NDLT
      COMMON/ELEM/NUL(4),ME
      COMMON/ASSE/NSYM,NKG,NKE,NDLE
      COMMON/RESO/NEQ,NRES,MRES
      COMMON/ES/M,MR,MP,M1,M2
C----------------------------------------------------------------------
      COMMON/LOC/LCORG,LDLNC,LNEQ,LDIMP,LPRNG,LPREG,LLD,LLOCE,LCORE,LNE,
     1  LPRNE,LPREE,LDLE,LKE,LFE,LKGS,LKGD,LKGI,LFG,LRES,LDLG
      COMMON VA(1000000)
      DIMENSION TBL(8)
      DATA TBL/4HFG  ,4HKE  ,4HFE  ,4HDLE ,4HKGS ,4HKGD ,4HKGI ,
     1 4HRES /
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      IF(M2.EQ.0) M2=ME
      WRITE(MP,2000) M
2000  FORMAT(//' ASSEMBLING OF DISTRIBUTED LOADS (M=',I2,')'/
     1 1X,40('=')/)
      IF(LFG.EQ.1) CALL ESPACE(NEQ,1,TBL(1),LFG)
      IF(LKE.EQ.1) CALL ESPACE(NKE,1,TBL(2),LKE)
      IF(LFE.EQ.1) CALL ESPACE(NDLE,1,TBL(3),LFE)
      IF(LDLE.EQ.1) CALL ESPACE(NDLE,1,TBL(4),LDLE)
      IF(LKGS.EQ.1) CALL ESPACE(NKG,1,TBL(5),LKGS)
      IF(LKGD.EQ.1) CALL ESPACE(NEQ,1,TBL(6),LKGD)
      IF(NSYM.EQ.1.AND.LKGI.EQ.1) CALL ESPACE(NKG,1,TBL(7),LKGI)
      IF(LRES.EQ.1) CALL ESPACE(NDLT,1,TBL(8),LRES)
      CALL EXSOLR(VA(LLD),VA(LDIMP),VA(LLOCE),VA(LCORE),VA(LPRNE),
     1            VA(LPREE),VA(LNE),VA(LKE),VA(LFE),VA(LKGS),VA(LKGD),
     2            VA(LKGI),VA(LFG),VA(LCORG),VA(LDLNC),VA(LNEQ),
     3            VA(LRES),VA(LDLE))
      RETURN
      END
      
      