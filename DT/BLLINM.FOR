       SUBROUTINE BLLINM
C===============================================================
C     pg 408
C===============================================================
C     TO CALL BLOCK 'LINM'
C     ASSEMBLE AND SOLVE A LINEAR PROBLEM IN CORE
C===============================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/COOR/NDIM,NNT,NDLN,NDLT
      COMMON/ELEM/NUL(4),ME
      COMMON/ASSE/NSYM,NKG,NKE,NDLE
      COMMON/RESO/NEQ,NRES,MRES
      COMMON/ES/M,MR,MP,M1,M2,M3
      COMMON/LOC/LCORG,LDLNC,LNEQ,LDIMP,LPRNG,LPREG,LLD,LLOCE,LCORE,LNE,
     1  LPRNE,LPREE,LDLE,LKE,LFE,LKGS,LKGD,LKGI,LFG,LRES,LDLG
      COMMON VA(1000000)
      DIMENSION TBL(8)
      DATA TBL/4HKGS ,4HKGD ,4HKGI ,4HFG  ,4HKE  ,4HFE  ,4HRES ,4HDLE /
C----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      IF(M2.EQ.0) M2=ME
      IF(M3.EQ.0) M3=MRES
      READ(M1,1000) IN
1000  FORMAT(1I5)
      IF(IN.NE.0) NRES=1
      WRITE(MP,2000) M,NRES
2000  FORMAT(//' ASSEMBLING AND LINEAR SOLUTION (M=',I2,')'/' ',30('=')/
     1  15X,' INDEX FOR RESIDUAL COMPUTATION (NRES)=',I5)
      IF(LKGS.EQ.1) CALL ESPACE(NKG,1,TBL(1),LKGS)
      IF(LKGD.EQ.1) CALL ESPACE(NEQ,1,TBL(2),LKGD)
      IF(NSYM.EQ.1.AND.LKGI.EQ.1) CALL ESPACE(NKG,1,TBL(3),LKGI)
      IF(LFG.EQ.1) CALL ESPACE(NEQ,1,TBL(4),LFG)
      IF(LKE.EQ.1) CALL ESPACE(NKE,1,TBL(5),LKE)
      IF(LFE.EQ.1) CALL ESPACE(NDLE,1,TBL(6),LFE)
      IF(LRES.EQ.1) CALL ESPACE(NDLT,1,TBL(7),LRES)
      IF(LDLE.EQ.1) CALL ESPACE(NDLE,1,TBL(8),LDLE)
      CALL EXLINM(VA(LLD),VA(LDIMP),VA(LLOCE),VA(LCORE),VA(LPRNE),
     1            VA(LPREE),VA(LNE),VA(LKE),VA(LFE),VA(LKGS),VA(LKGD),
     2            VA(LKGI),VA(LFG),VA(LCORG),VA(LDLNC),VA(LNEQ),
     3            VA(LRES),VA(LDLE))
      RETURN
      END