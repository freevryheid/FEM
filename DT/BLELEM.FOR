C
      SUBROUTINE BLELEM
C=======================================================================
C     pg 395
C=======================================================================
C     TO CALL BLOCK 'ELEM'
C     TO READ ELEMENT DATA
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/COOR/NDIM,NNT,NDLN
      COMMON/PRND/NPRN
      COMMON/PREL/NGPE,NPRE
      COMMON/ELEM/NELT,NNEL,NTPE,NGRE,ME,NIDENT,NPG
      COMMON/ASSE/NSYM,NKG
      COMMON/RESO/NEQ
      COMMON/ES/M,MR,MP,M1,M2
      COMMON/LOC/LCORG,LDLNC,LNEQ,LDIMP,LPRNG,LPREG,LLD,LLOCE,LCORE,LNE,
     1 LPRNE,LPREE,LDLE,LKE,LFE,LKGS,LKGD,LKGI,LFG,LRES,LDLG
      COMMON VA(1000000)
      DIMENSION TBL(6),IN(6)
      DATA TBL/4HLD  ,4HLOCE,4HCORE,4HNE  ,4HPRNE,4HPREE/
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      IF(M2.EQ.0) M2=ME
      READ(M1,1000)IN
1000  FORMAT(6I5)
      IF(IN(1).GT.0) NELT=IN(1)
      IF(IN(2).GT.0) NNEL=IN(2)
      IF(IN(3).GT.0) NTPE=IN(3)
      IF(IN(4).GT.0) NGRE=IN(4)
      IF(IN(5).NE.0) NSYM=1
      IF(IN(6).NE.0) NIDENT=1
      WRITE(MP,2000) M,NELT,NNEL,NTPE,NGRE,NSYM,NIDENT
2000  FORMAT(//' INPUT OF ELEMENTS (M=',I2,')'/' ',20('=')/
     1 15X,'MAX. NUMBER OF ELEMENTS                (NELT)=',I5/
     2 15X,'MAX. NUMBER OF NODES PER ELEMENT       (NNEL)=',I5/
     3 15X,'DEFAULT ELEMENT TYPE                   (NTPE)=',I5/
     4 15X,'NUMBER OF GROUPS OF ELEMENTS           (NGRE)=',I5/
     5 15X,'INDEX FOR NON SYMMETRIC PROBLEM        (NSYM)=',I5/
     6 15X,'INDEX FOR IDENTICAL ELEMENTS         (NIDENT)=',I5/)
      IF(LLD.EQ.1) CALL ESPACE(NEQ+1,0,TBL(1),LLD)
      IF(LLOCE.EQ.1) CALL ESPACE(NNEL*NDLN,0,TBL(2),LLOCE)
      IF(LCORE.EQ.1) CALL ESPACE(NNEL*NDIM,1,TBL(3),LCORE)
      IF(LNE.EQ.1) CALL ESPACE(NNEL,0,TBL(4),LNE)
      IF(NPRN.GT.0.AND.LPRNE.EQ.1) CALL ESPACE(NNEL*NPRN,1,TBL(5),LPRNE)
      IF(NPRE.GT.0.AND.LPREE.EQ.1) CALL ESPACE(NPRE,1,TBL(6),LPREE)
      CALL EXELEM(VA(LCORG),VA(LDLNC),VA(LPRNG),VA(LPREG),VA(LLOCE),
     1          VA(LCORE),VA(LNE),VA(LPRNE),VA(LPREE),VA(LNEQ),VA(LLD))
      WRITE(MP,2010) NKG,NPG
2010  FORMAT(15X,'LENGTH OF A TRIANGLE IN KG          (NKG)=',I10/
     1       15X,'NUMBER OF INTEGRATION POINTS        (NPG)=',I10/)
      RETURN
      END
      