C
      SUBROUTINE VIDE(IDEB,IREEL,TBL)
C=====================================================================
C     pg 368
C=====================================================================
C
C     TO DELETE A TABLE FROM VA, FOLLOWED BY COMPACTING
C       INPUT
C          IDEB FIRST POSITION OF TABLE TO BE DELETED
C          IREEL rYPE OF rABLE (SEE ESPACE)
C          TBL NAME OF THE TABLE (A4)
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/ES/M,MR,MP
      COMMON/ALLOC/NVA,IVA,IVAMAX,NREEL,NTBL
      COMMON/LOC/LXX(25)
      COMMON VA(1000000)
C-------  SEARCH FOR THE FIRST POSITION OF NEXT TABLE
      I1=IVA+1
      DO 10 I=1,NTBL
      IF(LXX(I).LE.IDEB) GO TO 10
      IF(LXX(I).LT.I1) I1=LXX(I)
10    CONTINUE
C-------  SHIFT ALL TABLES AFTER THIS
      ID=I1-IDEB
      IF(I1.EQ.IVA+1) GO TO 40
      DO 20 I=1,NTBL
      IF(LXX(I).GT.IDEB) LXX(I)=LXX(I)-ID
20    CONTINUE
      DO 30 I=I1,IVA
      J=I-ID
30    VA(J)=VA(I)
C-------  PRINT
40    IVA=IVA-ID
      IF(M.GT.0) WRITE(MP,2000) TBL,ID,IDEB
2000  FORMAT(60X,'DELETED TABLE ',A4,' COMPACTING ',I7,' REAL WORDS AFTE
     1R VA(',I7,')')
      RETURN
      END
