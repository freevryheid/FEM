C
      SUBROUTINE LOCELD(KDLNC,KNE,KNEQ,KLOCE,KLD)
C=======================================================================
C     pg 397
C=======================================================================
C     TO FORM THE ELEMENT LOCALIZATION TABLE (LOCE)
C     AND UPDATE COLUMN HEIGHTS FOR A GIVEN ELEMENT
C=======================================================================
      COMMON/COOR/NDIM,NNT
      COMMON/RGDT/NUL(4),IDLE,NUL1(3),INEL
      DIMENSION KDLNC(*),KNE(*),KNEQ(*),KLOCE(*),KLD(*)
      DATA NDLMAX/32000/
C-----------------------------------------------------------------------
C-----     GENERATE KLOCE FROM KNEQ
      IDLE=0
      LOCMIN=NDLMAX
      DO 20 IN=1,INEL
      INN=KNE(IN)
      IF(INN.GT.NNT) CALL ERREUR(56,INN,NNT,1)
      IEQ=KDLNC(INN)
      IEQ1=KDLNC(INN+1)
10    IF(IEQ.GE.IEQ1) GO TO 20
      IEQ=IEQ+1
      IDLE=IDLE+1
      J=KNEQ(IEQ)
      KLOCE(IDLE)=J
      IF(J.LT.LOCMIN.AND.J.GT.0) LOCMIN=J
      GO TO 10
20    CONTINUE
C------   UPDATE TABLE OF COLUMN HEIGHTS (KLD)
      DO 30 ID=1,IDLE
      J=KLOCE(ID)
      IF(J.LE.0) GO TO 30
      IH=J-LOCMIN
      IF(IH.GT.KLD(J+1))KLD(J+1)=IH
30    CONTINUE
      RETURN
      END
      