      SUBROUTINE ASSELD(IKG,IFG,IDLE,NSYM,IE1,IE2,KLOCE,KLD,VKE,VFE,
     1 VKGS,VKGD,VKGI,VFG)
C=======================================================================
C    pg 423
C=======================================================================
C     TO ASSEMBLE ELEMENT MATRIX (SYMMETRIC OR NOT) AND/OR VECTOR.
C     THE MATRIX IS STORED BLOCKWISE ON DISK
C     INPUT
C        IKG     IF IKG.EQ.1 ASSEMBLE ELEMENT MATRIX KE
C        IFG     IF IFG.EQ.1 ASSEMBLE ELEMENT VECTOR FE
C        IDLE    NUMBER OF D.O.F. OF THE ELEMENT
C        NSYM    0=SYMMETRIC PROBLEM, 1=NON SYMMETRIC PROBLEM
C        IE1,IE2 FIRST AND LAST COLUMN OF KG TO BE ASSEMBLED
C        KLOCE   ELEMENT LOCALIZATION VECTOR
C        KLD     CUMULATIVE COLUMN HEIGHTS IN KG
C        VKE     ELEMENT MATRIX KE (FULL OR UPPER TRIANGLE BY
C                DESCENDING COLUMNS)
C        VFE     ELEMENT VECTOR FE
C     OUTPUT
C        VKGS,VKGD,VKGI GLOBAL MATRIX (SKYLINE)
C                (SYMMETRIC OR NOT)
C        VFG     GLOBAL LOAD VECTOR
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KLOCE(*),KLD(*),VKE(*),VFE(*),VKGS(*),VKGD(*),
     1  VKGI(*),VFG(*)
C-----------------------------------------------------------------------
C
C-----   ASSEMBLE ELEMENT MATRIX
C
      IF(IKG.NE.1) GO TO 100
      I0BLOC=KLD(IE1)-1
      IEQ0=IDLE
      IEQ1=1
C        FOR EACH COLUMN OF KE
      DO 90 JD=1,IDLE
      IF(NSYM.NE.1) IEQ0=JD
      JL=KLOCE(JD)
      IF(JL) 90,90,10
10    I0=KLD(JL+1)-I0BLOC
      IEQ=IEQ1
      IQ=1
      IF(JL.LT.IE1.OR.JL.GT.IE2) GO TO 90
C-----   FOR EACH ROW OF KE
      DO 80 ID=1,IDLE
      IL=KLOCE(ID)
      IF(NSYM.EQ.1) GO TO 30
      IF(ID-JD) 30,20,20
20    IQ=ID
30    IF(IL) 80,80,40
40    IJ=JL-IL
      IF(IJ) 80,50,60
C-----   DIAGONAL TERMS IN KG
50    VKGD(IL)=VKGD(IL)+VKE(IEQ)
      GO TO 80
C-----   UPPER TRIANGLE TERMS IN KG
60    I=I0-IJ
      VKGS(I)=VKGS(I)+VKE(IEQ)
      IF(NSYM.NE.1) GO TO 80
C-----   LOWER TRIANGLE TERMS IN KG
      IEQI=(ID-1)*IDLE+JD
      VKGI(I)=VKGI(I)+VKE(IEQI)
80    IEQ=IEQ+IQ
90    IEQ1=IEQ1+IEQ0
C
C-----   ASSEMLE ELEMENT VECTOR
C
100   IF(IFG.NE.1) GO TO 130
      DO 120 ID=1,IDLE
      IL=KLOCE(ID)
      IF(IL) 120,120,110
110   VFG(IL)=VFG(IL)+VFE(ID)
120   CONTINUE
130   RETURN
      END