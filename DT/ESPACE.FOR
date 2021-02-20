C
      SUBROUTINE ESPACE(ILONG,IREEL,TBL,IDEB)
C======================================================================
C     pg 368
C======================================================================
C     TO ALLOCATE A REAL OR INTEGER TABLE IN ARRAY VA
C                 INPUT
C                    ILONG  LENGTH OF THE TABLE T0 BE ALLOCATED
C                           (IN REAL OR INTEGER WORDS~
C                    IREEL  TABLE TYPE :
C                                .EQ.0 INTEGER
C                                .EQ.1 REAL
C                    TBL  NAME OF THE TABLE (A4)
C                 OUTPUT
C                     IDEB  TABLE TO BE ALLOCATED STARTS IN VA(IDEB)
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/ES/M,MR,MP
      COMMON/ALLOC/NVA,IVA,IVAMAX,NREEL
      COMMON VA(1000000)
      DIMENSION KA(1000000)
      EQUIVALENCE (VA(1000000),KA(1000000))
      DATA ZERO/0.D0/
C---------------------------------------------------------------------
C-------  CALCULATE THE TABLE LENGTH IN REAL WORDS
      ILGR=ILONG
      IF(IREEL.EQ.0) ILGR=(ILONG+NREEL-1)/NREEL
      IVA1=IVA+ILGR
C-------  CHECK IF ENOUGH SPACE IS AVAILABLE
      IF(IVA1.LE.NVA) GO TO 20
C.........AUTOMATIC EXTENSION OF THE BLANK COMMON IF CORRESPONDING
C         SYSTEM COMMAND EXIST ON THE COMPUTER USED
C     CALL EXTEND(IVA1,IERR)
C     IF(IERR.EQ.1) GO TO 10
C     NVA=IVA1
C     GO TO 20
C-------  ALLOCATION ERROR (NOT ENOUGH SPACE)
10    WRITE(MP,2000) TBL,IVA1,NVA
2000  FORMAT(' **** ALL0CATION ERROR, TABLE ',A4/' REQUIRED SPACE:',I9,'
     1 REAL WORDS, AVAILABLE SPACE:',I9,' REAL WORDS')
      STOP
C-------  ALLOCATE TABLE
20    IDEB=IVA+1
      IVA=IVA1
      IF(IVA.GT.IVAMAX) IVAMAX=IVA
      IF(M.GT.0) WRITE(MP,2010) TBL,IDEB,IVA1
2010  FORMAT(60X,'TABLE ',A4,' GOES FROM VA(',I7,') TO VA(',I7,')')
C-------  INITIALIZE THE ALLOCATED TABLE TO ZERO
      I1=IDEB
      IF(IREEL.EQ.0) I1=(I1-1)*NREEL+1
      I2=I1+ILONG-1
      IF(IREEL.EQ.0) GO TO 40
      DO 30 I=I1,I2
30    VA(I)=ZERO
      RETURN
40    DO 50 I=I1,I2
50    KA(I)=0
      RETURN
      END
