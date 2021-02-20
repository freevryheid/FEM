      SUBROUTINE EQBLOC(KLD,NLBL,NBLMAX,NEQ,KEB,KPB)
C=======================================================================
C     pg 421
C=======================================================================
C     TO FORM TABLES KEB AND KPB DEFINING EQUATION BLOCKS                                           
C       INPUT
C          KLD ARRAY OF A ADDRESS OF COLUMN TOP TERMS IN KG
C          NLBL BLOCKS LENGTH
C          NBLMAX MAX. NUMBER OF BLOCKS ALLOWED
C          NEQ  NUMBER OF EQUATIONS
C       OUTPUT
C          KEB  ARRAY CONTAINING THE NUMBERS OF FIRST EQUATIONS IN
C               EACH BLOCK (DIMENSION NEQ+1) EQBL 11
C          KPB  ARRAY CONTAINING THE NUMBER OF FIRST BLOCKS CONNECTED
C               TO EACH BLOCK (DIMENSION NEQ) EQBL 13
C      NBLMAX NUMBER OF BLOCKS EQBL 14
C=======================================================================
      COMMON/ES/M,MR,MP
      DIMENSION KLD(*),KEB(*),KPB(*)
C-----------------------------------------------------------------------
C-----  FIRST BLOCK
      ILBL=0
      NBL=1
      KEB(1)=1
      KPB(1)=1
      IMIN=1
C-----  FOR EACH EQUATION
      DO 70 IK=1,NEQ
C-----  ADDRESSES FOR COLUMN IK
      JHK=KLD(IK)
      JHK1=KLD(IK+1)
      LBK1=JHK1-JHK
      IF(LBK1.LE.NLBL) GO TO 10
      WRITE(MP,2000) IK,LBK1,NLBL
2000  FORMAT(' *** ERROR,COLUMN',I5,' GREATER(',I5,')THAN BLOCK('  ,I5,'
     1)')
      STOP
C-----  CHECK FOR NEW BLOCK
10    ILBL=ILBL+LBK1
      IF(ILBL.LE.NLBL) GO TO 60
      NBL=NBL+1
      IF(NBL.LE.NBLMAX) GO TO 20
      WRITE(MP,2010) IK
2010  FORMAT(' *** ERROR, EXCESSIVE NUMBER OF BLOCKS, EQUATION',I5)
      STOP
20    KEB(NBL)=IK
      ILBL=LBK1
C-----  SEARCH FOR FIRST BLOCK CONNECTED TO COMPLETED BLOCK
      IB=NBL
40    IF(IMIN.GE.KEB(IB)) GO TO 50
      IB=IB-1
      GO TO 40
50    KPB(NBL-1)=IB
      IMIN=IK
C-----  SEARCH FOR MINIMUM ROW NUMBER FOR COLUMN TOP TERMS
60    I=IK-LBK1+1
      IF(I.LT.IMIN)IMIN=I
70    CONTINUE
C-----  FIRST BLOCK CONNECTED TO LAST BLOCK
      IB=NBL
80    IF(IMIN.GE.KEB(IB)) GO TO 90
      IB=IB-1
      GO TO 80
90    KPB(NBL)=IB
      KEB(NBL+1)=NEQ+1
      NBLMAX=NBL
      RETURN
      END