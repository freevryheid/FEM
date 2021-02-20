CLX
      SUBROUTINE BLPREL
C=======================================================================
C     pg 393
C=======================================================================
C     TO CALL BLOCK 'PREL'
C     TO READ ELEMENT PROPERTIES
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 TBL
      COMMON/PREL/NGPE,NPRE
      COMMON/ES/M,MR,MP,M1
      COMMON/LOC/LXX(5),LPREG
      COMMON/TRVL/IN(2)
      COMMON VA(1000000)
      DIMENSION TBL(2)
      DATA TBL/4HPREG,4HV   /
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
C------   READ NUMBER OF GROUPS AND PROPERTIES PER GROUP
      READ(M1,1000) IN
1000  FORMAT(2I5)
      IF(IN(1).GT.0) NGPE=IN(1)
      IF(IN(2).GT.0) NPRE=IN(2)
      WRITE(MP,2000) M,NGPE,NPRE
2000  FORMAT(//' INPUT OF ELEMENT PROPERTIES (M=',I2,')'/' ',
     1 35('=')/15X,'NUMBER OF GROUPS OF PROPERTIES (NGPE)=',I5/
     2 15X,'NUMBER OF PROPERTIES PER GROUP (NPRE)=',I5)
      IF(LPREG.EQ.1) CALL ESPACE(NGPE*NPRE,1,TBL(1),LPREG)
      CALL ESPACE(NPRE,1,TBL(2),L1)
      CALL EXPREL(VA(LPREG),VA(L1))
      CALL VIDE(L1,1,TBL(2))
      RETURN
      END

 