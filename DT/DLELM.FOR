      SUBROUTINE DLELM(KLOCE,VDLG,VDIMP,VDLE)
C======================================================================
C     pg 416
C======================================================================
C
C     TO GENERATE ELEMENT D.O.F.
C======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/RGDT/IEL,INUL(3),IDLE
      COMMON/ES/M,MR,MP
      DIMENSION KLOCE(*),VDLG(*),VDIMP(*),VDLE(*)
      DATA ZERO/0.D0/
C----------------------------------------------------------------------
      DO 40 ID=1,IDLE
      IL=KLOCE(ID)
      IF(IL) 10,20,30
10    VDLE(ID)=VDIMP(-IL)
      GO TO 40
20    VDLE(ID)=ZERO
      GO TO 40
30    VDLE(ID)=VDLG(IL)
40    CONTINUE
      IF(M.GE.2) WRITE(MP,2000) IEL,(VDLE(ID),ID=1,IDLE)
2000  FORMAT(' DEGREES OF FREEDOM OF ELEMENT ',I5/(1X,10E12.5))
      RETURN
      END