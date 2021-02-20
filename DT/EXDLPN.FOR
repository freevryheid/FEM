      SUBROUTINE EXDLPN(KDLNC)
C======================================================================
C     pg 387
C======================================================================
C     TO EXECUTE BLOCK 'DLPN'
C     TO READ THE NUMBER OF D.O.F. PER NODE
C====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/COOR/NDIM,NNT,NDLN,NDLT
      COMMON/ES/M,MR,MP,M1
      COMMON/TRVL/K1(15)
      DIMENSION KDLNC(*)
C-----------------------------------------------------------------------
      IF(M.GT.0) WRITE(MP,2000)
2000  FORMAT(//'GROUP OF D.O.F.'/)
C-------  READ A GROUP CARD
10    READ(M1,1000) IDLN,K1
1000  FORMAT(16I5)
      IF(M.GT.0) WRITE(MP,2010)IDLN,K1
2010  FORMAT(' >>>>>',16I5)
      IF(IDLN.LE.0) GO TO 40
      IF(IDLN.GT.NDLN) CALL ERREUR(21,IDLN,NDLN,1)
C-------  STORE D.O.F. NUMBERS
20    DO 30 I=1,15
      J=K1(I)
      IF(J.LE.0) GO TO 10
      IF(J.GT.NNT) CALL ERREUR(22,J,NNT,1)
30    KDLNC(J+1)=IDLN
      READ(M1,1010) K1
1010  FORMAT(5X,15I5)
      IF(M.GT.0) WRITE(MP,2020) K1
2020  FORMAT(' >>>>>',5X,15I5)
      GO TO 20
C-------  TOTAL NUMBER OF D.O.F.
40    NDLT=0
      J=NNT+1
      DO 50 I=2,J
50    NDLT=NDLT+KDLNC(I)
      RETURN
      END