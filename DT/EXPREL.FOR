CLX
      SUBROUTINE EXPREL(VPREG,V1)
C=======================================================================
C     pg 393
C=======================================================================
C     TO EXECUTE BLOCK 'PREL'
C     READ ELEMENT PROPERTIES
C=======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PREL/NGPE,NPRE
      COMMON/ES/M,MR,MP,M1
      DIMENSION VPREG(*),V1(*)
C-----------------------------------------------------------------------
      IF(M.GE.0) WRITE(MP,2000)
2000  FORMAT(//' CARDS OF ELEMENT PROPERTIES'/)
C-----   READ A GROUP
       I1= MIN0(7,NPRE)
       J=1
10     READ(M1,1000) IGPE,(V1(I),I=1,I1)
1000   FORMAT(I5,7F10.0)
       IF(M.GE.0) WRITE(MP,2010) IGPE,(V1(I),I=1,I1)
2010   FORMAT(' >>>>>',I5,7E12.5)
       IF(IGPE.LE.0) GO TO 40
       IF(IGPE.GT.NGPE) CALL ERREUR(41,IGPE,NGPE,1)
       IF(NPRE.LE.7) GO TO 20
C-------   READ THE PROPERTIES
       READ(M1,1010) (V1(I),I=8,NPRE)
1010   FORMAT(5X,7F10.0)
       IF(M.GE.0) WRITE(MP,2020) (V1(I),I=8,NPRE)
2020   FORMAT(' >>>>>',5X,7E12.5)
20     DO 30 I=1,NPRE
       VPREG(J)=V1(I)
30     J=J+1
       GO TO 10
40     RETURN
       END
