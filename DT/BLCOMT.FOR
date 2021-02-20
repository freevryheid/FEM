      SUBROUTINE BLCOMT
C=====================================================================
C     pg 384
C=====================================================================
C     TO CALL AND EXECUTE BLOCK 'COMT'
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 BLANC,CART
      COMMON/ES/M,MR,MP
      COMMON/TRVL/CART(20)
      DATA BLANC/4H    /
C---------------------------------------------------------------------
      WRITE(MP,2000)
2000  FORMAT(//' COMMENTS '/' ',8('=')/)
C-------  READ A COMMENT CARD
10    READ(MR,1000) CART
1000  FORMAT(20A4)
C-------  SEARCH FOR A WHOLLY BLANK CARD
      DO 20 I=1,20
      IF(CART(I).NE.BLANC) GO TO 30
20    CONTINUE
      RETURN
30    WRITE(MP,2010) CART
2010  FORMAT(1X,20A4)
      GO TO 10
      END