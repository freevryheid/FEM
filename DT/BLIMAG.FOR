      SUBROUTINE BLIMAG
C======================================================================
C     pg 383
C======================================================================
C     TO CALL AND EXECUTE BLOCK 'IMAG'
C     TO PRINT OUT THE IMAGE OF DATA CARDS
C=====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ES/M,MR,MP,M1
      COMMON/TRVL/CART(20)
      DATA ICARTM/40/
C-----------------------------------------------------------------------
      IF(M1.EQ.0) M1=MR
      WRITE(MP,2000)
2000  FORMAT(///,1X,'IMAGE OF DATA CARDS'/1X,19('='),/)
      WRITE(MP,2005)
2005  FORMAT(/
     1  50X,'C O L U M N   N U M B E R',/,13X,'CARD',8X,
     2  10X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6',9X,'7',9X,'8',/,
     3  12X,'NUMBER',8X,8('1234567890'),/,12X,8('-'),6X,80('-'))
      ICART=0
      ICART1=0
10    READ(M1,1000,END=30) CART
1000  FORMAT(20A4)
      ICART=ICART+1
      ICART1=ICART1+1
      IF(ICART1.LE.ICARTM) GO TO 20
      WRITE(MP,2010)
2010  FORMAT(12X,8(1H-),6X,80(1H-),/,13X,'CARD',9X,8('1234567890'),/,
     1  12X,'NUMBER',8X,9X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6',
     2  9X,'7',9X,'8',/,50X,'C O L U M N      N U M B E R')
      WRITE(MP,2015)
2015  FORMAT(1H1,//)
      WRITE(MP,2005)
      ICART1=0
20    WRITE(MP,2020) ICART,CART
2020  FORMAT(10X,I10,6X,20A4)
      GO TO 10
30    WRITE(MP,2010)
      WRITE(MP,2030)
2030  FORMAT(///,51X,'E N D    O F   D A T A',/,1H1)
      REWIND M1
      READ(M1,1000) CART
      RETURN
      END