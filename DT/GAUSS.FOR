      SUBROUTINE GAUSS(IPGKED,NDIM,VKPG,VCPG,IPG)
C======================================================================
C     pg 265
C======================================================================
C     TO FORM ARRAYS OF COORDINATES AND WEIGHTS AT GAUSS POINTS
C     (1,2 AND 3 DIMENSIONS)(1,2,3 OR 4 G.P. PER DIMENSION)
C       INPUT
C        IPGKED NUMBER OF POINTS IN KSI,ETA,ZETA DIRECTIONS
C        NDIM    NUMBER OF DIMENSIONS (1,2 OR 3)
C     OUTPUT
C        VKPG    COORDINATES OF GAUSS POINTS
C        VCPG    WEIGHTS AT GAUSS POINTS
C        IPG     TOTAL NUMBER OF GAUSS POINTS
C====================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IPGKED(*),VKPG(*),VCPG(*),G(10),P(10),INDIC(4)
      DATA INDIC/1,2,4,7/
      DATA G/0.0D0,-.577350269189626D0,.577350269189626D0,
     1       -.774596669241483D0,0.0D0,.774596669241483D0,
     2       -.861136311594050D0,-.339981043584860D0,
     3       .339981043584860D0,.861136311594050D0/
      DATA P/2.0D0,1.0D0,1.0D0,
     1      0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     2       .347854845137450D0,.652145154862550D0,
     3       .652145154862550D0,.347854845137450D0/
c-----------------------------------------------------------------------
      II=IPGKED(1)
      IMIN=INDIC(II)
      IMAX=IMIN+II-1
      IF(NDIM-2) 10,20,30
C-------  1 DIMENSION
10    IPG=0
      DO 15 I=IMIN,IMAX
      IPG=IPG+1
      VKPG(IPG)=G(I)
15    VCPG(IPG)=P(I)
      RETURN
C-------  2 DIMENSIONS
20    II=IPGKED(2)
      JMIN=INDIC(II)
      JMAX=JMIN+II-1
      IPG=0
      L=1
      DO 25 I=IMIN,IMAX
      DO 25 J=JMIN,JMAX
      IPG=IPG+1
      VKPG(L)=G(I)
      VKPG(L+1)=G(J)
      L=L+2
25    VCPG(IPG)=P(I)*P(J)
      RETURN
C-------  3 DIMENSIONS
30    II=IPGKED(2)
      JMIN=INDIC(II)
      JMAX=JMIN+II-1
      II=IPGKED(3)
      KMIN=INDIC(II)
      KMAX=KMIN+II-1
      IPG=0
      L=1
      DO 35 I=IMIN,IMAX
      DO 35 J=JMIN,JMAX
      DO 35 K=KMIN,KMAX
      IPG=IPG+1
      VKPG(L)=G(I)
      VKPG(L+1)=G(J)
      VKPG(L+2)=G(K)
      L=L+3
35    VCPG(IPG)=P(I)*P(J)*P(K)
      RETURN
      END