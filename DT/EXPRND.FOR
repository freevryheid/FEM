C
      SUBROUTINE EXPRND(VPRNG)
C===================================================================
C     pg 392
C===================================================================
C     TO EXECUTE BLOCK 'PRND'
C     READ NODAL PROPERTIES
C===================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/COOR/NDIM,NNT
      COMMON/PRND/NPRN
      COMMON/ES/M,MR,MP,M1
      DIMENSION VPRNG(*)
C-------------------------------------------------------------------
C-------  READ PROPERTIES NODEWISE
      I1=NNT*NPRN
      READ(M1,1000)(VPRNG(I),I=1,I1)
1000  FORMAT(8F10.0)
      IF(M.GE.0) WRITE(MP,2000) (VPRNG(I),I=1,I1)
2000  FORMAT(//' CARDS OF NODAL PROPERTIES'/ (' >>>>>',8E12.5))
      RETURN
      END
