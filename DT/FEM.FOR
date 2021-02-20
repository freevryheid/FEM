      PROGRAM FEM
C
C=======================================================================
C     PG 378
C=======================================================================
C
C     F . E . M .  - 3 -  PROGRAM,  'BOOK'  VERSION   OCTOBER 1979
C    ( G.TOUZOT , G.DHATT , COMPIEGNE UNIVERSITY OF TECHNOLOGY, FRANCE)
C     MAIN PROGRAM
C
C=======================================================================
      
      USE IFPORT
      
      IMPLICIT REAL*8(A-H,O-Z)
      

      REAL*8 elapsed_time     
      
      CHARACTER*12 DATF,OUTF,F3,F4,F5
      REAL*4 BLOC,BLOCS
      COMMON/ALLOC/NVA,IVA,IVAMAX,NREEL,NTBL
      COMMON/ES/M,MR,MP,MLUN(10)
      COMMON VA(1000000)
      DIMENSION BLOCS(21)
      DATA BLOCS/4HIMAG,4HCOMT,4HCOOR,4HDLPN,4HCOND,4HPRND,4HPREL,
     1           4HELEM,4HSOLC,4HSOLR,4HLINM,4HLIND,4HNLIN,4HTEMP,
     2           4HVALP,4H....,4H....,4H....,4H....,4H....,4HSTOP/
      DATA NB/21/
      DATA MR/5/,MP/6/
      DATA MLUN/0,12,13,14,15,16,17,18,19,20/
C--------------------------------------------------------------------------------
C

      PRINT*,'====================='
      PRINT*,'FEM - ADFS - APR 2005'
      PRINT*,'====================='
      

C-------- OPEN INPUT AND OUTPUT FILES
C
      F3 = 'F3.TMP'
      F4 = 'F4.TMP'
      F5 = 'F5.TMP'
      WRITE(*,931)
 931  FORMAT(' ENTER NAME OF DATA FILE.-')
      READ(*,932) DATF
 932  FORMAT(A)
      WRITE(*,933)
 933  FORMAT(' ENTER NAME OF OUTPUT FILE.-')
      READ(*,932) OUTF
      OPEN(MR,FILE=DATF)
      OPEN(MP,FILE=OUTF)
      OPEN(MLUN(2),FORM='UNFORMATTED')
      OPEN(MLUN(3),FILE=F3,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(MLUN(4),FILE=F4,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(MLUN(5),FILE=F5,STATUS='UNKNOWN',FORM='UNFORMATTED')
      DO LUNO = 6,10
        OPEN(MLUN(LUNO),FORM='UNFORMATTED')
      END DO
C-----------------------------------------------------------------------

      elapsed_time = TIMEF( )





C-------  LENGTH OF BLANK COMMON IN REAL WORDS (TABLE VA)
      NVA=1000000
C-------  HEADING
      WRITE(MP,2000)
2000  FORMAT(1H1,'  F.E.M.3.'/,'   G.TOUZOT , G.DHATT (MODIFIED BY A.D.F
     1.SMIT)'/3X,43('=')//)
C------- READ BLOCK TITLE
CWWB10    READ(MR,1000) BLOC,M,MLUN
10    READ(MR,1000) BLOC,M
1000  FORMAT(A4,I6,10I5)
C------- SEARCH FOR THE BLOCK TO BE EXECUTED
      DO 20 I=1,NB
      IF(BLOC.EQ.BLOCS(I)) GO TO 30
20    CONTINUE
      WRITE(MP,2010)
2010  FORMAT(' ** ERROR, MISSING BLOCK CALLING CARD')
      GO TO 10
30    GO TO (110,120,130,140,150,160,170,
     1 180,190,200,210,220,230,240,
     2 250,260,270,280,290,300,999),I
C------- BLOCK TO PRINT IMAGES OF DATA CARDS               'IMAG'
110   CALL BLIMAG
      GO TO 10
C------- BLOCK TO READ AND PRINT COMMENTS                  'COMT'
120   CALL BLCOMT
      GO TO 10
C------- BLOCK TO READ NODAL POINTS COORDINATES            'COOR'
130   CALL BLCOOR
      GO TO 10
C------- BLOCK TO READ DEGREES OF FREEDOM PER NODE         'DLPN'
140   CALL BLDLPN
      GO TO 10
C------- BLOCK T0 READ BOUNDARY CONDITIONS                 'COND'
150   CALL BLCOND
      GO TO 10
C------- BLOCK TO READ NODAL PROPERTIES                    'PRND'
160   CALL BLPRND
      GO TO 10
C------- BLOCK T0 READ ELEMENT PROPERTIES                  'PREL'
170   CALL BLPREL
      GO TO 10
C------- BLOCK TO READ ELEMENT DATA                        'ELEM'
180   CALL BLELEM
      GO TO 10
C------- ELOCK TO READ CONCENTRATED LOADS                  'SOLC'
190   CALL BLSOLC
      GO TO 10
C------- BLOCK TO READ DISTRIBUTED LOADS                   'SOLR'
200   CALL BLSOLR
      GO TO 10
C------- BLOCK FOR IN CORE ASSEMBLING AND LINEAR SOLUTION 'LINM'
210   CALL BLLINM
      GO TO 10
C------- BLOCK FOR ON DISK ASSEMBLING AND LINEAR SOLUTION 'LIND'
220   CALL BLLIND
      GO TO 10
C------- BLOCK FOR NON LINEAR PROBLEM SOLUTION             'NLIN'
230   CALL BLNLIN
      GO TO 10
C------- BLOCK FOR UNSTEADY PROBLEM                        'TEMP'
240   CALL BLTEMP
      GO TO 10
C------- BLOCK TO COMPUTE EIGENVALUES (SUBSPACE)           'VALP'
250   CALL BLVALP
      GO TO 10
C------- UNDEFINED BLOCKS
260   CONTINUE
270   CONTINUE
280   CONTINUE
290   CONTINUE
300   CONTINUE
      GO TO 10
C------- END OF PROBLEM                                    'STOP'
999   WRITE(MP,2020) IVAMAX,NVA
2020  FORMAT(//' END OF PROBLEM, ',I10,' UTILIZED REAL WORDS OVER ',I10)
      WRITE(*,2020) IVAMAX,NVA
      PRINT*, REAL(IVAMAX)/REAL(NVA)*100, '%'
C
C---- CLOSE INPUT AND OUTPUT FILES
C
      CLOSE(UNIT=MP)
      CLOSE(UNIT=MR)
      DO LUNC = 2,10
        CLOSE(UNIT=MLUN(LUNC))
      END DO

      elapsed_time = TIMEF( )
      PRINT *, 'ELAPSED TIME (SEC): ',elapsed_time
      PRINT*,'====================='
      STOP
      END
