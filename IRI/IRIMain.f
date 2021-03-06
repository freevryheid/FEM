C i686-w64-mingw32-gfortran -o IRIMain.exe -static IRIMain.f
C=======================================================================
C
C  This is a sample program for calculating IRI. It is meant to give
C  developers of profilers and profile analysis software a stipped-
C  down program for getting IRI. The program reads profile data
C  stored in a simple format known as ERD files.
C  The program has been tested on Microsoft Fortran for the PC and
C  Language Systems Fortran for the MAC.
C
C  For more information about the IRI and how this program works, see
C  Sayers, M. W., "On the Calcluation of International Roughness Index
C  from Longitudinal Road Profile." Transportation Research Record 1501,
C  (1995) p. 1-12.
C
C  See http://spyder.umtri.umich.edu/erd_soft/erd_file.html on the web
C  for a description of ERD files.
C
      PROGRAM CALCIRI

      LOGICAL        INSI, ISIT
      INTEGER        I, IOSTAT, J, KEYNUM, MAXCHN, MAXPNT, NCHAN, NS
      INTEGER        NSAMP
      PARAMETER      (MAXCHN = 2, MAXPNT = 60000)
      REAL*4         BASE, DATBUF(MAXPNT), PROF(MAXPNT), SCL, STEP
      REAL*4         XIRI(MAXCHN), UNITSC
      PARAMETER      (BASE = 0.25)
      CHARACTER*8    KEY, SHRT(MAXCHN), UNIRI, UNIT(MAXCHN), XUNIT
      CHARACTER*32   FNAME
      CHARACTER*80   BUFFER, STR

      WRITE(*,*) 'Test program for calculation of the IRI'
      WRITE(*,*)

C  Promt the user for a file name.
    1 WRITE(*,*) 'Enter a file name for profile data in ERD format.'
      READ(*,*) FNAME
      INQUIRE(FILE = FNAME, EXIST = ISIT)
      IF(.NOT.ISIT) THEN
        WRITE(*,*) 'Input file does not exist. Try again.'
        GO TO 1
      END IF
      OPEN(15, FILE = FNAME, STATUS = 'OLD')

C  Read the first 2 lines of the header and check for errors.
      READ(15, '(A12)') STR
      IF (STR .NE. 'ERDFILEV2.00')
     &    CALL WERROR('File is not ERD format.')

      READ(15, *) NCHAN, NSAMP, I, I, KEYNUM, STEP
      IF (KEYNUM .NE. 5)
     &    CALL WERROR('Data not stored in correct format.')
      IF (NCHAN .LT. 1 .OR. NCHAN .GT. MAXCHN)
     &    CALL WERROR('File must have 1 or 2 channels.')
      IF (NSAMP*NCHAN .GT. MAXPNT) NSAMP = MAXPNT/NCHAN
      IF (STEP .LE. 0.0) CALL WERROR('Illegal step size.')

C  Read the rest of the header, and look for XUNITS, FORMAT, UNITSNAM.
   10 READ(15, '(A80)', IOSTAT=IOSTAT) BUFFER
      IF (IOSTAT .NE. 0) BUFFER = 'END'
      KEY = BUFFER(1:8)
      BUFFER = BUFFER(9:)
      IF (KEY .EQ. 'END') THEN
        GOTO 20
      ELSE IF (KEY .EQ. 'XUNITS  ') THEN
        XUNIT = BUFFER(1:8)
      ELSE IF (KEY .EQ. 'UNITSNAM') THEN
        DO 30 I = 1, NCHAN
   30     UNIT(I) = BUFFER((I-1)*8+1:(I-1)*8+8)
      ELSE IF (KEY .EQ. 'SHORTNAM') THEN
        DO 40 I = 1, NCHAN
   40     SHRT(I) = BUFFER((I-1)*8+1:(I-1)*8+8)
      END IF
      GO TO 10

C  Echo file info to the screen.
   20 WRITE(*,*)
      WRITE(*,'(A21,F8.2,1X,A8)')
     &    ' Profile length     =', NSAMP*STEP, XUNIT
      WRITE(*,'(A21,I8)')
     &    ' Number of samples  =', NSAMP
      WRITE(*,'(A21,I8)')
     &    ' Number of channels =', NCHAN
      WRITE(*,'(A25,A8)')
     &    ' Units of elevation =    ', UNIT(1)
      WRITE(*,'(A25,A8)')
     &    ' Units of distance  =    ', XUNIT

C  Read the data.
      DO 50 J = 1, NSAMP
   50   READ(15, *, END = 60) (DATBUF((J-1)*NCHAN+I), I = 1, NCHAN)

C  Convert units of STEP to m.
      IF (XUNIT .EQ. 'ft' .OR. XUNIT .EQ. 'FT') THEN
        STEP = STEP*.3048
        INSI = .FALSE.
        UNITSC = 63360.
        UNIRI = ' in/mi'
      ELSE IF (XUNIT .EQ. 'm' .OR. XUNIT .EQ. 'M') THEN
        INSI = .TRUE.
        UNITSC = 1000.
        UNIRI = ' m/km'
      ENDIF
      IF (STEP .GE. 1.0) CALL WERROR('Step size too large.')

C  Analyze each profile.
      DO 70 I = 1, NCHAN
        NS = NSAMP
        IF (UNIT(I) .EQ. 'M' .OR. UNIT(I) .EQ. 'm') THEN
           SCL = 1.0
        ELSE IF (UNIT(I) .EQ. 'MM' .OR. UNIT(I) .EQ. 'mm') THEN
           SCL = 1./1000.
        ELSE IF (UNIT(I) .EQ. 'IN' .OR. UNIT(I) .EQ. 'in') THEN
           SCL = .0254
        ELSE
           CALL WERROR('Units name not recogized.')
        END IF
        DO 80 J = 1, NS
   80     PROF(J) = DATBUF((J-1)*NCHAN+I)*SCL
          CALL GETPI(PROF, NS, STEP, BASE, 1., XIRI(I), 11., 1.,
     &             653., 63.3, 6., .15)
   70   CONTINUE

C  Echo the IRI to the screen.
      DO 90 I = 1, NCHAN
        IF (SHRT(I) .EQ. ' ') THEN
          WRITE(SHRT(I),'(A5,I3)') 'Chan ',I
        ENDIF
        IF (INSI) THEN
          WRITE(STR,'(F8.2)') UNITSC*XIRI(I)
        ELSE
          WRITE(STR,'(F8.0)') UNITSC*XIRI(I)
        END IF
        WRITE(*,'(A8,A8,A5,A8,A6)')
     &            ' IRI of ', SHRT(I), '    =', STR, UNIRI
   90   CONTINUE
      IF (NCHAN .EQ. 2) THEN
        IF (INSI) THEN
          WRITE(STR,'(F8.2)') UNITSC*(XIRI(1) + XIRI(2))/2.
        ELSE
          WRITE(STR,'(F8.0)') UNITSC*(XIRI(1) + XIRI(2))/2.
        END IF
        WRITE(*,'(A21,A8,A6)') ' Average IRI        =', STR, UNIRI
      END IF
      STOP
   60 CALL WERROR('Wrong number of samples in data file.')
      END

C=======================================================================
      SUBROUTINE WERROR(STR)
C=======================================================================
C  This routine writes an error message and terminates the program.

      CHARACTER*(*) STR

      WRITE(*,*) 'ERROR: ', STR
      PAUSE
      STOP
      END
C=======================================================================
      SUBROUTINE GETPI(PROF, NSAMP, DX, BASE, UNITSC, PI, XLEAD, XEXP,
     &                 K1, K2, C, MU)
C=======================================================================
C Filter a longitudinal road profile and calculate PI.
C
C <-> PROF   Real     On input, an array of profile height values.
C                     On output, an array of filtered PI profile values.
C <-> NSAMP  Integer  Number of data samples in array PROF. The filtered
C                     profile has fewer points than the original.
C --> DX     Real     Distance step between profile points (m).
C --> BASE   Real     Distance covered by moving average (m).
C                     Use 0.250 for unfiltered profile input, and 0.0
C                     for pre-smoothed profiles (e.g. K.J. Law data).
C --> UNITSC Real     Product of two scale factors: (1) meters per unit
C                     of profile height, and (2) PI units of slope.
C                     Ex: height is inches, slope will be in/mi.
C                         UNITSC = (.0254 m/in)*(63360 in/mi) = 1069.34
C <-- PI     Real     The average PI for the entire profile.
C <-- XLEAD  Real     Initialization base length.
C <-- XEXP   Real     Power weighting (1. = ARS, 2. = RMS).
C <-- K1, K2, C, MU   Filter coefficients.

      INTEGER   I, IBASE, ILEAD, NSAMP
      REAL      AMAT, BASE, BMAT, C, CMAT, DX, K1, K2, MU, PI, PR
      REAL      PROF, SFPI, ST, UNITSC, V, XEXP, XIN, XLEAD
      DIMENSION AMAT(4, 4), BMAT(4), CMAT(4), PR(4), PROF(NSAMP),
     &          ST(4,4), XIN(4)

C  Set parameters and arrays.
      CALL SETABC(K1, K2, C, MU, AMAT, BMAT, CMAT)
      CALL SETSTM(DX/(80./3.6), AMAT, BMAT, ST, PR)
      IBASE = MAX(NINT(BASE/DX), 1)
      SFPI = UNITSC/(DX*IBASE)

C  Initialize simulation variables based on profile start.
      ILEAD = MIN(NINT(XLEAD/DX) + 1, NSAMP)
      XIN(1) = UNITSC*(PROF(ILEAD) - PROF(1))/(DX*ILEAD)
      XIN(2) = 0.0
      XIN(3) = XIN(1)
      XIN(4) = 0.0

C  Convert to averaged slope profile, with PI units.
      NSAMP = NSAMP - IBASE
      DO 10 I = 1, NSAMP
   10   PROF(I) = SFPI*(PROF(I + IBASE) - PROF(I))

C  Filter profile.
      CALL STFILT(PROF, NSAMP, ST, PR, CMAT, XIN)

C  Compute PI from filtered profile.
      PI = 0.0
      DO 20 I = 1, NSAMP
   20   PI = PI + ABS(PROF(I))**XEXP
      PI = (PI/NSAMP)**(1./XEXP)
      RETURN
      END
C=======================================================================
      SUBROUTINE SETABC(K1, K2, C, MU, AMAT, BMAT, CMAT)
C=======================================================================
C Set the A, B and C matrices for the 1/4 car model.
C
C -->  K1    REAL   Kt/Ms = normalized tire spring rate (1/s/s)
C -->  K2    REAL   Ks/Ms = normalized suspension spring rate (1/s/s)
C -->  C     REAL   C/Ms  = normalized suspension damper rate (1/s)
C -->  MU    REAL   Mu/Ms = normalized unsprung mass (-)
C <--  AMAT  REAL   The 4x4 A matrix.
C <--  BMAT  REAL   The 4x1 B matrix.
C <--  CMAT  REAL   The 4x1 C matrix.

      INTEGER       I, J
      REAL          AMAT, BMAT, CMAT, K1, K2, C, MU
      DIMENSION     AMAT(4, 4), BMAT(4), CMAT(4)

C  Set default for all matrix elements to zero.
      DO 10 J = 1, 4
        BMAT(J) = 0
        CMAT(J) = 0
        DO 10 I = 1, 4
   10     AMAT(I, J) = 0

C  Put 1/4 car model parameters into the A Matrix.
      AMAT(1, 2) = 1.
      AMAT(3, 4) = 1.
      AMAT(2, 1) = -K2
      AMAT(2, 2) = -C
      AMAT(2, 3) = K2
      AMAT(2, 4) = C
      AMAT(4, 1) = K2/MU
      AMAT(4, 2) = C/MU
      AMAT(4, 3) = -(K1 + K2)/MU
      AMAT(4, 4) = -C/MU

C  Set the B matrix for road input through tire spring.
      BMAT(4) = K1/MU

C  Set the C matrix to use suspension motion as output.
      CMAT(1) = -1
      CMAT(3) = 1
      RETURN
      END
C=======================================================================
      SUBROUTINE SETSTM(DT, A, B, ST, PR)
C=======================================================================
C Compute ST and PR arrays. This requires INVERT for matrix inversion.
C
C -->  DT    REAL   Time step (sec)
C -->  A     REAL   The 4x4 A matrix.
C -->  B     REAL   The 4x1 B matrix.
C <--  ST    REAL   4x4 state transition matrix.
C <--  PR    REAL   4x1 partial response vector.

      INTEGER   I, ITER, J, K
      LOGICAL   MORE
      REAL      A, A1, A2, B, DT, PR, ST, TEMP
      DIMENSION A(4, 4), A1(4, 4), A2(4, 4), B(4), PR(4), ST(4, 4),
     &          TEMP(4, 4)

      DO 20 J = 1, 4
        DO 10 I = 1, 4
          A1(I, J) = 0
   10     ST(I, J) = 0
        A1(J, J) = 1.
   20   ST(J, J) = 1.

C  Calculate the state transition matrix ST = exp(dt*A) with a Taylor
C  series. A1 is the previous term in the series, A2 is the next one.
      ITER = 0
   30 ITER = ITER + 1
      MORE = .FALSE.
      DO 40 J = 1, 4
        DO 40 I = 1, 4
          A2(I, J) = 0
          DO 40 K = 1, 4
   40       A2(I, J) = A2(I, J) + A1(I, K)*A(K, J)
      DO 50 J = 1, 4
        DO 50 I = 1, 4
          A1(I, J) = A2(I, J)*DT/ITER
          IF (ST(I, J) + A1(I, J) .NE. ST(I, J)) MORE = .TRUE.
   50     ST(I, J) = ST(I, J) + A1(I, J)
      IF (MORE) GO TO 30

C  Calculate particular response matrix: PR = A**-1*(ST-I)*B
      CALL INVERT(A, 4)
      DO 60 I = 1, 4
        PR(I) = 0.0
        DO 60 K = 1, 4
   60     PR(I) = PR(I) - A(I, K)*B(K)
      DO 90 J = 1, 4
        DO 70 I = 1, 4
          TEMP(J, I) = 0.0
          DO 70 K = 1, 4
   70       TEMP(J, I) = TEMP(J, I) + A(J, K)*ST(K, I)
        DO 80 K = 1, 4
   80     PR(J) = PR(J) + TEMP(J, K)*B(K)
   90   CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE STFILT(PROF, NSAMP, ST, PR, C, XIN)
C=======================================================================
C Filter profile using matrices ST, PR, and C.
C
C <->  PROF   REAL      Input profile. Replaced by the output.
C -->  NSAMP  INTEGER   Number of data values in array PROF.
C -->  ST     REAL      4x4 state transition matrix.
C -->  PR     REAL      4x1 partial response vector.
C -->  C      REAL      4x1 output definition vector.
C -->  XIN    REAL      4x1 vector of initial values of state variables.

      INTEGER   I, J, K, NSAMP
      REAL      C, PR, PROF, ST, X, XIN, XN
      DIMENSION C(4), PR(4), PROF(NSAMP), ST(4, 4), X(4), XIN(4), XN(4)

C  Initialize simulation variables.
      DO 10 I = 1, 4
   10   X(I) = XIN(I)

C  Filter profile using the state transition algorithm.
      DO 40 I = 1, NSAMP
        DO 20 J = 1, 4
          XN(J) = PR(J)*PROF(I)
          DO 20 K = 1, 4
   20       XN(J) = XN(J) + X(K)*ST(J, K)
        DO 30 J = 1, 4
   30     X(J) = XN(J)
        PROF(I) = X(1)*C(1) + X(2)*C(2) + X(3)*C(3) + X(4)*C(4)
   40   CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE INVERT(Y1, N)
C=======================================================================
C  This routine will store the inverse of NxN matrix Y1 in matrix YINV.
C  It was copied from "Numerical Recipes."
C
C  Y1   --> Real     The matrix to be inverted.
C  YINV --> Real     The inverse of matrix Y1.
C
      INTEGER      N, INDX, I, J
      REAL*4       Y1, YINV, D, A
      DIMENSION    Y1(N, N), YINV(4, 4), INDX(4), A(4, 4)

      DO 8 I = 1, N
        DO 9 J = 1, N
    9     A(I, J) = Y1(I, J)
    8   CONTINUE
      DO 10 I = 1, N
        DO 20 J = 1, N
   20     YINV(I, J) = 0.0
        YINV(I, I) = 1.0
   10   CONTINUE
      CALL LUDCMP(A, INDX, D)
      DO 30 J = 1, N
   30   CALL LUBKSB(A, INDX, YINV(1, J))
      DO 40 I = 1, N
        DO 50 J = 1, N
   50     Y1(I ,J) = YINV(I, J)
   40   CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE LUDCMP(A, INDX, D)
C=======================================================================
C  This routine was copied from "Numerical Recipes" for matrix
C  inversion.
C
      INTEGER      N, INDX, NMAX, I, J, IMAX
      REAL*4       A, TINY, VV, D, AAMAX, SUM, DUM
      PARAMETER    (NMAX = 100, TINY = 1.0E-20, N = 4)
      DIMENSION    A(N, N), INDX(N), VV(NMAX)

      D = 1.0
      DO 10 I = 1, N
        AAMAX = 0.0
        DO 20 J = 1, N
   20     IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
        IF(AAMAX.EQ.0.0) PAUSE 'Singular matrix'
        VV(I) = 1.0/AAMAX
   10   CONTINUE
      DO 30 J = 1, N
        DO 40 I = 1, J-1
          SUM = A(I, J)
          DO 50 K = 1, I-1
   50       SUM = SUM - A(I, K)*A(K, J)
          A(I, J) = SUM
   40     CONTINUE
        AAMAX = 0.0
        DO 60 I = J, N
          SUM = A(I, J)
          DO 70 K = 1, J-1
   70       SUM = SUM - A(I, K)*A(K, J)
          A(I, J) = SUM
          DUM = VV(I)*ABS(SUM)
          IF(DUM.GE.AAMAX)THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
   60     CONTINUE
        IF(J.NE.IMAX)THEN
          DO 80 K = 1, N
            DUM = A(IMAX, K)
            A(IMAX, K) = A(J, K)
            A(J, K) = DUM
   80       CONTINUE
          D = -D
          VV(IMAX) = VV(J)
        ENDIF
        INDX(J) = IMAX
        IF(A(J, J).EQ.0.0) A(J, J) = TINY
        IF(J.NE.N)THEN
          DUM = 1.0/A(J, J)
          DO 90 I = J+1, N
   90       A(I, J) = A(I, J)*DUM
        ENDIF
   30   CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE LUBKSB(A, INDX, B)
C=======================================================================
C  This routine was copied from "Numerical Recipes" for matrix
C  inversion.

      INTEGER      N, INDX, I, II, LL
      REAL*4       A, B, SUM
      PARAMETER    (N = 4)
      DIMENSION    A(N, N), INDX(N), B(N)

      II = 0
      DO 10 I = 1, N
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IF(II.NE.0)THEN
          DO 20 J = II, I-1
   20       SUM = SUM - A(I, J)*B(J)
        ELSEIF(SUM.NE.0)THEN
          II = I
        ENDIF
        B(I) = SUM
   10   CONTINUE
      DO 30 I = N, 1, -1
        SUM = B(I)
        IF(I.LT.N)THEN
          DO 40 J = I+1, N
   40       SUM = SUM - A(I, J)*B(J)
        ENDIF
        B(I) = SUM/A(I, I)
   30   CONTINUE
      RETURN
      END
