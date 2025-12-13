MODULE solver
  IMPLICIT NONE
  INTEGER, PARAMETER, PUBLIC :: dp = SELECTED_REAL_KIND (p = 13, r = 300) ! double precision parameter

  CONTAINS

  ! READ POLYNOMIAL FROM FILE
  SUBROUTINE read_poly(infile, coefficients)
    IMPLICIT NONE
    CHARACTER(LEN = *), INTENT(IN) :: infile
    REAL(KIND = dp), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: coefficients
    INTEGER :: i, ios, n

    OPEN (UNIT = 11, FILE = infile, STATUS = "old", ACTION = "read")  

    n = 0 ! number of coefficients read

    ! read the number of coefficients
    DO
        READ(UNIT = 11, FMT = *, IOSTAT = ios)
        IF (ios /= 0) EXIT
        n = n + 1
    END DO

    CLOSE(11)

    ! allocate "coefficients" based on the number of coefficients in the file
    ALLOCATE(coefficients(n)) 

    OPEN (UNIT = 11, FILE = infile, STATUS = "old", ACTION = "read")

    ! assign coefficients to each member of the array
    DO i = 1, n
        READ(UNIT = 11, FMT = *) coefficients(i)
    END DO

    CLOSE(11)

    END SUBROUTINE read_poly

    ! BUILD POLYNOMIAL FROM READ FILE AT POINT X ON THE CURVE
    FUNCTION build_poly(x, coefficients)
      IMPLICIT NONE
      REAL(KIND = dp), INTENT(IN) :: x
      REAL(KIND = dp), DIMENSION(:), INTENT(IN) :: coefficients
      REAL(KIND = dp) :: build_poly
      INTEGER :: i

      ! start building from first coefficient
      build_poly = coefficients(1) 

      ! from the second coefficient onward, use Horner's method
      DO i = 2, SIZE(coefficients)
        build_poly = build_poly * x + coefficients(i)
      END DO

    END FUNCTION build_poly

    ! BUILD POTENTIALS FOR EVERY POINT X ON THE CURVE
    SUBROUTINE build_potential(x, coefficients, V)
      IMPLICIT NONE
      REAL(KIND = dp), DIMENSION(:), INTENT(IN) :: x, coefficients
      REAL(KIND = dp), DIMENSION(:), INTENT(OUT) :: V
      INTEGER :: i

      DO i = 1, SIZE(coefficients)
        V(i) = build_poly(x(i), coefficients)
      END DO

    END SUBROUTINE build_potential

    ! BUILD HAMILTONIAN MATRIX
    SUBROUTINE build_hamiltonian(V, dx, H)
      IMPLICIT NONE 
      REAL(KIND = dp), DIMENSION(:), INTENT(IN) :: V
      REAL(KIND = dp), INTENT (IN) :: dx 
      REAL(KIND = dp), DIMENSION(:,:), INTENT(OUT) :: H 
      INTEGER i, N 

      N = SIZE(V) ! grid size
      H = 0.0_dp

      DO i = 1, N 
        H(i,i) = 1.0_dp / (dx**2) + V(i) ! diagonal elements

        IF (i < N) THEN ! off-diagonal elements 
          H(i, i+1) = -0.5_dp / (dx**2)
          H(i+1, i) = -0.5_dp / (dx**2)
        END IF
      END DO  

    END SUBROUTINE build_hamiltonian

    ! SOLVE FOR THE EIGENVALUES AND EIGENVECTORS USING DYSEV (LAPACK)
    SUBROUTINE solve_eigenvalue(H, eigenvalues, eigenvectors)
      IMPLICIT NONE 
      REAL(KIND = dp), DIMENSION(:,:), INTENT(IN) :: H 
      REAL(KIND = dp), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: eigenvalues
      REAL(KIND = dp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: eigenvectors
      INTEGER :: N, LWORK
      REAL(KIND = dp), DIMENSION(:), ALLOCATABLE :: WORK
      INTEGER :: INFO

      N = SIZE(H, 1)

      ALLOCATE(eigenvalues(N))
      ALLOCATE(eigenvectors(N, N))

      eigenvectors = H 

      ! allocate minimum workload size for DYSEV
      LWORK = 3*N - 1
      ALLOCATE(WORK(LWORK))
      CALL DSYEV('V', 'U', N, eigenvectors, N, eigenvalues, WORK, LWORK, INFO)

    END SUBROUTINE solve_eigenvalue

END MODULE solver