MODULE solver
  IMPLICIT NONE
  INTEGER, PARAMETER, PUBLIC :: dp = SELECTED_REAL_KIND (p = 13, r = 300) ! double precision parameter

  CONTAINS

  ! READ BOX SIZE AND POLYNOMIAL FROM FILE
  SUBROUTINE read_input(infile, L, coefficients)
    IMPLICIT NONE
    CHARACTER(LEN = *), INTENT(IN) :: infile
    REAL(KIND = dp), INTENT(OUT) :: L
    REAL(KIND = dp), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: coefficients
    INTEGER :: i, ios, n

    OPEN (UNIT = 11, FILE = infile, STATUS = "old", ACTION = "read")  

    n = 0 ! number of coefficients read

    READ(UNIT = 11, FMT = *) L ! read first number as L

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

    READ(UNIT = 11, FMT = *) L ! read first number as L

    ! assign coefficients to each member of the array
    DO i = 1, n
        READ(UNIT = 11, FMT = *) coefficients(i)
    END DO

    CLOSE(11)

    END SUBROUTINE read_input

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

    ! WRITE THE RESULTS TO A FILE
    SUBROUTINE write_result(filename, coefficients, eigenvalues, N, L)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: filename
      REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: coefficients, eigenvalues
      REAL(KIND=dp), INTENT(IN) :: L
      INTEGER, INTENT(IN) :: N

      INTEGER :: unit_out, i

      ! open the file 
      OPEN(NEWUNIT = unit_out, FILE = filename, &
      STATUS = 'OLD', POSITION = 'APPEND', ACTION = 'WRITE')

      WRITE(unit_out, '(ES20.10E3,A)', ADVANCE = 'NO') L, ','
      
      ! insert the calculated data
      DO i = 1, 5
        IF (i <= SIZE(coefficients)) THEN
      WRITE(unit_out, '(ES20.10E3,A)', ADVANCE = 'NO') coefficients(i), ','
        ELSE
      WRITE(unit_out, '(ES20.10E3,A)', ADVANCE = 'NO') 0.0_dp, ','
        END IF
      END DO

      DO i = 1, MIN(10, N)
        WRITE(unit_out, '(ES20.10E3)', ADVANCE = 'NO') eigenvalues(i)
        IF (i < MIN(10, N)) WRITE(unit_out, '(A)', ADVANCE='NO') ','
      END DO

      WRITE(unit_out, *)

      CLOSE(unit_out)
    END SUBROUTINE write_result

END MODULE solver