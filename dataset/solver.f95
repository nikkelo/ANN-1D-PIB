MODULE solver
  IMPLICIT NONE
  INTEGER, PARAMETER, PUBLIC :: sp = SELECTED_REAL_KIND (p = 6, r = 37) ! single precision parameter
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

    ! allocate "coefficients" based on the number of coefficients in the file
    ALLOCATE(coefficients(n)) 

    ! assign coefficients to each member of the array
    DO i = 1, n
        READ(UNIT = 11, FMT = *) coefficients(i)
    END DO

    CLOSE(10)

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
        V(i) = build_poly(x(i), coefficients(i))
      END DO

    END SUBROUTINE build_potential

    ! BUILD HAMILTONIAN MATRIX
    ! SUBROUTINE build_hamiltonian()

    ! END SUBROUTINE build_hamiltonian

    ! SUBROUTINE solve_eigenproblem

    ! END SUBROUTINE solve_eigenproblem

END MODULE solver