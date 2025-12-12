PROGRAM solve_poly
  USE solver
  IMPLICIT NONE

  CHARACTER(LEN = *), INTENT(IN) :: infile
  REAL(KIND = dp), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: coefficients

  CALL read_poly(infile, coefficients)

  PRINT *, coefficients

END PROGRAM solve_poly