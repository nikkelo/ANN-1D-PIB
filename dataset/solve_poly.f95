PROGRAM solve_poly
  USE solver
  IMPLICIT NONE

  REAL(KIND = dp), DIMENSION(:), ALLOCATABLE :: coefficients, x, V, eigV
  REAL(kind = dp), DIMENSION(:,:), ALLOCATABLE :: H, eigF
  REAL(KIND = dp) :: L, dx
  INTEGER :: N, i

  CALL read_poly("test_coefficients.txt", coefficients)
  PRINT *, "Polynomial coefficients:", coefficients

  ! define grid parameters
  N = 512 ! grid points
  L = 0.5_dp ! box size
  dx = (2.0_dp * L)/(N - 1) ! grid space

  ALLOCATE(x(N))
  ALLOCATE(V(N))
  ALLOCATE(H(N, N))

  ! write out a grid from -L to +L
  DO i = 1, N
    x(i) = -L + (i - 1) * dx
  END DO

  ! get the potential at every grid
  DO i = 1, N
    V(i) = build_poly(x(i),coefficients)
  END DO

  CALL build_hamiltonian(V, dx, H)

  CALL solve_eigenvalue(H, eigV, eigF)

  ! print first 10 eigenvalues
  DO i = 1, MIN(10, N)
    PRINT *, "E", i, " = ", eigV(i)
  END DO

  DEALLOCATE(x, V, H, eigV, eigF, coefficients)

END PROGRAM solve_poly