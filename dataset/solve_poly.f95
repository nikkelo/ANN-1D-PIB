PROGRAM solve_poly
  USE solver
  IMPLICIT NONE

  REAL(KIND = dp), DIMENSION(:), ALLOCATABLE :: coefficients, x, V, eigV
  REAL(kind = dp), DIMENSION(:,:), ALLOCATABLE :: H, eigF
  REAL(KIND = dp) :: L, dx
  INTEGER :: N, i

  N = 512 ! grid points

  ALLOCATE(x(N))
  ALLOCATE(V(N))
  ALLOCATE(H(N, N))

  CALL read_input("coefficients.txt", L, coefficients)

  ! grid spacing setup 
  L = L/2
  dx = (2.0_dp * L) / (N - 1)

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

  CALL write_result("training_data.csv", coefficients, eigV, N, 2*L)

END PROGRAM solve_poly