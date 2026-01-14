PROGRAM solve_poly
  USE solver
  IMPLICIT NONE

  REAL(KIND = dp), DIMENSION(:), ALLOCATABLE :: coefficients, x, V, eigV
  REAL(kind = dp), DIMENSION(:,:), ALLOCATABLE :: H, eigF
  REAL(KIND = dp) :: L, dx
  INTEGER :: N, N_in, i

  N = 513 ! grid points
  N_in = N - 2 ! grid points excluding boundaries

  ALLOCATE(x(N_in))
  ALLOCATE(V(N_in))
  ALLOCATE(H(N_in, N_in))

  CALL read_input("coefficients.txt", L, coefficients)

  ! grid spacing setup 
  L = L/2
  dx = (2.0_dp * L) / (N - 1)

  ! write out a grid from -L to +L
  DO i = 1, N_in
    x(i) = -L + i * dx
  END DO

  ! get the potential at every grid
  DO i = 1, N_in
    V(i) = build_poly(x(i),coefficients)
  END DO

  CALL build_hamiltonian(V, dx, H)

  CALL solve_eigenvalue(H, eigV, eigF)

  CALL write_result("training_data.csv", coefficients, eigV, N, 2*L)

END PROGRAM solve_poly