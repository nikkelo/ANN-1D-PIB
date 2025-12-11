PROGRAM generate_data
  IMPLICIT NONE
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p = 13, r = 300)

  REAL (KIND = wp) :: hbar, m, L ! constant parameters (hbar, mass, length of box)
  INTEGER eigenstates ! number of eigenstates to solve for (starting with n = 1 for the ground state) 
  REAL (KIND = wp) :: n_data ! number of potentials to generate and solve for
  REAL (KIND = WP) :: dx ! grid spacing
  REAL (KIND = wp), DIMENSION(:), ALLOCATABLE :: x, V, eigenvalues ! array of points for x-position, array of potentials and array of eigenvalues
  REAL (KIND = wp), DIMENSION(:,:), ALLOCATABLE :: H, eigenvectors ! N x N Hamiltonian array, N x eigenstates eigenvector array
  INTEGER :: i, j, N ! loop counters, grid points

  ! set number of eigenstates to solve for and grid points
  eigenstates = 4
  N = 128
  ALLOCATE(x(N), V(N), H(N,N), eigenvalues(eigenstates), eigenvectors(N,eigenstates)) 

  ! set the sizes of hbar, m and L
  hbar = 1.0_wp
  m = 1.0_wp
  L = 10.0_wp

  ! define the grid spacing
  dx = L / (N - 1.0_wp)

	! define the x parameters on the grid
	DO i = 1, N 
		x(i) = (i - 1.0_wp) * dx
	END DO

	! define the discrete version of the Hamiltonian
	H = 0.0_wp

	! loop to build the Hamiltonian
    DO i = 2, N-1
      H(i,i)   = (hbar**2_wp / (m*dx**2_wp)) + V(i)
      H(i,i - 1_wp) = -0.5_wp * (hbar**2_wp / (m*dx**2_wp))
      H(i - 1_wp,i) = H(i,i - 1_wp)
    END DO

    ! define boundry conditions at H(1,1) and H(N,N), the limits of the box
    H(1,1) = 0.0_wp
    H(N,N) = 0.0_wp 


	PRINT *, H

END PROGRAM

SUBROUTINE normalize()
	IMPLICIT NONE
	
END SUBROUTINE  