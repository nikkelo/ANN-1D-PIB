PROGRAM generate_data
	IMPLICIT NONE
	INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)

	REAL (KIND = wp) :: hbar, m, L ! constant parameters (hbar, mass, length of box)
	REAL (KIND = wp) :: n_eigen ! number of eigenstates to solve for (starting with n = 1 for the ground state) 
 	REAL (KIND = wp) :: n_data ! number of potentials to generate and solve for
 	REAL (KIND = wp) :: grid ! define how the grid is discretized 

END PROGRAM

SUBROUTINE normalize()
	IMPLICIT NONE
	
END SUBROUTINE  