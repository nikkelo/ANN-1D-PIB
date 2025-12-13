# ANN-1D-PIB
**ANN-1D-PIB** is a ANN model for solving the Schrodinger equation for a 1D particle in a box given a polynomial function of the potential. The model takes the coefficients of the polynomial and outputs the first 5 eigenvalues. 

The dataset is based on a numerical solution implemented in Fotran, based on the finite difference approximation. The code makes use of LAPACK's DSYEV subroutine for computing the eigenvalues of the Hamiltonian tridiagonal matrix, and so LAPACK is a dependency required to re-compute data if necessary. It is not necessary to use the ANN, however.

This project was built in 2025/2026 as part of the CDDC (Complex & Data-Driven Chemistry) degree at Universita degli Studi di Padova, as an assignment for the courses Machine Learning for Chemistry and Coding in Chemistry.  
