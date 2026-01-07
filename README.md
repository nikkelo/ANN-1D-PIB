# ANN-1D-PIB

**ANN-1D-PIB** is a ANN model for solving the Schrodinger equation for a 1D particle in a box given a polynomial function of the potential. The model takes the coefficients of the polynomial and outputs the first 5 eigenvalues. 

The dataset is based on a numerical solution implemented in Fotran, based on the finite difference approximation. The code makes use of LAPACK's DSYEV subroutine for computing the eigenvalues of the Hamiltonian tridiagonal matrix, and so LAPACK is a dependency required to re-compute data if necessary. It is not necessary to use the ANN, however.

This project was built in 2025/2026 as part of the CDDC (Complex & Data-Driven Chemistry) degree at Universita degli Studi di Padova, as an assignment for the courses Machine Learning for Chemistry and Coding in Chemistry.  

## Setup & Pre-Requisites

The **numerical solver** has only two pre-requisites: 
- A Fortran compiler
- BLAS for the DYSEV subroutine used to solve the eigenproblem

**You do not need the numerical solver if you are only trying to run the neural network.** By using the default dataset included in the repository when the Python script asks you for which data to use, you can run the Python section without having any of the Fortran pre-requisites.

The **artificial neural network** has the following pre-requisites:
- Python
- Pandas library
- Scikit-learn library
- Scipy library
- Numpy library

To use the neural network, compile the **ANN-1D-PIB.py** file in the root folder and follow the instructions given in the command line interface. 
+ Press Enter to use the default dataset included in the repository. Alternatively, press Y to create your own dataset. 
    + If you opt to create your own dataset, you'll be asked for **L_max** (maximum size of your box), **A** (the parameter used to vary the coefficients of the V(x) polynomial) and **N** (the number of samples to generate). If you're not sure about any of these parameters, pressing Enter will use the default values.
+ After selecting your dataset, you have a choice between 3 models:
    + Type NN for the Artificial Neural Network
    + Type RF for the Random Forest 
    + Type RR for the Ridge Regression
+ Upon selecting the model, the program will use the database to train the model and then output a result in the form of an R2 score and MSE. 

## Numerical Solver

The numerical solver used to create a training dataset is built in Fortran. All of the core subroutines used to compute the solutions of the Schrodinger equation are given in the **solver.f95** module. Those subroutines are outlined and explained below:
- 