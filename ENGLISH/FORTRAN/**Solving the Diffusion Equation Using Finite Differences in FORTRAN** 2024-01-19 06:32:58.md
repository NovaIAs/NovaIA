```fortran
PROGRAM DIFFUSIVE_WAVE_EQUATION
! This program solves the one-dimensional diffusion equation using finite differences.

! Declare the parameters and variables.
INTEGER, PARAMETER :: NX = 100 ! Number of grid points in the x-direction
INTEGER, PARAMETER :: NT = 1000 ! Number of time steps
REAL(KIND=8) :: DX = 1.0 ! Grid spacing in the x-direction
REAL(KIND=8) :: DT = 0.001 ! Time step
REAL(KIND=8), DIMENSION(NX) :: U ! Solution vector
REAL(KIND=8), DIMENSION(NX) :: U_OLD ! Previous solution vector
REAL(KIND=8) :: D = 1.0 ! Diffusion coefficient

! Initialize the solution vector.
U = 0.0

! Set the boundary conditions.
U(1) = 1.0
U(NX) = 0.0

! Time loop.
DO NT = 1, NT

  ! Copy the solution vector to the previous solution vector.
  U_OLD = U

  ! Calculate the new solution vector.
  DO IX = 1, NX
    U(IX) = U_OLD(IX) + DT * D * ((U_OLD(IX+1) - 2.0*U_OLD(IX) + U_OLD(IX-1)) / DX**2)
  END DO

END DO

! Plot the solution.
CALL PLOT(X, U)

END PROGRAM DIFFUSIVE_WAVE_EQUATION
```

This program solves the one-dimensional diffusion equation using finite differences. The diffusion equation is a partial differential equation that describes the flow of heat or mass through a medium.

The program first declares the parameters and variables. The parameters are the number of grid points in the x-direction (NX), the number of time steps (NT), the grid spacing in the x-direction (DX), the time step (DT), and the diffusion coefficient (D). The variables are the solution vector (U), the previous solution vector (U_OLD), and the time (NT).

The program then initializes the solution vector to zero.

Next, the program sets the boundary conditions. The boundary conditions are the values of the solution at the boundaries of the domain. In this case, the boundary conditions are that the solution is equal to 1.0 at the left boundary and 0.0 at the right boundary.

The program then enters the time loop. In the time loop, the program first copies the solution vector to the previous solution vector. Then, the program calculates the new solution vector using the finite difference method. The finite difference method is a numerical method for solving partial differential equations.

After the program has calculated the new solution vector, it plots the solution.

The output of the program is a plot of the solution to the diffusion equation. The plot shows the solution as a function of time and space.