```fortran
PROGRAM DIFFERENTIAL_EQUATION_SOLVER

IMPLICIT NONE

! Declare constants
REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679

! Declare variables
REAL :: x, y, dx, dy, t, dt, h
INTEGER :: i, n

! Get input from the user
WRITE (*, *) 'Enter the initial position (x0, y0):'
READ (*, *) x, y
WRITE (*, *) 'Enter the time step (dt):'
READ (*, *) dt
WRITE (*, *) 'Enter the number of time steps (n):'
READ (*, *) n

! Calculate the time step size
h = dt / n

! Initialize the time
t = 0.0

! Initialize the output file
OPEN (UNIT=10, FILE='output.txt')

! Write the header to the output file
WRITE (10, *) 'x', 'y', 't'

! Loop over the time steps
DO i = 1, n

    ! Calculate the derivatives
    dx = y
    dy = -x

    ! Calculate the next position
    x = x + h * dx
    y = y + h * dy

    ! Increment the time
    t = t + dt

    ! Write the results to the output file
    WRITE (10, *) x, y, t

END DO

! Close the output file
CLOSE (10)

END PROGRAM DIFFERENTIAL_EQUATION_SOLVER
```

This code solves the differential equation system:

```
dx/dt = y
dy/dt = -x
```

using the forward Euler method. The code takes the initial position (x0, y0), the time step (dt), and the number of time steps (n) as input from the user. It then calculates the derivatives, the next position, and the time for each time step. The results are written to an output file.

The code uses the following variables:

* `x`: The x-coordinate of the position.
* `y`: The y-coordinate of the position.
* `t`: The time.
* `dx`: The derivative of x with respect to time.
* `dy`: The derivative of y with respect to time.
* `dt`: The time step.
* `h`: The time step size.
* `i`: The loop counter.
* `n`: The number of time steps.

The code uses the following constants:

* `PI`: The value of pi.