```fortran
PROGRAM TRIANGULAR_MATRIX
! This program solves a system of linear equations with a triangular matrix.
! The matrix is input in a text file, and the solution is output to a text file.

! Declare the variables.
INTEGER, PARAMETER :: N = 3  ! The size of the matrix.
REAL :: A(N,N)               ! The matrix.
REAL :: b(N)                 ! The right-hand side vector.
REAL :: x(N)                 ! The solution vector.
INTEGER :: i, j              ! Loop indices.
REAL :: sum                  ! A temporary variable.

! Open the input file.
OPEN(1, FILE='matrix.txt')

! Read the matrix from the input file.
DO i = 1, N
  READ (1, *) (A(i,j), j = 1, N)
END DO

! Close the input file.
CLOSE(1)

! Open the output file.
OPEN(2, FILE='solution.txt')

! Solve the system of equations.
DO i = 1, N
  sum = 0.0
  DO j = 1, i-1
    sum = sum + A(i,j) * x(j)
  END DO
  x(i) = (b(i) - sum) / A(i,i)
END DO

! Write the solution to the output file.
DO i = 1, N
  WRITE (2, *) x(i)
END DO

! Close the output file.
CLOSE(2)

END PROGRAM TRIANGULAR_MATRIX
```

This code solves a system of linear equations with a triangular matrix. The matrix is input in a text file, and the solution is output to a text file.

The code first declares the variables. The variable `N` is the size of the matrix, `A` is the matrix, `b` is the right-hand side vector, `x` is the solution vector, and `i` and `j` are loop indices.

The code then opens the input file and reads the matrix from the input file. The code then closes the input file.

The code then opens the output file. The code then solves the system of equations. The code does this by looping over each row of the matrix. For each row, the code calculates the sum of the products of the elements of the row and the corresponding elements of the solution vector. The code then subtracts this sum from the right-hand side vector element and divides the result by the diagonal element of the matrix. This gives the solution for the current row.

The code then writes the solution to the output file. The code does this by looping over each element of the solution vector and writing it to the output file. The code then closes the output file.