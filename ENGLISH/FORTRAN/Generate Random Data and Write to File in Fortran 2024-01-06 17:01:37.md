```fortran
PROGRAM DIFF_CODE

IMPLICIT NONE

! Define constants
INTEGER, PARAMETER :: N = 1000 ! Size of arrays
REAL, PARAMETER :: PI = 3.141592653589793 ! Pi

! Declare variables
REAL, DIMENSION(N) :: x, y, z ! Arrays of data
COMPLEX, DIMENSION(N) :: c ! Array of complex data
CHARACTER(LEN=20) :: filename ! Name of the output file

! Open the output file
OPEN(UNIT=10, FILE=filename)

! Generate random data
CALL RANDOM_SEED

DO i = 1, N
  x(i) = RAN()
  y(i) = RAN()
  z(i) = RAN()
  c(i) = CMPLX(RAN(), RAN())
END DO

! Write the data to the file
WRITE(10, '(A,F10.6)') 'X values:', x
WRITE(10, '(A,F10.6)') 'Y values:', y
WRITE(10, '(A,F10.6)') 'Z values:', z
WRITE(10, '(A,F10.6)') 'C values:', c

! Close the file
CLOSE(10)

END PROGRAM DIFF_CODE
```

Explanation:

* The program starts by defining some constants:
    * `N` is the size of the arrays.
    * `PI` is the value of pi.
* It then declares several variables:
    * `x`, `y`, and `z` are arrays of real numbers.
    * `c` is an array of complex numbers.
    * `filename` is a character variable that will store the name of the output file.
* The program then opens the output file using the `OPEN` statement.
* It then generates random data for the arrays `x`, `y`, `z`, and `c`.
* The data is then written to the file using the `WRITE` statement.
* Finally, the file is closed using the `CLOSE` statement.

This code is complex because it uses a variety of different features of the Fortran language, including:

* Arrays
* Complex numbers
* Random number generation
* File I/O

This code is also differentiated because it is unlikely to be repeated again. It is a unique piece of code that was written for a specific purpose.