Certainly! Here's a highly complex and differentiated code written in FORTRAN:

```fortran
PROGRAM ComplexCode
  IMPLICIT NONE

  INTEGER :: n, i, j
  REAL :: a, b, c, d, e, f, g, h, x, y, z
  REAL, DIMENSION(100) :: array

  WRITE(*,*) 'Enter the size of array (less than or equal to 100): '
  READ(*,*) n

  IF (n > 100) THEN
    STOP 'Array size exceeds the limit!'
  END IF

  WRITE(*,*) 'Enter ', n, ' elements of the array: '
  DO i = 1, n
    READ(*,*) array(i)
  END DO

  a = 0.0
  b = 0.0
  c = 0.0
  d = 0.0
  e = 0.0
  f = 0.0
  g = 0.0
  h = 0.0

  DO i = 1, n
    j = n - i + 1

    x = array(i)
    y = array(j)
    z = x + y

    a = a + x**2
    b = b + y**2
    c = c + x**3
    d = d + y**3
    e = e + EXP(x)
    f = f + LOG(y)
    g = g + SIN(x)
    h = h + COS(y)

    IF (z < 0.0) THEN
      array(i) = -z
    ELSE
      array(i) = z
    END IF
  END DO

  WRITE(*,*) 'Results:'
  WRITE(*,*) 'a = ', a
  WRITE(*,*) 'b = ', b
  WRITE(*,*) 'c = ', c
  WRITE(*,*) 'd = ', d
  WRITE(*,*) 'e = ', e
  WRITE(*,*) 'f = ', f
  WRITE(*,*) 'g = ', g
  WRITE(*,*) 'h = ', h

END PROGRAM ComplexCode
```

This code is a program written in FORTRAN that performs various calculations on an array of real numbers. Here's a brief explanation of the code:

1. The program starts by declaring the necessary variables, including `n` (size of the array) and various variables for calculations.

2. The user is prompted to enter the size of the array (limited to 100 elements) and the elements themselves.

3. The program then performs a series of calculations on the array elements, including calculating sums of squares, cubes, exponentials, logarithms, sines, and cosines.

4. Additionally, the program modifies the array by replacing negative sums with their absolute values.

5. Finally, the program outputs the results of the calculations.

Please note that this code is highly specific and unlikely to be repeated exactly due to the random inputs and calculations involved. Feel free to modify and adapt it according to your needs.