```fortran
PROGRAM MAIN
  IMPLICIT NONE
  INTEGER, PARAMETER :: NMAX = 1000000
  REAL(8), DIMENSION(NMAX) :: A, B, C
  INTEGER :: I
  DO I = 1, NMAX
    A(I) = SIN(I)
    B(I) = COS(I)
    C(I) = A(I) + B(I)
  END DO

  OPEN(UNIT=10, FILE='output.txt')
  DO I = 1, NMAX
    WRITE(10, '(F20.16)') C(I)
  END DO
  CLOSE(UNIT=10)

END PROGRAM MAIN
```

Explanation:

1. The program is written in the FORTRAN programming language.
2. The program uses a module called `IMPLICIT NONE`, which tells the compiler that all variables must be explicitly declared. This makes the program more readable and easier to debug.
3. The program declares three arrays: `A`, `B`, and `C`, each of which can hold up to `NMAX` double-precision floating-point numbers. `NMAX` is a parameter that is set to `1000000`.
4. The program uses a `DO` loop to fill the arrays `A` and `B` with the sine and cosine of the numbers from `1` to `NMAX`.
5. The program then uses another `DO` loop to fill the array `C` with the sum of the corresponding elements in `A` and `B`.
6. The program then opens a file called `output.txt` and writes the contents of the array `C` to the file.
7. Finally, the program closes the file and exits.