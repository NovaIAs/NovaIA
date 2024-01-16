```fortran
PROGRAM PRIME_NUMBERS
! This program finds all the prime numbers up to a given limit.

IMPLICIT NONE

! Declare variables.

INTEGER :: LIMIT, I, J, PRIME

! Get the limit from the user.

WRITE (*,*) 'Enter the limit:'
READ (*,*) LIMIT

! Find all the prime numbers up to the limit.

DO I = 2, LIMIT
   PRIME = 1
   DO J = 2, SQRT(I)
      IF (MOD(I, J) == 0) THEN
         PRIME = 0
         EXIT
      END IF
   END DO
   IF (PRIME == 1) THEN
      WRITE (*,*) I
   END IF
END DO

END PROGRAM PRIME_NUMBERS
```

**Explanation:**

* The program starts by declaring the variables that it will use.
* The variable `LIMIT` is used to store the limit up to which the program will search for prime numbers.
* The variable `I` is used to iterate through the numbers from 2 to the limit.
* The variable `J` is used to iterate through the numbers from 2 to the square root of `I`.
* The variable `PRIME` is used to indicate whether or not `I` is a prime number.
* The program uses a nested `DO` loop to iterate through the numbers from 2 to the limit.
* The outer loop iterates through the numbers from 2 to the limit.
* The inner loop iterates through the numbers from 2 to the square root of `I`.
* If `I` is divisible by any number from 2 to its square root, then it is not a prime number.
* If `I` is not divisible by any number from 2 to its square root, then it is a prime number.
* The program prints out all the prime numbers that it finds.