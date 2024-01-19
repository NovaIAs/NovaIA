```
PROGRAM CALC_PI                                  ! Calculate Pi using the Gregory-Leibniz series.

IMPLICIT NONE

! Declare constants.
REAL, PARAMETER :: PI = 3.1415926535897932384626433832795

! Declare variables.
INTEGER :: n, sign                            ! Loop counter and sign flag.
REAL :: pi_approx, diff                        ! Pi approximation and difference from true value.

! Initialize variables.
n = 0
pi_approx = 0.0
sign = 1

! Iterate until the approximation is within 0.0001 of the true value.
DO
    n = n + 1
    pi_approx = pi_approx + REAL(sign) / (2 * n - 1)
    diff = ABS(pi_approx - PI)
    sign = -sign
LOOP WHILE (diff > 0.0001)

! Print the result.
PRINT *, 'Pi approximation:', pi_approx
PRINT *, 'Difference from true value:', diff

END PROGRAM CALC_PI
```

This code calculates the value of Pi using the Gregory-Leibniz series, which is given by:

```
Pi = 4 * (1 - 1/3 + 1/5 - 1/7 + 1/9 - ...)
```

The code starts by declaring the constants and variables that will be used in the calculation. The `IMPLICIT NONE` statement tells the compiler that all variables must be explicitly declared, which helps to catch errors early on.

The `DO` loop is used to iterate through the terms of the series. The loop counter `n` is incremented by 1 each iteration, and the sign flag `sign` is used to alternate between adding and subtracting the terms. The `REAL()` function is used to convert the integer value of `n` to a real number, which is necessary for the division operation.

The `pi_approx` variable is updated with each iteration of the loop, and the `diff` variable is used to store the difference between the current approximation and the true value of Pi. The loop continues until the difference is less than 0.0001.

Once the loop has finished, the `PRINT` statements are used to display the final approximation of Pi and the difference between the approximation and the true value.