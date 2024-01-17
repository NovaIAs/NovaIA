```fortran
program complex_fortran_code

implicit none

integer, parameter :: n = 1000000 ! Number of data points

real, dimension(n) :: x, y, z ! Arrays to store data

! Generate some random data
call random_number(x)
call random_number(y)
call random_number(z)

! Perform some complex calculations on the data
do i = 1, n
  z(i) = x(i) * y(i) + z(i)
end do

! Print the results
print *, 'x:', x
print *, 'y:', y
print *, 'z:', z

end program complex_fortran_code
```

This code generates three arrays of random numbers, then performs a complex calculation on each element of the arrays, and finally prints the results.

The `implicit none` statement tells the compiler that no implicit typing is allowed. This means that every variable must be explicitly declared with a type.

The `integer, parameter :: n = 1000000` statement declares an integer parameter named `n` with a value of 1000000. This parameter is used to specify the size of the arrays.

The `real, dimension(n) :: x, y, z` statement declares three real arrays named `x`, `y`, and `z`. Each array has a size of `n`.

The `call random_number(x)` statement calls a subroutine named `random_number` to generate random numbers and store them in the array `x`.

The `call random_number(y)` statement calls the `random_number` subroutine again to generate random numbers and store them in the array `y`.

The `call random_number(z)` statement calls the `random_number` subroutine a third time to generate random numbers and store them in the array `z`.

The `do i = 1, n` statement starts a loop that iterates from 1 to `n`.

The `z(i) = x(i) * y(i) + z(i)` statement performs a calculation on each element of the arrays `x`, `y`, and `z`. The result is stored in the array `z`.

The `end do` statement ends the loop.

The `print *, 'x:', x` statement prints the array `x` to the console.

The `print *, 'y:', y` statement prints the array `y` to the console.

The `print *, 'z:', z` statement prints the array `z` to the console.