```fortran
PROGRAM COMPLEX_AND_DIFFERENTIATED_CODE

implicit none

! Declare variables
integer, parameter :: N = 1000 ! Size of arrays
real, dimension(N) :: x, y, z ! Arrays for data

! Initialize arrays
do i = 1, N
  x(i) = i
  y(i) = 2*i
  z(i) = 3*i
end do

! Perform calculations
do i = 1, N
  z(i) = x(i) + y(i)
end do

! Print results
print *, "x =", x
print *, "y =", y
print *, "z =", z

END PROGRAM COMPLEX_AND_DIFFERENTIATED_CODE
```

Explanation:

1. `implicit none` statement: This statement requires all variables to be explicitly declared, which helps to prevent errors and makes the code more readable.

2. Declare variables: The `integer, parameter` statement declares an integer constant `N` with a value of 1000. The `real, dimension(N)` statement declares three arrays `x`, `y`, and `z` with `N` elements each.

3. Initialize arrays: The `do` loop iterates from 1 to `N` and initializes the arrays `x`, `y`, and `z` with values `i`, `2*i`, and `3*i`, respectively.

4. Perform calculations: Another `do` loop iterates from 1 to `N` and performs the calculation `z(i) = x(i) + y(i)` to add the corresponding elements of `x` and `y` and store the result in `z`.

5. Print results: The `print` statements are used to print the values of the arrays `x`, `y`, and `z`.

This code demonstrates a complex and differentiated program in Fortran, including explicit variable declarations, arrays, loops, and calculations. The code is well-commented and structured to make it easy to understand and modify.