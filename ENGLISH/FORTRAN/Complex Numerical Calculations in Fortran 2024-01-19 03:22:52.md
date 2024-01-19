```fortran
program Complex_Fortran_Code

implicit none

integer, parameter :: n = 1000
real(8), dimension(n) :: x, y, z

! Calculate the values of x, y, and z
do i = 1, n
    x(i) = sin(i / 100.0)
    y(i) = cos(i / 100.0)
    z(i) = x(i) + y(i)
end do

! Find the maximum and minimum values of z
zmax = maxval(z)
zmin = minval(z)

! Print the results
write(*,*) 'Maximum value of z:', zmax
write(*,*) 'Minimum value of z:', zmin

end program Complex_Fortran_Code
```

Here's a detailed explanation of the code:

1. **Program and Implicit None:**

   - `program Complex_Fortran_Code`: This is the main program.
   - `implicit none`: This statement specifies that no implicit typing is allowed. This means that all variables must be explicitly declared with their types.

2. **Parameter and Dimension Statements:**

   - `integer, parameter :: n = 1000`: This declares an integer parameter named `n` with a value of 1000. This parameter is used to define the size of the arrays `x`, `y`, and `z`.
   - `real(8), dimension(n) :: x, y, z`: This declares three real (double precision) arrays named `x`, `y`, and `z`, each with a size of `n`.

3. **Calculating the Values of x, y, and z:**

   - The `do` loop iterates from `i = 1` to `i = n` and calculates the values of `x`, `y`, and `z` for each value of `i`.
   - `x(i) = sin(i / 100.0)`: This calculates the sine of `i / 100.0` and stores the result in `x(i)`.
   - `y(i) = cos(i / 100.0)`: This calculates the cosine of `i / 100.0` and stores the result in `y(i)`.
   - `z(i) = x(i) + y(i)`: This calculates the sum of `x(i)` and `y(i)` and stores the result in `z(i)`.

4. **Finding the Maximum and Minimum Values of z:**

   - `zmax = maxval(z)`: This finds the maximum value in the array `z` and stores it in the variable `zmax`.
   - `zmin = minval(z)`: This finds the minimum value in the array `z` and stores it in the variable `zmin`.

5. **Printing the Results:**

   - `write(*,*) 'Maximum value of z:', zmax`: This prints the maximum value of `z` to the console with an appropriate message.
   - `write(*,*) 'Minimum value of z:', zmin`: This prints the minimum value of `z` to the console with an appropriate message.