```fortran
program example

implicit none

integer, parameter :: n = 10000

real, dimension(n) :: x, y, z

! Initialize the arrays
x = (/ (i, i=1,n) /)
y = (/ (2*i, i=1,n) /)
z = (/ (3*i, i=1,n) /)

! Calculate the sum of the arrays
sum_x = sum(x)
sum_y = sum(y)
sum_z = sum(z)

! Print the results
print *, 'Sum of x:', sum_x
print *, 'Sum of y:', sum_y
print *, 'Sum of z:', sum_z

end program example
```

This code is a simple example of a Fortran program that calculates the sum of three arrays. The program first initializes the arrays x, y, and z with the values 1, 2, and 3, respectively. Then, it calculates the sum of each array using the sum() function. Finally, it prints the results to the console.

Here is a more detailed explanation of the code:

* The `implicit none` statement tells the compiler that no implicit typing is allowed. This means that every variable must be explicitly declared with a type.
* The `integer, parameter :: n = 10000` statement declares an integer parameter named n with the value 10000. This parameter is used to determine the size of the arrays.
* The `real, dimension(n) :: x, y, z` statement declares three real arrays named x, y, and z with a size of n.
* The `x = (/ (i, i=1,n) /)` statement initializes the array x with the values 1, 2, ..., n. The `/` operator is used to create a list of values.
* The `y = (/ (2*i, i=1,n) /)` statement initializes the array y with the values 2, 4, ..., 2*n.
* The `z = (/ (3*i, i=1,n) /)` statement initializes the array z with the values 3, 6, ..., 3*n.
* The `sum_x = sum(x)` statement calculates the sum of the array x using the sum() function.
* The `sum_y = sum(y)` statement calculates the sum of the array y using the sum() function.
* The `sum_z = sum(z)` statement calculates the sum of the array z using the sum() function.
* The `print *, 'Sum of x:', sum_x` statement prints the sum of the array x to the console.
* The `print *, 'Sum of y:', sum_y` statement prints the sum of the array y to the console.
* The `print *, 'Sum of z:', sum_z` statement prints the sum of the array z to the console.

This code is a simple example of how to use Fortran to perform basic mathematical operations on arrays.