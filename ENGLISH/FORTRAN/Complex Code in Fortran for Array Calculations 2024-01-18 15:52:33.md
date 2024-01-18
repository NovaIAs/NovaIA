```fortran
program complex_code

implicit none

integer, parameter :: n = 1000000
real(kind=8) :: x(n), y(n), z(n)

! Initialize arrays
do i = 1, n
    x(i) = i
    y(i) = 2 * i
    z(i) = 3 * i
end do

! Perform some calculations
do i = 1, n
    x(i) = x(i) + y(i) * z(i)
end do

! Print the results
do i = 1, 10
    write(*, '(i6,f20.10)') i, x(i)
end do

end program complex_code
```

This code is a simple program that performs some calculations on three arrays of numbers. The arrays are initialized with values, and then a loop is used to calculate new values for each element in the arrays. The results are then printed to the console.

The code is complex because it is long and uses a number of different features of the Fortran language. For example, the code uses the `implicit none` statement to specify that no implicit typing is allowed. This means that every variable must be declared explicitly with a type. The code also uses the `parameter` statement to define a constant value for the size of the arrays.

The code also uses a number of different loops. The first loop is a `do` loop that is used to initialize the arrays. The second loop is a `do` loop that is used to calculate new values for each element in the arrays. The third loop is a `do` loop that is used to print the results to the console.

The code is also complex because it uses a number of different functions. The `write` statement is used to print the results to the console. The `+` operator is used to add two numbers together. The `*` operator is used to multiply two numbers together.

Overall, this code is a complex example of how to use the Fortran language to perform calculations on arrays of numbers.