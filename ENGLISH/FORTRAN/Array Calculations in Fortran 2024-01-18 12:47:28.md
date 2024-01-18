```fortran
program complex_code
implicit none

integer, parameter :: n = 1000000

real(kind=8), allocatable :: x(:)
real(kind=8), allocatable :: y(:)
real(kind=8), allocatable :: z(:)

allocate (x(n), y(n), z(n))

! Initialize arrays
x = 1.0
y = 2.0
z = 3.0

! Perform some calculations
do i = 1, n
  z(i) = x(i) * y(i)
end do

! Print the results
print *, "The first 10 elements of z are:"
print *, z(1:10)

! Free the allocated memory
deallocate (x, y, z)

end program complex_code
```

This code performs a simple calculation on a large array of numbers. The array size is specified by the parameter `n`, which is set to 1,000,000. The code allocates memory for the array and initializes it with values of 1.0, 2.0, and 3.0. It then performs a calculation on the array, multiplying each element of `x` by the corresponding element of `y` and storing the result in `z`. Finally, it prints the first 10 elements of `z` and deallocates the memory that was allocated for the arrays.

This code is complex because it involves the use of large arrays and loops. It also uses some advanced features of Fortran, such as the `allocatable` attribute and the `do` loop.

Here is a more detailed explanation of the code:

* The first line of the code declares the main program.
* The second line declares the `implicit none` statement. This statement tells the compiler to not allow any implicit typing of variables. All variables must be explicitly declared.
* The third line declares the `n` parameter. This parameter specifies the size of the arrays.
* The fourth line declares the `x`, `y`, and `z` arrays. The `allocatable` attribute tells the compiler that the arrays can be allocated at runtime.
* The fifth line allocates memory for the arrays.
* The sixth line initializes the `x` array to 1.0.
* The seventh line initializes the `y` array to 2.0.
* The eighth line initializes the `z` array to 3.0.
* The ninth line performs a calculation on the arrays. The `do` loop iterates over each element of the arrays and multiplies the corresponding elements of `x` and `y`. The result is stored in `z`.
* The tenth line prints the first 10 elements of `z`.
* The eleventh line deallocates the memory that was allocated for the arrays.
* The last line ends the main program.